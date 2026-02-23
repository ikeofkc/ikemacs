;;; ikemacs-sidebar.el --- Sidebar configuration -*- lexical-binding: t; -*-

(require 'nerd-icons)
(require 'recentf)
(require 'tab-bar)
(require 'ikemacs-collab)
(require 'org)
(require 'org-agenda)

(recentf-mode 1)

(setq tab-bar-tab-name-function
      (lambda ()
        (if (string= (buffer-name) ike/sidebar-buffer-name)
            (buffer-name (window-buffer (other-window 1)))
          (buffer-name))))

(defvar ike/sidebar-buffer-name "*Global Sidebar*")
(defvar ike/sidebar-width 25) ;; Width of default expanded bar (resizable)
(defvar ike/sidebar-icon-width 2) ;; Width of sidebar collapsed (not resizable after)
(defvar ike/sidebar-expanded t)
(defvar ike/sidebar-window nil)

;;; --- WINDOW TRACKING ---
(defvar ike/sidebar-last-window nil
  "Holds the last valid window the user interacted with.")

(defun ike/sidebar-record-window (&optional _frame)
  "Record the current window if it is a valid target for opening files."
  (let ((win (selected-window)))
    (when (and (window-live-p win)
               (not (window-minibuffer-p win))
               (not (string= (buffer-name (window-buffer win)) ike/sidebar-buffer-name))
               (not (window-dedicated-p win)))
      (setq ike/sidebar-last-window win))))

(add-hook 'window-selection-change-functions #'ike/sidebar-record-window)
;;; -----------------------

(defun ike/sidebar-search-tags-todo ()
  (interactive)
  (require 'org-ql)
  (let ((tag (read-string "Filter Active TODOs by Tag: ")))
    (if (and (boundp 'org-agenda-files) org-agenda-files)
        (progn
          (ike/sidebar--select-main-window)
          (org-ql-search org-agenda-files
            `(and (tags ,tag) (not (done)))
            :title (format "Active Tasks: #%s" tag)))
      (message "No Org agenda files found to search."))))

(defun ike/sidebar-search-org ()
  (interactive)
  (require 'org-ql)
  (if (and (boundp 'org-agenda-files) org-agenda-files)
      (let ((query (read-string "Search Notes (org-ql): ")))
        (ike/sidebar--select-main-window)
        (org-ql-search org-agenda-files query))
    (message "No Org agenda files found to search. Check your path!")))

(defun ike/sidebar--jump-to-marker (marker)
  (run-with-timer 0.05 nil
                  (lambda ()
                    (ike/sidebar--select-main-window)
                    (switch-to-buffer (marker-buffer marker))
                    (goto-char (marker-position marker))
                    (org-show-entry)
                    (recenter))))

(defun ike/sidebar--select-main-window ()
  (if (and ike/sidebar-last-window 
           (window-live-p ike/sidebar-last-window)
           (not (string= (buffer-name (window-buffer ike/sidebar-last-window)) ike/sidebar-buffer-name)))
      (select-window ike/sidebar-last-window)
    (let ((win (get-window-with-predicate 
                (lambda (w) 
                  (and (not (window-dedicated-p w))
                       (not (string= (buffer-name (window-buffer w)) ike/sidebar-buffer-name)))))))
      (if (and win (window-live-p win))
          (select-window win)
        (select-window (next-window))))))

(defun ike/sidebar--make-callback (fn)
  (lambda (&rest _)
    (interactive)
    (run-with-timer 0.05 nil
                    (lambda ()
                      (ike/sidebar--select-main-window)
                      (if (commandp fn)
                          (call-interactively fn)
                        (funcall fn))))))

(defun ike/sidebar--insert-button (label icon fn &optional help face-override)
  (let ((inhibit-read-only t)
        (callback (ike/sidebar--make-callback fn)))
    (let ((map (make-sparse-keymap)))
      (define-key map [down-mouse-1] #'ignore)
      (define-key map [mouse-1] callback)
      (define-key map [return] callback)
      (insert (propertize icon 'help-echo help 'mouse-face 'highlight 'keymap map)))
    (if ike/sidebar-expanded
        (progn
          (insert " ")
          (insert-text-button label 'action callback 'follow-link t 'help-echo help 'face (or face-override 'default))
          (insert "\n"))
      (insert "\n"))))

;; FIXED: We now pull JUST the hex color from the face so it doesn't clobber the icon font
(defun ike/sidebar--icon (name &optional face)
  (let ((color (face-foreground (or face 'default) nil t)))
    (nerd-icons-mdicon name :height 1.0 :v-adjust -0.1 :face `(:foreground ,color))))

(defun ike/sidebar--render ()
  (with-current-buffer (get-buffer-create ike/sidebar-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      
      ;; FIXED: Pulling colors from 'default and 'success faces
      (let* ((menu-icon (ike/sidebar--icon "nf-md-menu"))
             (refresh-icon (nerd-icons-mdicon "nf-md-refresh" :height 1.0 :v-adjust -0.1 
                                             :face `(:foreground ,(face-foreground 'default nil t))))
             (padded-icon (propertize menu-icon 'display '(raise -0.25))))
        
        (insert-text-button
         (if ike/sidebar-expanded (concat padded-icon " Collapse") padded-icon)
         'action (lambda (_) 
                   (when ike/sidebar-expanded
                     (setq ike/sidebar-width (window-width ike/sidebar-window)))
                   (setq ike/sidebar-expanded (not ike/sidebar-expanded)) 
                   (ike/sidebar--show))
         'follow-link t)
        
        (when ike/sidebar-expanded
          (insert "  ") 
          (insert-text-button
           (concat (propertize refresh-icon 'display '(raise -0.25)) " Refresh")
           'action (lambda (_) 
                     (message "Refreshing agenda files...")
                     (when (fboundp 'ike/refresh-agenda-files)
                       (ike/refresh-agenda-files)))
           'follow-link t
           'help-echo "Force re-scan of Org files")))
      
      (insert "\n\n")

      (ike/sidebar--insert-button "Open/Create File" (ike/sidebar--icon "nf-md-folder_open") 
                                  #'ike/gui-visit-file-dialog "Open file") 
      
      (ike/sidebar--insert-button "Search Notes" (ike/sidebar--icon "nf-md-magnify") 
                                  #'ike/sidebar-search-org "Search all notes") 
      
      (insert "\n")
      
      (ike/sidebar--insert-button "Agenda (Week)" (ike/sidebar--icon "nf-md-calendar") 
                                  (lambda () (interactive) (org-agenda-list nil nil 'week)) 
                                  "View current week")
      
      (when ike/sidebar-expanded
        (let ((sub-icon (nerd-icons-mdicon "nf-md-subdirectory_arrow_right" :height 0.8 
                                          :face `(:foreground ,(face-foreground 'shadow nil t)))))
          (insert "  " sub-icon " ")
          (insert-text-button "Day View" 
                              'action (ike/sidebar--make-callback (lambda () (interactive) (org-agenda-list nil nil 'day)))
                              'follow-link t 'face 'default)
          (insert "\n")
          (insert "  " sub-icon " ")
          (insert-text-button "Month View" 
                              'action (ike/sidebar--make-callback (lambda () (interactive) (org-agenda-list nil nil 'month)))
                              'follow-link t 'face 'default)
          (insert "\n")))

      (ike/sidebar--insert-button "Global TODOs" (ike/sidebar--icon "nf-md-format_list_bulleted") 
                                  #'org-todo-list "List all open tasks") 
      
      (ike/sidebar--insert-button "Filter by Tag" (ike/sidebar--icon "nf-md-label_outline") 
                                  #'ike/sidebar-search-tags-todo "Search active TODOs by Tag") 
      
      (when (and ike/sidebar-expanded (boundp 'ike/overdue-tasks-cache) ike/overdue-tasks-cache)
        (insert "\n") 
        (insert (propertize "Due Today / Overdue:\n" 'face 'bold))
        (dolist (task-data ike/overdue-tasks-cache)
          (let ((label (car task-data))
                (marker (cadr task-data)))
            (insert "• ")
            (insert-text-button label
                                'action (lambda (_) (ike/sidebar--jump-to-marker marker))
                                'follow-link t
                                'help-echo "Jump to task")
            (insert "\n"))))

      (insert "\n")
      
      ;; FIXED: Collaboration block uses extracted color hexes
      (let ((crdt-accent 'font-lock-keyword-face) 
            (crdt-warn 'error))
        (ike/sidebar--insert-button "Share Buffer" (ike/sidebar--icon "nf-md-access_point" crdt-accent) #'crdt-share-buffer "Start CRDT")
        (ike/sidebar--insert-button "Join Session" (ike/sidebar--icon "nf-md-link" crdt-accent) #'crdt-connect "Connect to URL") 
        (when (ike/collab-is-active-p)
          (ike/sidebar--insert-button "Copy Link" (ike/sidebar--icon "nf-md-content_copy" crdt-accent) #'ike/collab-copy-url-smart "Copy URL")
          (ike/sidebar--insert-button "Follow Teammate" (ike/sidebar--icon "nf-md-eye" crdt-accent) #'ike/collab-follow-wrapper "Follow user")
          (ike/sidebar--insert-button "Stop Following" (ike/sidebar--icon "nf-md-eye_off" crdt-accent) #'ike/collab-stop-follow "Stop follow")
          (ike/sidebar--insert-button "User Manager" (ike/sidebar--icon "nf-md-account_group" crdt-accent) #'ike/collab-list-users-wrapper "User List")
          (ike/sidebar--insert-button "Stop Session" (ike/sidebar--icon "nf-md-power" crdt-warn) #'ike/collab-stop "Stop Session")
          (when ike/sidebar-expanded (insert (propertize "  Active Sessions:\n" 'face 'bold))
            (dolist (b (ike/collab-get-active-buffers)) (insert-text-button (format "  • %s" (buffer-name b)) 'action (lambda (_) (run-with-timer 0.05 nil (lambda () (switch-to-buffer b)))) 'follow-link t) (insert "\n")))))
      
      (insert "\n")
      
      (ike/sidebar--insert-button "Recent Files" (ike/sidebar--icon "nf-md-history") #'recentf-open "Recent Files") 
      (recentf-load-list)
      (when ike/sidebar-expanded 
        (dolist (f (seq-take recentf-list 5)) 
          (insert-text-button (format "  %s" (file-name-nondirectory f)) 
                              'action (lambda (_) 
                                        (run-with-timer 0.05 nil 
                                                        (lambda () 
                                                          (ike/sidebar--select-main-window) 
                                                          (find-file f)))) 
                              'follow-link t) 
          (insert "\n")))
      
      (goto-char (point-min))
      (read-only-mode 1)
      (setq-local truncate-lines t)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      
      (if ike/sidebar-expanded
          (setq-local window-size-fixed nil)
        (setq-local window-size-fixed 'width)))))

(defun ike/sidebar--show ()
  "Show the sidebar. Fixed width when collapsed, resizable when expanded."
  (unless (minibuffer-window-active-p (minibuffer-window))
    (let* ((buf (get-buffer-create ike/sidebar-buffer-name))
           (target-width (if ike/sidebar-expanded ike/sidebar-width ike/sidebar-icon-width))
           (window-min-width 1)
           (window-safe-min-width 1))
      
      (unless (and ike/sidebar-window (window-live-p ike/sidebar-window))
        (setq ike/sidebar-window 
              (display-buffer-in-side-window buf 
               `((side . left)
                 (slot . 0)
                 (window-width . ,target-width) 
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))))
      
      (set-window-dedicated-p ike/sidebar-window t)
      (set-window-parameter ike/sidebar-window 'min-width 1)
      
      (with-current-buffer buf
        (setq-local window-size-fixed nil))
      
      (let ((delta (- target-width (window-width ike/sidebar-window))))
        (when (/= delta 0)
          (ignore-errors
            (window-resize ike/sidebar-window delta t))))
            
      (window-preserve-size ike/sidebar-window t t) 
      
      (with-selected-window ike/sidebar-window 
        (ike/sidebar--render)))))

(defun ike/sidebar-update ()
  (interactive) (unless (minibuffer-window-active-p (minibuffer-window)) (ike/sidebar--render)))

(defun ike/sidebar--ensure-visible ()
  (unless (minibuffer-window-active-p (minibuffer-window)) (unless (get-buffer-window ike/sidebar-buffer-name) (ike/sidebar--show))))

(add-hook 'window-configuration-change-hook #'ike/sidebar--ensure-visible)
(setq ike/sidebar-expanded nil)
(add-hook 'tab-bar-tab-post-select-functions (lambda (&rest _) (ike/sidebar--show)))
(add-hook 'tab-bar-tab-post-open-functions (lambda (&rest _) (ike/sidebar--show)))
(add-hook 'emacs-startup-hook #'ike/sidebar--show)
(add-hook 'crdt-mode-hook #'ike/sidebar-update)

(provide 'ikemacs-sidebar)
