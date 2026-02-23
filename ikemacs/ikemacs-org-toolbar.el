;;; ikemacs-org-toolbar.el --- Horizontal Org-mode Toolbar -*- lexical-binding: t; -*-

(require 'nerd-icons)
(require 'org)

(defvar ike/org-toolbar-buffer-name " *Org Toolbar*")
(defvar ike/org-toolbar-window nil)

(defun ike/org-toolbar--select-main-window ()
  "Focus the main window so toolbar actions apply to the correct buffer."
  (let ((win (get-window-with-predicate 
              (lambda (w) 
                (and (not (window-dedicated-p w))
                     (not (window-minibuffer-p w)))))))
    (if (and win (window-live-p win))
        (select-window win)
      (select-window (next-window)))))

(defun ike/org-toolbar--make-callback (fn)
  "Creates a callback that focuses the main window before running FN."
  (lambda (&rest _)
    (interactive)
    (ike/org-toolbar--select-main-window)
    (if (commandp fn)
        (call-interactively fn)
      (funcall fn))))

(defun ike/org-toolbar--insert-button (icon-name tooltip fn &optional color)
  "Insert a clickable icon into the toolbar."
  (let* ((inhibit-read-only t)
         (callback (ike/org-toolbar--make-callback fn))
         (icon (nerd-icons-mdicon icon-name :height 1.2 :v-adjust 0.0 :face `(:foreground ,(or color "#f8f8f2"))))
         (map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] callback)
    (define-key map [return] callback)
    
    (insert "  ") ;; Left padding for each icon
    (insert (propertize icon 
                        'help-echo tooltip 
                        'mouse-face 'highlight 
                        'keymap map))
    (insert "  "))) ;; Right padding

;; Custom horizontal scroll wrappers
(defun ike/org-toolbar--scroll-right ()
  (interactive) (scroll-left 5 t))
(defun ike/org-toolbar--scroll-left ()
  (interactive) (scroll-right 5 t))

(defun ike/org-toolbar--render ()
  "Draw the icons into the toolbar buffer."
  (with-current-buffer (get-buffer-create ike/org-toolbar-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      
      ;; The Custom Transient Button
      (ike/org-toolbar--insert-button "nf-md-format_color_text" "Emphasis Menu" 
                                      #'ike/org-emphasis-menu 
                                      "#ff79c6")
      
      ;; Structure Icons
      (insert (propertize "  |  " 'face '(:foreground "#44475a")))
      (ike/org-toolbar--insert-button "nf-md-link" "Insert Link" #'org-insert-link "#8be9fd")
      (ike/org-toolbar--insert-button "nf-md-table" "Insert Table" #'org-table-create-or-convert-from-region "#bd93f9")
      (ike/org-toolbar--insert-button "nf-md-format_list_checkbox" "Toggle Checkbox" #'org-toggle-checkbox "#50fa7b")
      
      ;; Metadata Icons
      (insert (propertize "  |  " 'face '(:foreground "#44475a")))
      (ike/org-toolbar--insert-button "nf-md-check_circle_outline" "TODO State" #'org-todo "#ff5555")
      (ike/org-toolbar--insert-button "nf-md-tag_outline" "Set Tags" #'org-set-tags-command "#f1fa8c")
      (ike/org-toolbar--insert-button "nf-md-calendar_clock" "Schedule/Deadline" #'org-schedule "#bd93f9")
      (ike/org-toolbar--insert-button "nf-md-flag" "Priority" #'org-priority "#ffb86c")

      (goto-char (point-min))
      (read-only-mode 1)
      (setq-local truncate-lines t)
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil) 
      (setq-local window-size-fixed 'height)
      
      ;; Hijack vertical mouse wheel inputs and convert them to horizontal scrolling
      (local-set-key (kbd "<wheel-up>") #'ike/org-toolbar--scroll-left)
      (local-set-key (kbd "<wheel-down>") #'ike/org-toolbar--scroll-right)
      (local-set-key (kbd "<mouse-4>") #'ike/org-toolbar--scroll-left)   ;; Linux X11 Support
      (local-set-key (kbd "<mouse-5>") #'ike/org-toolbar--scroll-right)))) ;; <--- FIXED PARENS

(defun ike/org-toolbar-show ()
  "Show the toolbar directly below the currently selected window."
  (interactive)
  (let ((buf (get-buffer-create ike/org-toolbar-buffer-name)))
    (unless (and ike/org-toolbar-window (window-live-p ike/org-toolbar-window))
      (setq ike/org-toolbar-window
            (display-buffer buf
             '((display-buffer-below-selected) ;; <--- Attaches specifically to active window
               (window-height . 2) 
               (inhibit-same-window . t)))))
    
    (set-window-dedicated-p ike/org-toolbar-window t)
    (set-window-parameter ike/org-toolbar-window 'no-other-window t)
    (set-window-parameter ike/org-toolbar-window 'no-delete-other-windows t)
    
    (with-selected-window ike/org-toolbar-window
      (ike/org-toolbar--render))))

(defun ike/org-toolbar-hide ()
  "Hide the toolbar."
  (interactive)
  (when (and ike/org-toolbar-window (window-live-p ike/org-toolbar-window))
    (delete-window ike/org-toolbar-window)
    (setq ike/org-toolbar-window nil)))

(defun ike/org-toolbar-toggle ()
  "Toggle the visibility of the Org toolbar."
  (interactive)
  (if (and ike/org-toolbar-window (window-live-p ike/org-toolbar-window))
      (ike/org-toolbar-hide)
    (ike/org-toolbar-show)))

(defun ike/org-toolbar-smart-toggle (&optional window)
  "Show toolbar for Org files, hide for everything else."
  (let* ((win (or window (selected-window)))
         (buf (window-buffer win)))
    ;; Ignore clicks inside the toolbar itself or the command input
    (unless (or (string= (buffer-name buf) ike/org-toolbar-buffer-name)
                (window-minibuffer-p win))
      ;; Check if it is Org-mode AND a physical file on your hard drive
      (if (and (eq (buffer-local-value 'major-mode buf) 'org-mode)
               (buffer-file-name buf)) 
          (ike/org-toolbar-show)
        (ike/org-toolbar-hide)))))

;; Watch for window/tab changes globally
(add-hook 'window-selection-change-functions #'ike/org-toolbar-smart-toggle)

(provide 'ikemacs-org-toolbar)
