;;; sidebar.el --- Expandable sidebar with icons -*- lexical-binding: t; -*-

;;; Commentary:
;; Sidebar with icons and expandable sections for recent files, CRDT, and more
;; Works without treemacs dependency

;;; Code:

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 100)

;; Custom sidebar buffer
(defvar my-sidebar-buffer-name "*Sidebar*"
  "Name of the sidebar buffer.")

(defvar my-sidebar-width 50
  "Width of expanded sidebar.")

(defvar my-sidebar-narrow-width 4
  "Width of narrow (icon-only) sidebar.")

(defvar my-sidebar-expanded nil
  "Whether sidebar is currently expanded.")

(defvar my-sidebar-window nil
  "Window object for the sidebar.")

(defface my-sidebar-button-face
  '((t :inherit default :box (:line-width 1 :color "gray50") :background "#3a3a3a"))
  "Face for sidebar buttons.")

(defface my-sidebar-header-face
  '((t :inherit default :weight bold :height 1.2 :foreground "#51afef"))
  "Face for sidebar headers.")

(defun my-sidebar-create-buffer ()
  "Create and populate the sidebar buffer."
  (with-current-buffer (get-buffer-create my-sidebar-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)

      ;; Make buffer interactive
      (setq cursor-type nil)

      ;; Insert header with expand/collapse button
      (my-sidebar-insert-clickable-header)
      (insert "\n")

      ;; Recent Files Section
      (my-sidebar-insert-section
       "📁" "Recent Files"
       (lambda ()
         (let ((files (seq-take recentf-list 10)))
           (if files
               (dolist (file files)
                 (my-sidebar-insert-file-item file))
             (insert (propertize "    No recent files\n" 'face 'font-lock-comment-face))))))

      (insert "\n")

      ;; Org Agenda Section
      (my-sidebar-insert-section
       "📅" "Org Agenda"
       (lambda ()
         (my-sidebar-insert-clickable-item "Agenda" 'org-agenda "Open Org Agenda")
         (my-sidebar-insert-clickable-item "Capture" 'org-capture "Quick capture")))

      (insert "\n")

      ;; CRDT Section
      (my-sidebar-insert-section
       "🔗" "CRDT Collaboration"
       'my-sidebar-crdt-content)

      (insert "\n")

      ;; Buffers Section
      (my-sidebar-insert-section
       "📋" "Buffers"
       (lambda ()
         (my-sidebar-insert-clickable-item "Switch Buffer" 'switch-to-buffer "Switch to another buffer")
         (my-sidebar-insert-clickable-item "Kill Buffer" 'kill-buffer "Close current buffer")))

      ;; Settings at bottom
      (goto-char (point-max))
      (insert "\n")
      (my-sidebar-insert-clickable-item "⚙️ Settings" 'my-sidebar-settings "Sidebar settings")

      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun my-sidebar-insert-clickable-header ()
  "Insert clickable header for expand/collapse."
  (let ((start (point))
        (text (if my-sidebar-expanded "  SIDEBAR" "≡")))
    (insert (propertize text 'face 'my-sidebar-header-face))
    (insert "\n")
    (make-text-button start (1- (point))
                     'action (lambda (_) (my-sidebar-toggle-expand))
                     'mouse-face 'highlight
                     'help-echo "Click to expand/collapse sidebar"
                     'follow-link t)))

(defun my-sidebar-insert-section (icon title content-fn)
  "Insert a sidebar section with ICON, TITLE, and CONTENT-FN."
  (let ((start (point)))
    (if my-sidebar-expanded
        (insert (propertize (format "%s  %s\n" icon title)
                           'face '(:weight bold :foreground "#98be65")))
      (progn
        (insert (propertize icon 'face '(:weight bold)))
        (insert "\n")
        (make-text-button start (1- (point))
                         'action (lambda (_) (my-sidebar-toggle-expand))
                         'mouse-face 'highlight
                         'help-echo (format "Click to view %s" title)
                         'follow-link t)))

    (when my-sidebar-expanded
      (funcall content-fn))))

(defun my-sidebar-insert-clickable-item (label action help-text)
  "Insert a clickable item with LABEL, ACTION, and HELP-TEXT."
  (when my-sidebar-expanded
    (let ((start (point)))
      (insert "  ")
      (insert (propertize label 'face 'link))
      (insert "\n")
      (make-text-button start (1- (point))
                       'action (lambda (_)
                                (interactive)
                                (with-selected-window (next-window)
                                  (call-interactively action)))
                       'mouse-face 'highlight
                       'help-echo help-text
                       'follow-link t))))

(defun my-sidebar-insert-file-item (file)
  "Insert a clickable file item for FILE."
  (when my-sidebar-expanded
    (let ((start (point))
          (display-name (file-name-nondirectory file)))
      (insert "  ")
      (insert (propertize display-name 'face 'link))
      (insert "\n")
      (make-text-button start (1- (point))
                       'action `(lambda (_)
                                 (with-selected-window (next-window)
                                   (find-file ,file)))
                       'mouse-face 'highlight
                       'help-echo file
                       'follow-link t))))

(defun my-sidebar-crdt-content ()
  "Generate CRDT section content."
  (if (and (boundp 'crdt--session) crdt--session)
      (progn
        (my-sidebar-insert-clickable-item "📋 Copy Share Link" 'my-crdt-copy-share-link "Copy collaboration link")
        (my-sidebar-insert-clickable-item "👁️  View/Follow Users" 'my-crdt-show-users "See connected users")
        (my-sidebar-insert-clickable-item "🚪 Disconnect" 'crdt-stop-session "Stop collaboration session"))
    (my-sidebar-insert-clickable-item "➕ Create Session" 'crdt-share-buffer "Start new collaboration")
    (my-sidebar-insert-clickable-item "🔌 Connect to Session" 'crdt-connect "Join existing session")))

(defun my-sidebar-toggle ()
  "Toggle sidebar visibility."
  (interactive)
  (if (and my-sidebar-window (window-live-p my-sidebar-window))
      (progn
        (delete-window my-sidebar-window)
        (setq my-sidebar-window nil))
    (my-sidebar-show)))

(defun my-sidebar-show ()
  "Show the sidebar."
  (interactive)
  (let ((buf (get-buffer-create my-sidebar-buffer-name)))
    (my-sidebar-create-buffer)
    (setq my-sidebar-window
          (display-buffer-in-side-window
           buf
           `((side . left)
             (slot . 0)
             (window-width . ,(if my-sidebar-expanded
                                 my-sidebar-width
                               my-sidebar-narrow-width))
             (preserve-size . (t . nil)))))
    (with-selected-window my-sidebar-window
      (setq mode-line-format nil
            header-line-format nil
            cursor-type nil)
      ;; Enable mouse interaction
      (setq buffer-read-only t))))

(defun my-sidebar-toggle-expand ()
  "Toggle sidebar between narrow and expanded state."
  (interactive)
  (setq my-sidebar-expanded (not my-sidebar-expanded))
  (when (and my-sidebar-window (window-live-p my-sidebar-window))
    (with-selected-window my-sidebar-window
      (window-resize my-sidebar-window
                     (- (if my-sidebar-expanded
                           my-sidebar-width
                         my-sidebar-narrow-width)
                        (window-width))
                     t))
    (my-sidebar-create-buffer)))

(defun my-sidebar-refresh ()
  "Refresh sidebar content."
  (interactive)
  (when (get-buffer my-sidebar-buffer-name)
    (my-sidebar-create-buffer)))

(defun my-sidebar-settings ()
  "Open sidebar settings."
  (interactive)
  (message "Sidebar settings - customize in sidebar.el"))

;; Mouse-specific keybindings for sidebar
(defvar my-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "RET") 'push-button)
    (define-key map (kbd "q") 'my-sidebar-toggle)
    (define-key map (kbd "e") 'my-sidebar-toggle-expand)
    (define-key map (kbd "r") 'my-sidebar-refresh)
    map)
  "Keymap for sidebar buffer.")

;; Apply keymap to sidebar buffer
(add-hook 'temp-buffer-setup-hook
          (lambda ()
            (when (string= (buffer-name) my-sidebar-buffer-name)
              (use-local-map my-sidebar-mode-map))))

;; Global keybindings
(global-set-key (kbd "C-c s t") 'my-sidebar-toggle)
(global-set-key (kbd "C-c s e") 'my-sidebar-toggle-expand)
(global-set-key (kbd "C-c s r") 'my-sidebar-refresh)
(global-set-key (kbd "<f8>") 'my-sidebar-toggle)

;; Auto-refresh sidebar on certain events
(add-hook 'buffer-list-update-hook 'my-sidebar-refresh)

;; Show sidebar on startup
(add-hook 'emacs-startup-hook 'my-sidebar-show)

(provide 'sidebar)
;;; sidebar.el ends here
