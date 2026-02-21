;;; crdt-config.el --- CRDT collaboration configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for crdt.el collaborative editing with helper functions

;;; Code:

(use-package crdt
  :config
  (setq crdt-default-name (user-full-name)
        crdt-ask-for-name nil))

;; Helper functions for CRDT operations

(defun my-crdt-copy-share-link ()
  "Copy CRDT share link to clipboard."
  (interactive)
  (if (and (boundp 'crdt--session) crdt--session)
      (let* ((session crdt--session)
             (port (process-contact (crdt--session-network-process session) :service))
             (url (format "crdt://%s:%s" (system-name) port)))
        (kill-new url)
        (message "Share link copied: %s" url))
    (message "No active CRDT session")))

(defun my-crdt-show-users ()
  "Show connected CRDT users in a popup."
  (interactive)
  (if (and (boundp 'crdt--session) crdt--session)
      (let ((users (crdt--session-name-table crdt--session))
            (buf (get-buffer-create "*CRDT Users*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "Connected Users\n" 'face '(:weight bold :height 1.2)))
            (insert (propertize "═════════════════\n\n" 'face 'font-lock-comment-face))

            (if users
                (maphash
                 (lambda (id name)
                   (insert (propertize "👤 " 'face '(:foreground "cyan")))
                   (insert (propertize (format "%s" name) 'face '(:weight bold)))
                   (insert (propertize (format " (ID: %s)" id) 'face 'font-lock-comment-face))
                   (insert "\n")

                   ;; Add follow button
                   (let ((start (point)))
                     (insert "   ")
                     (insert (propertize "[Follow]" 'face 'link))
                     (insert "\n\n")
                     (make-button start (- (point) 2)
                                  'action `(lambda (_) (my-crdt-follow-user ,id))
                                  'follow-link t
                                  'help-echo "Follow this user's cursor")))
                 users)
              (insert "No users connected.\n"))

            (setq buffer-read-only t)
            (goto-char (point-min)))
          (display-buffer buf)))
    (message "No active CRDT session")))

(defun my-crdt-follow-user (user-id)
  "Follow USER-ID's cursor in CRDT session."
  (interactive)
  (if (and (boundp 'crdt--session) crdt--session)
      (progn
        (crdt-goto-user user-id)
        (message "Following user %s" user-id))
    (message "No active CRDT session")))

(defun my-crdt-quick-share ()
  "Quickly share current buffer via CRDT and copy link."
  (interactive)
  (call-interactively 'crdt-share-buffer)
  (sit-for 0.5)  ; Wait for session to initialize
  (my-crdt-copy-share-link))

(defun my-crdt-connect-with-url ()
  "Connect to CRDT session using a URL."
  (interactive)
  (let ((url (read-string "Enter CRDT URL (crdt://host:port): ")))
    (when (string-match "crdt://\\([^:]+\\):\\([0-9]+\\)" url)
      (let ((host (match-string 1 url))
            (port (string-to-number (match-string 2 url))))
        (crdt-connect host port)))))

(defun my-crdt-session-info ()
  "Display current CRDT session information."
  (interactive)
  (if (and (boundp 'crdt--session) crdt--session)
      (let* ((session crdt--session)
             (port (process-contact (crdt--session-network-process session) :service))
             (url (format "crdt://%s:%s" (system-name) port))
             (user-count (hash-table-count (crdt--session-name-table session))))
        (message "CRDT Session Active | URL: %s | Users: %d" url user-count))
    (message "No active CRDT session")))

;; Keybindings for CRDT
(with-eval-after-load 'crdt
  (define-key crdt-mode-map (kbd "C-c c s") 'my-crdt-quick-share)
  (define-key crdt-mode-map (kbd "C-c c c") 'crdt-connect)
  (define-key crdt-mode-map (kbd "C-c c u") 'my-crdt-connect-with-url)
  (define-key crdt-mode-map (kbd "C-c c l") 'my-crdt-copy-share-link)
  (define-key crdt-mode-map (kbd "C-c c p") 'my-crdt-show-users)
  (define-key crdt-mode-map (kbd "C-c c i") 'my-crdt-session-info)
  (define-key crdt-mode-map (kbd "C-c c q") 'crdt-stop-session))

;; Mode line indicator
(defun my-crdt-mode-line ()
  "Return mode line indicator for CRDT status."
  (when (and (boundp 'crdt--session) crdt--session)
    (let ((user-count (hash-table-count (crdt--session-name-table crdt--session))))
      (propertize (format " 🔗%d" user-count)
                  'face '(:foreground "cyan")
                  'help-echo "CRDT session active - click for info"
                  'mouse-face 'mode-line-highlight
                  'local-map (make-mode-line-mouse-map
                             'mouse-1 'my-crdt-session-info)))))

(add-to-list 'mode-line-misc-info '(:eval (my-crdt-mode-line)))

;; Auto-refresh sidebar when CRDT status changes
(add-hook 'crdt-mode-hook
          (lambda ()
            (when (fboundp 'my-sidebar-refresh)
              (my-sidebar-refresh))))

(provide 'crdt-config)
;;; crdt-config.el ends here
