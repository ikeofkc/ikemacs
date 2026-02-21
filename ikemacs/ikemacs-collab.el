;;; ikemacs-collab.el --- CRDT Collaboration Tools -*- lexical-binding: t; -*-

(require 'ikemacs-packages)

(use-package crdt
  :ensure t
  :config
  (setq crdt-default-name (or (user-login-name) "Guest"))
  
  ;; --- WINDOWS FIX: Robust Nonce Generation ---
  (defun crdt--generate-nonce (&optional length)
    (let* ((len (or length 32))
           (nonce (condition-case nil
                      (if (fboundp 'gnutls-strong-random)
                          (gnutls-strong-random len)
                        nil)
                    (error nil))))
      (or nonce
          (let ((str (make-string len 0)))
            (dotimes (i len)
              (aset str i (random 256)))
            str)))))

;; --- HELPERS ---

(defun ike/collab-get-active-buffers ()
  (seq-filter (lambda (b)
                (with-current-buffer b
                  (bound-and-true-p crdt-mode)))
              (buffer-list)))

(defun ike/collab-is-active-p ()
  (not (null (ike/collab-get-active-buffers))))

;; --- SMART WRAPPERS ---

(defun ike/collab-follow-wrapper ()
  "Switches to a shared buffer and runs the native follow command."
  (interactive)
  (let ((shared-buffer (car (ike/collab-get-active-buffers))))
    (if shared-buffer
        (progn
          (switch-to-buffer shared-buffer)
          (call-interactively #'crdt-follow-user))
      (message "No active CRDT session found."))))

(defun ike/collab-stop-follow ()
  "Global command to stop following anyone in the current session."
  (interactive)
  (let ((shared-buffer (car (ike/collab-get-active-buffers))))
    (if shared-buffer
        (with-current-buffer shared-buffer
          ;; FIX: Changed from crdt-stop-follow-user to crdt-stop-follow
          (crdt-stop-follow)
          (message "Stopped following teammates."))
      (message "No active CRDT session."))))

(defun ike/collab-list-users-wrapper ()
  (interactive)
  (let ((shared-buffer (car (ike/collab-get-active-buffers))))
    (if shared-buffer
        (progn
          (switch-to-buffer shared-buffer)
          (call-interactively #'crdt-list-users))
      (message "No active CRDT session found."))))

(defun ike/collab-stop ()
  (interactive)
  (let ((buffer (car (ike/collab-get-active-buffers))))
    (if buffer
        (with-current-buffer buffer
          (cond
           ((and (boundp 'crdt--session) crdt--session (crdt--server-p))
            (call-interactively #'crdt-stop-session))
           ((and (boundp 'crdt--session) crdt--session)
            (call-interactively #'crdt-disconnect))
           (t (message "Session state inconsistent."))))
      (message "No active session."))))

(defun ike/collab-copy-url-smart ()
  "Manually constructs the session URL as HOST:PORT only."
  (interactive)
  (let ((buffer (car (ike/collab-get-active-buffers))))
    (if buffer
        (with-current-buffer buffer
          (if (and (boundp 'crdt--session) crdt--session)
              (let* ((process (crdt--session-network-process crdt--session))
                     (port (if process (process-contact process :service) 6530))
                     (ip-data (shell-command-to-string "ipconfig"))
                     (host (if (string-match "IPv4 Address[^:]+: \\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" ip-data)
                               (match-string 1 ip-data)
                             "localhost"))
                     (url-string (format "%s:%s" host port)))
                (kill-new url-string)
                (message "Shared URL copied to clipboard: %s" url-string))
            (message "Session active but data missing.")))
      (message "No active CRDT session found."))))

(provide 'ikemacs-collab)