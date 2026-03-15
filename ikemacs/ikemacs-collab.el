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

(defun ike/collab--detect-local-ip ()
  "Detect the local LAN IP address cross-platform.
Returns the first non-loopback IPv4 address, or \"localhost\" as fallback."
  (let ((ip-data
         (cond
          ((eq system-type 'windows-nt)
           (shell-command-to-string "ipconfig"))
          ((eq system-type 'darwin)
           (shell-command-to-string "ifconfig 2>/dev/null || ip addr 2>/dev/null"))
          (t ;; Linux and others
           (shell-command-to-string "ip addr 2>/dev/null || ifconfig 2>/dev/null")))))
    (cond
     ;; Windows: "IPv4 Address... : 192.168.x.x"
     ((string-match "IPv4 Address[^:]+: \\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" ip-data)
      (match-string 1 ip-data))
     ;; Linux/macOS: "inet 192.168.x.x" (skip 127.0.0.1)
     ((string-match "inet \\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" 
                     (replace-regexp-in-string "inet 127\\.[0-9.]+" "" ip-data))
      (match-string 1 (replace-regexp-in-string "inet 127\\.[0-9.]+" "" ip-data)))
     (t "localhost"))))

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
                     (host (ike/collab--detect-local-ip))
                     (url-string (format "%s:%s" host port)))
                (kill-new url-string)
                (message "Shared URL copied to clipboard: %s" url-string))
            (message "Session active but data missing.")))
      (message "No active CRDT session found."))))

(provide 'ikemacs-collab)