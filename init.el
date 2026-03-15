;;; init.el --- Emacs Initialization -*- lexical-binding: t; -*-

;; --- PERFORMANCE: GC TUNING ---
(setq gc-cons-threshold (* 50 1000 1000)) ;; 50MB during startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Startup complete. Restoring GC values.")
            (setq gc-cons-threshold (* 2 1000 1000)))) ;; 2MB default

;; --- SAFETY: Restore frame visibility (set to nil in early-init.el) ---
(add-hook 'emacs-startup-hook
          (lambda ()
            (modify-frame-parameters nil '((visibility . t)))))

;; Add the 'ikemacs' folder to the load path
(add-to-list 'load-path (expand-file-name "ikemacs" user-emacs-directory))

;; =================================================================
;; Load Modules
;; =================================================================

;; 1. Core Package Management (Must be first)
(require 'ikemacs-packages)

;; 2. Adds functions to force dialog boxes for open/save (used by sidebar and keys)
(require 'ikemacs-dialogs)

;; 3. UI and Appearance
(require 'ikemacs-ui)

;; 4. Autocomplete (Ivy/Company)
(require 'ikemacs-completion)

;; 5. Programming/Code
(require 'ikemacs-code)

;; 6. Collaboration (New)
(require 'ikemacs-collab)

;; 7. Org Mode
(require 'ikemacs-org)
(require 'org-gadget)
(org-gadget-mode 1)

;; 8. Sidebar (needs ikemacs-dialogs)
(require 'ikemacs-sidebar)

;; 9. Global Keybindings (Needs ikemacs-dialogs) Must be last to override others)
(require 'ikemacs-keys)

;; 10. Top Toolbar (needs ikemacs-keys for ike/smart-close)
(require 'ikemacs-topbar)
(ike/topbar-mode 1)

;; Welcome screen and new tab page
(require 'ikemacs-welcome)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
