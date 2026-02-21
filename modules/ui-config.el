;;; ui-config.el --- UI and theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; User interface configuration including theme and fonts

;;; Code:

;; Modern theme
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; All the icons for sidebar
(use-package all-the-icons
  :if (display-graphic-p)
  :demand t)

;; Enhanced modeline - simple and clean
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                (:propertize " %b " face mode-line-buffer-id)
                "  "
                (:propertize "%l:%c" face font-lock-type-face)
                "  "
                (:propertize "%p" face font-lock-constant-face)
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Which-key for keybinding discovery
(use-package which-key
  :demand t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;; Vertico for better minibuffer completion
(use-package vertico
  :demand t
  :init (vertico-mode))

;; Marginalia for richer annotations
(use-package marginalia
  :demand t
  :init (marginalia-mode))

;; Orderless for flexible matching
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Font configuration
(when (display-graphic-p)
  (condition-case nil
      (progn
        (set-face-attribute 'default nil :font "Fira Code" :height 120)
        (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
        (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130))
    (error
     ;; Fallback to monospace if Fira Code not available
     (set-face-attribute 'default nil :font "Monospace" :height 120))))

(provide 'ui-config)
;;; ui-config.el ends here
