(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
   (ivy-mode))

(setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)

;; --- NERD ICONS MIGRATION ---
(use-package nerd-icons
  :ensure t
  :config
  ;; Check for the standard Nerd Font family
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (message "Ikemacs: Nerd Fonts missing. Run 'M-x nerd-icons-install-fonts' to fix.")))

(use-package nerd-icons-ivy-rich
  :ensure t
  :init (nerd-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init (ivy-rich-mode 1))

;; Add better search and bind to Ctrl-f
(use-package swiper
  :after ivy
  :ensure t 
  :bind (("C-f" . swiper)))

;; Make result sorting much more sensible
(use-package ivy-prescient
  :ensure t
  :init (ivy-prescient-mode 1))

(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;;---------------------------------
;;    Company - Completion
;;---------------------------------
(use-package company
  :ensure t
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2))
  :config
  (progn
    (global-company-mode)))

(provide 'ikemacs-completion)