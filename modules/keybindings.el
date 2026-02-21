;;; keybindings.el --- WYSIWYG-style keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Intuitive keybindings familiar to users of web markup and WYSIWYG editors

;;; Code:

(use-package general
  :config
  (general-evil-setup t))

;; Global keybindings (Common editor shortcuts)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'kill-current-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-n") 'make-frame-command)

;; Org-mode specific keybindings
(defun my-org-keybindings ()
  "Set up WYSIWYG-style keybindings for org-mode."
  (local-set-key (kbd "C-b") 'org-emphasize) ;; Will prompt for type
  (local-set-key (kbd "C-i") 'my-org-toggle-italic)
  (local-set-key (kbd "C-S-b") 'my-org-toggle-bold)
  (local-set-key (kbd "C-S-u") 'my-org-toggle-underline)
  (local-set-key (kbd "C-S-s") 'my-org-toggle-strike)
  (local-set-key (kbd "C-S-c") 'my-org-toggle-code)
  (local-set-key (kbd "M-RET") 'org-insert-heading-respect-content)
  (local-set-key (kbd "C-k") 'my-org-insert-link)
  (local-set-key (kbd "C-l") 'org-insert-link)
  (local-set-key (kbd "C-<return>") 'org-insert-heading-after-current)
  (local-set-key (kbd "M-<up>") 'org-move-subtree-up)
  (local-set-key (kbd "M-<down>") 'org-move-subtree-down)
  (local-set-key (kbd "M-<left>") 'org-promote-subtree)
  (local-set-key (kbd "M-<right>") 'org-demote-subtree)
  (local-set-key (kbd "C-S-l") 'org-toggle-link-display)
  (local-set-key (kbd "C-'") 'org-edit-special))

(add-hook 'org-mode-hook 'my-org-keybindings)

;; Helper functions for text formatting
(defun my-org-toggle-bold ()
  "Toggle bold emphasis on region or insert bold markers."
  (interactive)
  (if (region-active-p)
      (org-emphasize ?*)
    (insert "****")
    (backward-char 2)))

(defun my-org-toggle-italic ()
  "Toggle italic emphasis on region or insert italic markers."
  (interactive)
  (if (region-active-p)
      (org-emphasize ?/)
    (insert "//")
    (backward-char 1)))

(defun my-org-toggle-underline ()
  "Toggle underline emphasis on region or insert underline markers."
  (interactive)
  (if (region-active-p)
      (org-emphasize ?_)
    (insert "__")
    (backward-char 1)))

(defun my-org-toggle-strike ()
  "Toggle strikethrough on region or insert strike markers."
  (interactive)
  (if (region-active-p)
      (org-emphasize ?+)
    (insert "++")
    (backward-char 1)))

(defun my-org-toggle-code ()
  "Toggle code markup on region or insert code markers."
  (interactive)
  (if (region-active-p)
      (org-emphasize ?~)
    (insert "~~")
    (backward-char 1)))

(defun my-org-insert-link ()
  "Insert or edit org link, WYSIWYG style."
  (interactive)
  (if (region-active-p)
      (call-interactively 'org-insert-link)
    (org-insert-link)))

;; Leader key setup (SPC in normal mode if using evil)
(defvar my-leader-key "C-c")

(define-key global-map (kbd (concat my-leader-key " f f")) 'find-file)
(define-key global-map (kbd (concat my-leader-key " f r")) 'recentf-open-files)
(define-key global-map (kbd (concat my-leader-key " f s")) 'save-buffer)
(define-key global-map (kbd (concat my-leader-key " b b")) 'switch-to-buffer)
(define-key global-map (kbd (concat my-leader-key " b k")) 'kill-buffer)
(define-key global-map (kbd (concat my-leader-key " w d")) 'delete-window)
(define-key global-map (kbd (concat my-leader-key " w v")) 'split-window-right)
(define-key global-map (kbd (concat my-leader-key " w s")) 'split-window-below)

;; Org-specific leader bindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd (concat my-leader-key " o a")) 'org-agenda)
  (define-key org-mode-map (kbd (concat my-leader-key " o c")) 'org-capture)
  (define-key org-mode-map (kbd (concat my-leader-key " o t")) 'org-todo)
  (define-key org-mode-map (kbd (concat my-leader-key " o s")) 'org-schedule)
  (define-key org-mode-map (kbd (concat my-leader-key " o d")) 'org-deadline)
  (define-key org-mode-map (kbd (concat my-leader-key " o e")) 'org-export-dispatch)
  (define-key org-mode-map (kbd (concat my-leader-key " o i")) 'org-insert-structure-template))

(provide 'keybindings)
;;; keybindings.el ends here
