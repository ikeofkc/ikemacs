;;; org-config.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-mode setup with WYSIWYG-style features

;;; Code:

(use-package org
  :config
  ;; Modern org appearance
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300)
        org-ellipsis " ▾"
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-cycle-separator-lines 2)

  ;; Better list bullets
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Agenda files
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list org-directory)))

;; Org bullets for better headers
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Visual fill column for better writing
(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq-default visual-fill-column-width 100
                visual-fill-column-center-text t))

;; Org modern for even better aesthetics
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table nil
        org-modern-keyword nil
        org-modern-timestamp nil
        org-modern-priority nil))

;; Export backends
(require 'ox-md nil t)
(require 'ox-html nil t)

;; Template expansion
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(provide 'org-config)
;;; org-config.el ends here
