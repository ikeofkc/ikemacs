(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.json\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :config
   (setq web-mode-code-indent-offset 2)
   (setq web-mode-css-indent-offset 2)
   (setq web-mode-markup-indent-offset 2))

;; Note: CRDT collaboration is configured in ikemacs-collab.el

(provide 'ikemacs-code)