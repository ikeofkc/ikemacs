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

(use-package crdt
  :ensure t
  :bind
  ("C-c c s" . crdt-share-buffer)
  ("C-c c c" . crdt-connect)
  ("C-c c d" . crdt-disconnect))

(provide 'ikemacs-code)