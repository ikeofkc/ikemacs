;; ADD C-U-A mode
(setq cua-rectangle-mark-key (kbd "C-`"))
(cua-mode t)

;; Set up Alt+Space to open the command palette
(global-set-key (kbd "M-SPC") 'execute-extended-command)

;; set Ctrl-Q to quit with save prompt
(define-key global-map (kbd "C-q") nil)

;; Bind C-o to behave like 'visit new file' using system file dialog
(define-key global-map (kbd "C-o") nil)
(define-key global-map (kbd "C-o") #'ike/gui-visit-file-dialog)

;; Remap C-s to save the current buffer
(define-key global-map (kbd "C-s") nil)
(define-key global-map (kbd "C-s") #'ike/gui-save-file)

;; Remap Ctrl-Shift-s to save with a specific filename
(define-key global-map (kbd "C-S-s") nil)
(define-key global-map (kbd "C-S-s") #'ike/gui-save-file-as)

;; Remap Ctrl-Shift-Z to perform redo
(define-key global-map (kbd "C-S-z") nil)
(define-key global-map (kbd "C-S-z") 'undo-redo)

;; Remap C-A to select all
(define-key global-map (kbd "C-a") nil)
(define-key global-map (kbd "C-a") 'mark-whole-buffer)

;; Remap C-/ to select all
(define-key global-map (kbd "C-/") nil)
(define-key global-map (kbd "C-/") 'comment-line)

;; Make ESC do same thing as Ctrl-G
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Remap C-<tab> and <C-S-<tab> to cycle between tab-bar tabs
(define-key global-map (kbd "C-<tab>") nil)
(define-key global-map (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
(define-key global-map (kbd "C-S-<tab>") nil)
(define-key global-map (kbd "C-S-<tab>") 'tab-bar-switch-to-prev-tab)

;; Creating a new tab with Ctrl+t
(define-key global-map (kbd "C-t") nil)
(define-key global-map (kbd "C-t") 'tab-bar-new-tab)

(define-key global-map (kbd "C-w") nil)
;; (define-key global-map (kbd "C-w") 'tab-close)

;; Custom function to handle hierarchical closing
(defun ike/smart-close ()
  "Closes the active buffer.
   1. Kills the current buffer.
   2. Tries to delete the window (handling splits).
   3. If deleting the window fails (because it's the last one or main window),
      it closes the tab.
   4. If it's the last tab, it quits Emacs."
  (interactive)
  ;; Step 1: Try to kill the buffer. If cancelled (returns nil), stop everything.
  (when (kill-buffer (current-buffer))
    ;; Step 2: Try to delete the window
    (condition-case nil
        (delete-window)
      ;; Step 3: Catch the error! 
      ;; If delete-window fails (e.g. "Attempt to delete main window"), 
      ;; it means we are at the last window of the tab.
      (error
       (if (= 1 (length (tab-bar-tabs)))
           ;; Step 4: Last Tab -> Quit
           (save-buffers-kill-emacs)
         ;; Else -> Close Tab
         (tab-bar-close-tab))))))

;; Remap C-w (ensure this is still present)
(define-key global-map (kbd "C-w") nil)
(define-key global-map (kbd "C-w") 'ike/smart-close)

;; Remap C-w to the smart close function
(define-key global-map (kbd "C-w") nil)
(define-key global-map (kbd "C-w") 'ike/smart-close)


(provide 'ikemacs-keys)
