;;; ikemacs-topbar.el --- Browser-Style Top Toolbar -*- lexical-binding: t; -*-

;; Author: Ikemacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.1.0"))
;; Keywords: convenience, frames

;;; Commentary:
;;
;; A global top toolbar providing browser-like navigation:
;;   [×] Smart Close  [←] Back  [→] Forward  [≡] Buffer List
;;   [⊞] Split Right  |  ~/path/to/file  |  [🗂] File Tree
;;
;; The address bar shows the current file path (shortened with ~/)
;; and is clickable for quick file navigation with completion.
;;
;; The file tree opens a persistent right sidebar targeting the
;; last active window, following the same patterns as the left sidebar.
;;
;; Quick start:
;;   (require 'ikemacs-topbar)
;;   (ike/topbar-mode 1)

;;; Code:

;; ═══════════════════════════════════════════════════════
;; DEPENDENCIES
;; ═══════════════════════════════════════════════════════

(require 'nerd-icons)
(require 'seq)

;; ═══════════════════════════════════════════════════════
;; CUSTOMIZATION
;; ═══════════════════════════════════════════════════════

(defgroup ike-topbar nil
  "Browser-style top toolbar for Ikemacs."
  :group 'convenience
  :prefix "ike/topbar-")

(defcustom ike/topbar-file-tree-width 30
  "Width of the file tree sidebar."
  :type 'integer
  :group 'ike-topbar)

;; ═══════════════════════════════════════════════════════
;; INTERNAL STATE
;; ═══════════════════════════════════════════════════════

(defvar ike/topbar-buffer-name " *Ikemacs Topbar*")
(defvar ike/topbar--window nil "Window displaying the topbar.")

;; File tree
(defvar ike/topbar-ftree-buffer-name "*File Tree*")
(defvar ike/topbar-ftree--window nil "Window displaying the file tree.")
(defvar ike/topbar-ftree--root nil "Current root directory for the file tree.")
(defvar ike/topbar-ftree--expanded (make-hash-table :test 'equal)
  "Hash of expanded directory paths.")

;; Window tracking — reuses sidebar's tracked window when available,
;; but also maintains its own fallback so the topbar works standalone.
(defvar ike/topbar--last-window nil
  "Last valid editing window tracked by the topbar.")

;; Dirty flag to avoid excessive re-renders
(defvar ike/topbar--last-rendered-path nil
  "The path string that was last rendered in the address bar.")

;; ═══════════════════════════════════════════════════════
;; WINDOW TRACKING
;; ═══════════════════════════════════════════════════════

(defun ike/topbar--dedicated-buffer-p (buf)
  "Return t if BUF is one of ikemacs' chrome buffers."
  (let ((name (buffer-name buf)))
    (or (string= name ike/topbar-buffer-name)
        (string= name ike/topbar-ftree-buffer-name)
        (and (boundp 'ike/sidebar-buffer-name)
             (string= name ike/sidebar-buffer-name))
        (string= name " *Org Gadget Toolbar*"))))

(defun ike/topbar--record-window (&optional _frame-or-window)
  "Track the last valid editing window (excludes chrome buffers)."
  (let ((win (selected-window)))
    (when (and (window-live-p win)
               (not (window-minibuffer-p win))
               (not (window-dedicated-p win))
               (not (ike/topbar--dedicated-buffer-p (window-buffer win))))
      (setq ike/topbar--last-window win))))

(defun ike/topbar--select-main-window ()
  "Focus the last known editable window."
  ;; Prefer the sidebar's tracked window if available (it's the same concept)
  (let ((sidebar-win (and (boundp 'ike/sidebar-last-window)
                          ike/sidebar-last-window)))
    (cond
     ;; Try sidebar's tracked window first
     ((and sidebar-win
           (window-live-p sidebar-win)
           (not (ike/topbar--dedicated-buffer-p (window-buffer sidebar-win))))
      (select-window sidebar-win))
     ;; Try our own tracked window
     ((and ike/topbar--last-window
           (window-live-p ike/topbar--last-window)
           (not (ike/topbar--dedicated-buffer-p (window-buffer ike/topbar--last-window))))
      (select-window ike/topbar--last-window))
     ;; Fallback: find any non-dedicated, non-chrome window
     (t
      (let ((win (get-window-with-predicate
                  (lambda (w)
                    (and (not (window-dedicated-p w))
                         (not (window-minibuffer-p w))
                         (not (ike/topbar--dedicated-buffer-p (window-buffer w))))))))
        (when (and win (window-live-p win))
          (select-window win)))))))

(defun ike/topbar--make-callback (fn)
  "Create a deferred callback that runs FN in the main editing window.
Same timer pattern as the sidebar for reliable mouse event handling."
  (lambda (&rest _)
    (interactive)
    (run-with-timer
     0.05 nil
     (lambda ()
       (ike/topbar--select-main-window)
       (if (commandp fn)
           (call-interactively fn)
         (funcall fn))))))

;; ═══════════════════════════════════════════════════════
;; HELPERS: PATH DISPLAY
;; ═══════════════════════════════════════════════════════

(defun ike/topbar--shorten-path (path)
  "Replace the home directory prefix with ~/ in PATH."
  (if path
      (let ((home (expand-file-name "~")))
        (if (string-prefix-p home path)
            (concat "~" (substring path (length home)))
          path))
    "No File"))

(defun ike/topbar--current-file-path ()
  "Get the path to display in the address bar.
Uses the focused main window's buffer, not the topbar's buffer."
  (let* ((win (or (and (boundp 'ike/sidebar-last-window)
                       (window-live-p ike/sidebar-last-window)
                       ike/sidebar-last-window)
                  (and ike/topbar--last-window
                       (window-live-p ike/topbar--last-window)
                       ike/topbar--last-window)))
         (buf (if win (window-buffer win) (current-buffer)))
         (file (buffer-file-name buf)))
    (if file
        (ike/topbar--shorten-path file)
      (format "[%s]" (buffer-name buf)))))

;; ═══════════════════════════════════════════════════════
;; ICON HELPER (matches sidebar pattern)
;; ═══════════════════════════════════════════════════════

(defun ike/topbar--icon (name &optional face)
  "Render nerd-icon NAME colored by FACE."
  (let* ((color (face-foreground (or face 'default) nil t))
         (fn (cond
              ((string-prefix-p "nf-fa-" name)  #'nerd-icons-faicon)
              ((string-prefix-p "nf-cod-" name) #'nerd-icons-codicon)
              ((string-prefix-p "nf-oct-" name) #'nerd-icons-octicon)
              (t                                #'nerd-icons-mdicon))))
    (funcall fn name :height 1.0 :v-adjust -0.1 :face `(:foreground ,color))))

;; ═══════════════════════════════════════════════════════
;; TOOLBAR ICON INSERTION
;; ═══════════════════════════════════════════════════════

(defun ike/topbar--insert-icon (icon-str callback &optional tooltip pad-left pad-right)
  "Insert ICON-STR as a clickable icon running CALLBACK.
PAD-LEFT and PAD-RIGHT default to \" \" (one space).
Uses the sidebar's proven down-mouse-1 → ignore, mouse-1 → callback pattern."
  (let ((inhibit-read-only t)
        (map (make-sparse-keymap))
        (lpad (or pad-left " "))
        (rpad (or pad-right " ")))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] callback)
    (define-key map [return] callback)
    (insert lpad)
    (insert (propertize icon-str
                        'help-echo (or tooltip "")
                        'mouse-face 'highlight
                        'keymap map
                        'pointer 'hand))
    (insert rpad)))

;; ═══════════════════════════════════════════════════════
;; NAVIGATION COMMANDS
;; ═══════════════════════════════════════════════════════

(defun ike/topbar-go-back ()
  "Switch to the previous buffer in the current window."
  (interactive)
  (previous-buffer))

(defun ike/topbar-go-forward ()
  "Switch to the next buffer in the current window."
  (interactive)
  (next-buffer))

(defun ike/topbar-list-buffers ()
  "List open buffers using ivy if available, otherwise buffer-menu."
  (interactive)
  (if (fboundp 'ivy-switch-buffer)
      (call-interactively #'ivy-switch-buffer)
    (call-interactively #'buffer-menu)))

(defun ike/topbar-address-navigate ()
  "Open a file using completion, starting from the current file's directory."
  (interactive)
  (let* ((buf (if (and ike/topbar--last-window (window-live-p ike/topbar--last-window))
                  (window-buffer ike/topbar--last-window)
                (current-buffer)))
         (dir (with-current-buffer buf
                (if buffer-file-name
                    (file-name-directory buffer-file-name)
                  default-directory)))
         ;; Use the dialog trick from ikemacs-dialogs if gui is available,
         ;; otherwise fall back to minibuffer completion (ivy will kick in)
         (filename (read-file-name "Go to: " dir)))
    (when filename
      (find-file filename))))

;; ═══════════════════════════════════════════════════════
;; TOOLBAR RENDERING
;; ═══════════════════════════════════════════════════════

(defun ike/topbar--render ()
  "Draw the top toolbar contents."
  (let ((path (ike/topbar--current-file-path)))
    (with-current-buffer (get-buffer-create ike/topbar-buffer-name)
      (let ((inhibit-read-only t)
            (color-default (face-foreground 'default nil t))
            (color-warn    (face-foreground 'error nil t))
            (color-accent  (face-foreground 'font-lock-keyword-face nil t))
            (color-dim     (face-foreground 'font-lock-comment-face nil t))
            (color-path    (face-foreground 'font-lock-string-face nil t)))
        (erase-buffer)

        ;; --- CLOSE (tight left padding to align with sidebar below) ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-close" 'error)
         (ike/topbar--make-callback #'ike/smart-close)
         "Smart Close (C-w)" "" " ")

        ;; --- SEPARATOR ---
        (insert (propertize "│" 'face `(:foreground ,color-dim)))

        ;; --- BACK ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-arrow_left")
         (ike/topbar--make-callback #'ike/topbar-go-back)
         "Previous Buffer")

        ;; --- FORWARD ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-arrow_right")
         (ike/topbar--make-callback #'ike/topbar-go-forward)
         "Next Buffer")

        ;; --- SEPARATOR ---
        (insert (propertize "│" 'face `(:foreground ,color-dim)))

        ;; --- NEW TAB ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-plus")
         (ike/topbar--make-callback #'tab-bar-new-tab)
         "New Tab (C-t)")

        ;; --- BUFFER LIST ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-view_list" 'font-lock-keyword-face)
         (ike/topbar--make-callback #'ike/topbar-list-buffers)
         "List Open Buffers")

        ;; --- SPLIT RIGHT ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-arrow_split_vertical" 'font-lock-function-name-face)
         (ike/topbar--make-callback #'split-window-right)
         "Split Window Right")

        ;; --- SPLIT BELOW ---
        (ike/topbar--insert-icon
         (ike/topbar--icon "nf-md-arrow_split_horizontal" 'font-lock-function-name-face)
         (ike/topbar--make-callback #'split-window-below)
         "Split Window Below")

        ;; --- FILE TREE (before the address bar, next to split icons) ---
        (let* ((tree-active (and ike/topbar-ftree--window
                                 (window-live-p ike/topbar-ftree--window)))
               (tree-face (if tree-active 'font-lock-keyword-face 'default)))
          (ike/topbar--insert-icon
           (ike/topbar--icon "nf-md-file_tree" tree-face)
           (lambda (&rest _)
             (interactive)
             (run-with-timer 0.05 nil #'ike/topbar-ftree-toggle))
           "Toggle File Tree"))

        ;; --- SEPARATOR ---
        (insert (propertize " │ " 'face `(:foreground ,color-dim)))

        ;; --- ADDRESS BAR (last element, fills remaining space) ---
        (let ((addr-map (make-sparse-keymap))
              (addr-cb (ike/topbar--make-callback #'ike/topbar-address-navigate)))
          (define-key addr-map [down-mouse-1] #'ignore)
          (define-key addr-map [mouse-1] addr-cb)
          (define-key addr-map [return] addr-cb)
          (insert (propertize path
                              'face `(:foreground ,color-path :underline nil)
                              'help-echo "Click to navigate to a file"
                              'mouse-face 'highlight
                              'keymap addr-map
                              'pointer 'hand)))

        (goto-char (point-min))
        (read-only-mode 1)
        (setq-local truncate-lines t)
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil)
        (setq-local window-size-fixed 'height))
      
      (setq ike/topbar--last-rendered-path path))))

;; ═══════════════════════════════════════════════════════
;; SHOW / HIDE
;; ═══════════════════════════════════════════════════════

(defun ike/topbar--show ()
  "Show the topbar as a top side window."
  (unless (minibuffer-window-active-p (minibuffer-window))
    (let ((buf (get-buffer-create ike/topbar-buffer-name)))
      (unless (and ike/topbar--window (window-live-p ike/topbar--window))
        (setq ike/topbar--window
              (display-buffer-in-side-window
               buf
               '((side . top)
                 (slot . 0)
                 (window-height . 1)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))))
      (set-window-dedicated-p ike/topbar--window t)
      (set-window-parameter ike/topbar--window 'min-height 1)
      (window-preserve-size ike/topbar--window nil t)
      (with-selected-window ike/topbar--window
        (ike/topbar--render)))))

(defun ike/topbar--hide ()
  "Hide the topbar."
  (when (and ike/topbar--window (window-live-p ike/topbar--window))
    (delete-window ike/topbar--window)
    (setq ike/topbar--window nil)))

(defun ike/topbar--update (&optional _frame-or-window)
  "Re-render the topbar if the displayed path has changed.
Called from window change hooks — avoids redundant redraws."
  (when (and ike/topbar--window (window-live-p ike/topbar--window))
    (ike/topbar--record-window)
    (let ((new-path (ike/topbar--current-file-path)))
      (unless (equal new-path ike/topbar--last-rendered-path)
        (ike/topbar--render)))))

;; ═══════════════════════════════════════════════════════
;; FILE TREE — RIGHT SIDEBAR
;; ═══════════════════════════════════════════════════════

(defun ike/topbar-ftree--default-root ()
  "Determine a sensible root for the file tree.
Uses project root if available, else the current file's directory."
  (or (and (fboundp 'project-root) (project-current)
           (project-root (project-current)))
      (and ike/topbar--last-window
           (window-live-p ike/topbar--last-window)
           (with-current-buffer (window-buffer ike/topbar--last-window)
             (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))
      default-directory))

(defun ike/topbar-ftree--entries (dir)
  "Return a sorted list of (NAME . FULL-PATH) for DIR.
Directories come first, dotfiles are excluded."
  (when (file-directory-p dir)
    (let* ((files (directory-files dir nil "^[^.]"))  ;; skip dotfiles
           (dirs  '())
           (regs  '()))
      (dolist (f files)
        (let ((full (expand-file-name f dir)))
          (if (file-directory-p full)
              (push (cons (concat f "/") full) dirs)
            (push (cons f full) regs))))
      (append (sort dirs (lambda (a b) (string< (car a) (car b))))
              (sort regs (lambda (a b) (string< (car a) (car b))))))))

(defun ike/topbar-ftree--render ()
  "Draw the file tree contents."
  (with-current-buffer (get-buffer-create ike/topbar-ftree-buffer-name)
    (let ((inhibit-read-only t)
          (root (or ike/topbar-ftree--root (ike/topbar-ftree--default-root)))
          (color-dir (face-foreground 'font-lock-function-name-face nil t))
          (color-file (face-foreground 'default nil t))
          (color-dim (face-foreground 'font-lock-comment-face nil t)))
      (erase-buffer)
      (setq ike/topbar-ftree--root root)

      ;; --- HEADER: Root path + parent nav ---
      (let* ((short-root (ike/topbar--shorten-path (directory-file-name root)))
             (up-map (make-sparse-keymap))
             (up-cb (lambda (&rest _)
                      (interactive)
                      (setq ike/topbar-ftree--root
                            (file-name-directory (directory-file-name root)))
                      (ike/topbar-ftree--render))))
        (define-key up-map [down-mouse-1] #'ignore)
        (define-key up-map [mouse-1] up-cb)
        (insert (propertize " ↑ " 'face `(:foreground ,color-dir)
                            'help-echo "Go to parent directory"
                            'mouse-face 'highlight
                            'keymap up-map
                            'pointer 'hand))
        (insert (propertize (truncate-string-to-width short-root 28 nil nil "…")
                            'face 'bold))
        (insert "\n")
        (insert (propertize (make-string 30 ?─) 'face `(:foreground ,color-dim)))
        (insert "\n"))

      ;; --- RECURSIVE TREE ---
      (ike/topbar-ftree--insert-level root 0)

      (goto-char (point-min))
      (read-only-mode 1)
      (setq-local truncate-lines t)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil))))

(defun ike/topbar-ftree--insert-level (dir depth)
  "Insert file tree entries for DIR at indentation DEPTH."
  (let ((entries (ike/topbar-ftree--entries dir))
        (indent (make-string (* depth 2) ?\s))
        (color-dir (face-foreground 'font-lock-function-name-face nil t))
        (color-file (face-foreground 'default nil t)))
    (dolist (entry entries)
      (let* ((name (car entry))
             (full-path (cdr entry))
             (is-dir (string-suffix-p "/" name))
             (expanded (gethash full-path ike/topbar-ftree--expanded))
             (map (make-sparse-keymap)))

        (if is-dir
            ;; --- DIRECTORY: toggle expand/collapse ---
            (let ((dir-cb (lambda (&rest _)
                            (interactive)
                            (if (gethash full-path ike/topbar-ftree--expanded)
                                (remhash full-path ike/topbar-ftree--expanded)
                              (puthash full-path t ike/topbar-ftree--expanded))
                            (ike/topbar-ftree--render))))
              (define-key map [down-mouse-1] #'ignore)
              (define-key map [mouse-1] dir-cb)
              (define-key map [return] dir-cb)
              (let* ((chevron (if expanded "▽ " "▷ "))
                     (icon (nerd-icons-mdicon
                            (if expanded "nf-md-folder_open" "nf-md-folder")
                            :height 0.9 :v-adjust -0.1
                            :face `(:foreground ,color-dir)))
                     (label (string-remove-suffix "/" name))
                     ;; Build keymap/mouse props to apply to ALL parts
                     (props `(help-echo ,full-path
                              mouse-face highlight
                              keymap ,map
                              pointer hand)))
                (insert indent)
                ;; Chevron — apply face + interactive props
                (insert (apply #'propertize chevron
                               'face `(:foreground ,color-dir)
                               props))
                ;; Icon — only add interactive props, keep icon's own face
                (insert (apply #'propertize icon props))
                ;; Label — apply face + interactive props
                (insert (apply #'propertize (concat " " label)
                               'face `(:foreground ,color-dir)
                               props)))
              (insert "\n")
              ;; Recurse into expanded directories
              (when expanded
                (ike/topbar-ftree--insert-level full-path (1+ depth))))

          ;; --- FILE: open in main window ---
          (let ((file-cb (lambda (&rest _)
                           (interactive)
                           (run-with-timer
                            0.05 nil
                            (lambda ()
                              (ike/topbar--select-main-window)
                              (find-file full-path))))))
            (define-key map [down-mouse-1] #'ignore)
            (define-key map [mouse-1] file-cb)
            (define-key map [return] file-cb)
            (let* ((icon (nerd-icons-icon-for-file name :height 0.9 :v-adjust -0.1))
                   (props `(help-echo ,full-path
                            mouse-face highlight
                            keymap ,map
                            pointer hand)))
              (insert indent)
              (insert "  ")
              ;; Icon — only interactive props, preserve icon face
              (insert (apply #'propertize icon props))
              ;; Filename — apply default face + interactive props
              (insert (apply #'propertize (concat " " name)
                             'face 'default
                             props)))
            (insert "\n")))))))

;; ═══════════════════════════════════════════════════════
;; FILE TREE — SHOW / HIDE / TOGGLE
;; ═══════════════════════════════════════════════════════

(defun ike/topbar-ftree-show ()
  "Show the file tree in a right side window."
  (interactive)
  (let ((buf (get-buffer-create ike/topbar-ftree-buffer-name)))
    (setq ike/topbar-ftree--root (ike/topbar-ftree--default-root))
    (unless (and ike/topbar-ftree--window (window-live-p ike/topbar-ftree--window))
      (setq ike/topbar-ftree--window
            (display-buffer-in-side-window
             buf
             `((side . right)
               (slot . 0)
               (window-width . ,ike/topbar-file-tree-width)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))))
    (set-window-dedicated-p ike/topbar-ftree--window t)
    (window-preserve-size ike/topbar-ftree--window t t)
    (with-selected-window ike/topbar-ftree--window
      (ike/topbar-ftree--render))
    ;; Re-render topbar to update the tree icon state
    (when (and ike/topbar--window (window-live-p ike/topbar--window))
      (ike/topbar--render))))

(defun ike/topbar-ftree-hide ()
  "Hide the file tree sidebar."
  (interactive)
  (when (and ike/topbar-ftree--window (window-live-p ike/topbar-ftree--window))
    (delete-window ike/topbar-ftree--window)
    (setq ike/topbar-ftree--window nil))
  ;; Re-render topbar to update the tree icon state
  (when (and ike/topbar--window (window-live-p ike/topbar--window))
    (ike/topbar--render)))

(defun ike/topbar-ftree-toggle ()
  "Toggle the file tree sidebar."
  (interactive)
  (if (and ike/topbar-ftree--window (window-live-p ike/topbar-ftree--window))
      (ike/topbar-ftree-hide)
    (ike/topbar-ftree-show)))

(defun ike/topbar-ftree-refresh ()
  "Refresh the file tree contents."
  (interactive)
  (when (and ike/topbar-ftree--window (window-live-p ike/topbar-ftree--window))
    (ike/topbar-ftree--render)))

;; ═══════════════════════════════════════════════════════
;; MINOR MODE
;; ═══════════════════════════════════════════════════════

;;;###autoload
(define-minor-mode ike/topbar-mode
  "Global minor mode for the Ikemacs top toolbar."
  :global t
  :group 'ike-topbar
  (if ike/topbar-mode
      (progn
        (add-hook 'window-selection-change-functions #'ike/topbar--update)
        (add-hook 'window-buffer-change-functions #'ike/topbar--update)
        ;; Refresh file tree on save (new files may appear)
        (add-hook 'after-save-hook #'ike/topbar-ftree-refresh)
        ;; Show on tab changes (same as sidebar)
        (add-hook 'tab-bar-tab-post-select-functions (lambda (&rest _) (ike/topbar--show)))
        (add-hook 'tab-bar-tab-post-open-functions (lambda (&rest _) (ike/topbar--show)))
        (ike/topbar--show))
    ;; Cleanup
    (remove-hook 'window-selection-change-functions #'ike/topbar--update)
    (remove-hook 'window-buffer-change-functions #'ike/topbar--update)
    (remove-hook 'after-save-hook #'ike/topbar-ftree-refresh)
    (ike/topbar--hide)
    (ike/topbar-ftree-hide)))

(provide 'ikemacs-topbar)
;;; ikemacs-topbar.el ends here
