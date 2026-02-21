;;; ikemacs-welcome.el --- Custom Start Page -*- lexical-binding: t; -*-

(require 'tab-bar)

(defvar ike/welcome-buffer-name "*Ikemacs Home*")

(defun ike/welcome--insert-centered (text &optional face char-width-override)
  "Insert TEXT centered, optionally applying FACE.
   CHAR-WIDTH-OVERRIDE adjusts the math if a face (like height 3.0) 
   drastically changes the visual width of the string."
  (let* ((width (window-width))
         (len (or char-width-override (length text)))
         (padding (max 0 (truncate (/ (- width len) 2)))))
    ;; Force the padding spaces to be monospace so they are predictable
    (insert (propertize (make-string padding ?\s) 'face 'fixed-pitch)
            (if face (propertize text 'face face) text)
            "\n")))

(defun ike/welcome--insert-logo ()
  "Insert the SVG logo centered and tinted white."
  (let ((logo-path (expand-file-name "ikemacs/ikemacs-logo.svg" user-emacs-directory)))
    (when (and (display-graphic-p) (file-exists-p logo-path))
      (let* ((img (create-image logo-path 'svg nil :foreground "#ffffff" :scale 1.5))
             (img-width-chars (car (image-size img)))
             (padding (max 0 (truncate (/ (- (window-width) img-width-chars) 2)))))
        ;; Again, use monospace padding to ensure exact centering
        (insert (propertize (make-string padding ?\s) 'face 'fixed-pitch))
        (insert-image img)
        (insert "\n")))))

(defun ike/welcome-render ()
  "Draws the content of the center-aligned welcome buffer."
  (with-current-buffer (get-buffer-create ike/welcome-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (fundamental-mode)
      
      (variable-pitch-mode 1) 
      
      (insert "\n\n\n\n") 
      
      ;; 1. SVG Logo
      (ike/welcome--insert-logo)
      (insert "\n")
      
      ;; 2. Title (White, Height 3.0)
      (ike/welcome--insert-centered "Ikemacs" '(:height 3.0 :weight bold :foreground "#ffffff") 21)
      (insert "\n")
      
      ;; 3. Summary
      (ike/welcome--insert-centered "An emacs config optimized for org-mode outlining" 
                                    '(:inherit fixed-pitch :slant italic :foreground "#6272a4"))
      (ike/welcome--insert-centered "with keyboard shortcuts made for mere mortals" 
                                    '(:inherit fixed-pitch :slant italic :foreground "#6272a4"))
      (insert "\n\n")
      
      ;; 4. Sidebar Instructions
       (ike/welcome--insert-centered "👈 See the sidebar on the left for easy commands"
                                    '(:inherit fixed-pitch :slant italic :foreground "#ffffff"))
       (ike/welcome--insert-centered "Select the menu icon (≡) to expand it for more wow."
                                     '(:inherit fixed-pitch :slant italic :foreground "#6272a4"))
      (insert "\n\n\n")
      
      ;; Helper lambda to cleanly align the table columns using fixed-pitch
      (let ((insert-row (lambda (col1 col2 col3 &optional is-header)
                          (let* ((row (format "%-25s %-18s %-15s" col1 col2 col3))
                                 (width (window-width))
                                 (len (length row))
                                 (padding (max 0 (truncate (/ (- width len) 2))))
                                 (pad-str (make-string padding ?\s)))
                            (insert (propertize pad-str 'face 'fixed-pitch)
                                    (propertize row 'face (if is-header
                                                              '(:inherit fixed-pitch :weight bold :foreground "#ff79c6")
                                                            '(:inherit fixed-pitch :foreground "#b4befe")))
                                    "\n")))))
        
        ;; 5. Keybindings Table
        (ike/welcome--insert-centered "--- Modern Keybindings ---" '(:weight bold :foreground "#ff79c6"))
        (insert "\n")
        
        (funcall insert-row "ACTION" "SHORTCUT" "CONTEXT" t)
        (funcall insert-row "------" "--------" "-------" t)
        (funcall insert-row "Save File" "C-s" "Global")
        (funcall insert-row "Save As..." "C-S-s" "Global")
        (funcall insert-row "Open File" "C-o" "Global")
        (funcall insert-row "Smart Close Tab/Win" "C-w" "Global")
        (funcall insert-row "New Tab" "C-t" "Global")
        (funcall insert-row "Restore Tab" "C-S-t" "Global")
        (funcall insert-row "Next / Prev Tab" "C-tab / C-S-tab" "Global")
        (funcall insert-row "Command Palette" "M-SPC" "Global")
        (funcall insert-row "Select All" "C-a" "Global")
        (funcall insert-row "Toggle Comment" "C-/" "Code/Text")
        (funcall insert-row "Format Bold" "C-b" "Org Mode")
        (funcall insert-row "Format Italic" "C-i" "Org Mode")
        (funcall insert-row "Format Underline" "C-u" "Org Mode")
        (funcall insert-row "Format Menu (Popup)" "C-e" "Org Mode")
        
        (insert "\n\n")
        
        ;; 6. Org Quirks Table
        (ike/welcome--insert-centered "--- Preserved Org Quirks ---" '(:weight bold :foreground "#ffb86c"))
        (insert "\n")
        
        ;; Temporarily change header color for this table
        (let ((insert-row-quirks (lambda (col1 col2 col3 &optional is-header)
                                   (let* ((row (format "%-25s %-18s %-15s" col1 col2 col3))
                                          (width (window-width))
                                          (len (length row))
                                          (padding (max 0 (truncate (/ (- width len) 2))))
                                          (pad-str (make-string padding ?\s)))
                                     (insert (propertize pad-str 'face 'fixed-pitch)
                                             (propertize row 'face (if is-header
                                                                       '(:inherit fixed-pitch :weight bold :foreground "#ffb86c")
                                                                     '(:inherit fixed-pitch :foreground "#b4befe")))
                                             "\n")))))
          (funcall insert-row-quirks "ACTION" "SHORTCUT" "REASON" t)
          (funcall insert-row-quirks "------" "--------" "------" t)
          (funcall insert-row-quirks "Move/Promote Headings" "Alt + Arrows" "Core Org Outline")
          (funcall insert-row-quirks "Change Dates/TODOs" "Shift + Arrows" "Core Org Workflow")
          (funcall insert-row-quirks "Begin Text Selection" "Ctrl + SPC" "Avoids Arrow Clashes")))
        
      (insert "\n\n")
      
      ;; 7. Config Summary for Veterans
      (ike/welcome--insert-centered "--- Notable Config Changes (For Emacs Veterans) ---" '(:weight bold :foreground "#8be9fd"))
      (insert "\n")
      (ike/welcome--insert-centered "• Org headlines are explicitly un-bolded to maintain a clean outliner focus." '(:inherit fixed-pitch :foreground "#f8f8f2"))
      (ike/welcome--insert-centered "• CUA mode is active for standard Copy/Paste, but Mark (C-SPC) is preserved." '(:inherit fixed-pitch :foreground "#f8f8f2"))
      (ike/welcome--insert-centered "• Mixed-pitch typography is enabled automatically for Org-mode buffers." '(:inherit fixed-pitch :foreground "#f8f8f2"))
      (ike/welcome--insert-centered "• Default UI chrome (menus, toolbars, scrollbars) is disabled for minimalism." '(:inherit fixed-pitch :foreground "#f8f8f2"))
      
      (insert "\n\n\n")
      (ike/welcome--insert-centered (format "Current Time: %s" (format-time-string "%Y-%m-%d %H:%M")) '(:foreground "#6272a4"))
      
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (setq-local cursor-type nil)
      (local-set-key (kbd "q") 'bury-buffer))))

;; --- 1. DYNAMIC RECENTERING ---
(defun ike/welcome--resize-watchdog (&optional _frame)
  "Redraw the welcome screen to maintain centering if the window size changes."
  (when (get-buffer-window ike/welcome-buffer-name)
    (let ((win (get-buffer-window ike/welcome-buffer-name)))
      (with-selected-window win
        (ike/welcome-render)))))

(add-hook 'window-size-change-functions #'ike/welcome--resize-watchdog)

;; --- 2. FUNCTION FOR NEW TABS ---
(defun ike/get-welcome-buffer-for-tab ()
  "Render and return the buffer object (Required by tab-bar)."
  (ike/welcome-render)
  (get-buffer-create ike/welcome-buffer-name))

(setq tab-bar-new-tab-choice #'ike/get-welcome-buffer-for-tab)

;; --- 3. FUNCTION FOR STARTUP ---
(defun ike/smart-startup-screen ()
  "Show welcome screen ONLY if no file was opened via command line."
  (unless (buffer-file-name)
    (ike/welcome-render)
    (switch-to-buffer ike/welcome-buffer-name)))

(add-hook 'emacs-startup-hook #'ike/smart-startup-screen)

(provide 'ikemacs-welcome)
