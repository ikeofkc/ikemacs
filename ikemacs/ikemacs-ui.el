;;; ikemacs-ui.el --- UI Visuals and Theme -*- lexical-binding: t; -*-

(require 'seq) ;; Required for font detection

;; Disable toolbar/menubar/scrollbars early for speed
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;;; --- FRAME SIZING ---
;; Width is measured in characters, height is measured in lines
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(height . 35))

(use-package catppuccin-theme
  :ensure t
  :config
  (catppuccin-set-color 'base "#0f0b13") 
  (load-theme 'catppuccin :no-confirm)
  (set-face-attribute 'region nil :background "#039177" :foreground "#ffffff"))

;;; --- CURSOR SHAPE ---
(setq-default cursor-type '(bar . 2))
(add-hook 'overwrite-mode-hook
          (lambda ()
            (if overwrite-mode
                (setq cursor-type 'box)
              (setq cursor-type '(bar . 2)))))

;;; --- FONT FALLBACKS (SYMBOLS ONLY) ---
;; Note: We removed the Icon Mapping block because nerd-icons handles 
;; its own font registration automatically.

(when (display-graphic-p)
  ;; 1. SYMBOLS & DINGBATS (Checkboxes, Stars, Arrows, Shapes)
  ;;    Range: #x2000 -> #x3000 covers Arrows, Box Drawing, Geometric Shapes, and Misc Symbols.
  (let ((symbol-range '(#x2000 . #x3000)))
    ;; Windows: Excellent built-in coverage
    (set-fontset-font t symbol-range "Segoe UI Symbol" nil 'prepend)
    
    ;; Linux: Common symbol fonts (require installation)
    (set-fontset-font t symbol-range "Symbola" nil 'prepend)
    (set-fontset-font t symbol-range "Noto Sans Symbols" nil 'prepend)
    (set-fontset-font t symbol-range "Noto Sans Symbols 2" nil 'prepend)
    (set-fontset-font t symbol-range "DejaVu Sans" nil 'prepend)
    
  ;; "Nuclear Option": Noto Color Emoji often has excellent shape coverage
    (set-fontset-font t symbol-range "Noto Color Emoji" nil 'prepend))

  ;; EMOJIS (Smileys, etc.)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend))

;;; --- MIXED PITCH CONFIGURATION ---
(defvar ike/variable-pitch-fonts
  '("Jost"              ;; <--- YOUR PREFERENCE
    "Roboto"            ;; Modern Linux Standard
    "Segoe UI"          ;; Windows Standard
    "Helvetica Neue"    ;; macOS Standard
    "Arial"             ;; Fallback
    "Sans Serif")       ;; System Generic
  "Ordered list of preferred sans-serif fonts for prose.")

(defvar ike/fixed-pitch-fonts
  '("Google Sans Code"  ;; Your preference
    "JetBrains Mono"    ;; Popular Dev Font
    "Fira Code"         ;; Popular Dev Font
    "Consolas"          ;; Windows Standard
    "Menlo"             ;; macOS Standard
    "Monospace")        ;; System Generic
  "Ordered list of preferred monospace fonts for code.")

(defun ike/set-font-face (face font-list)
  "Finds the first available font from FONT-LIST and applies it to FACE."
  (let ((font (seq-find (lambda (f) (find-font (font-spec :name f))) font-list)))
    (when font
      (set-face-attribute face nil :family font :weight 'regular))))

(defun ike/enable-mixed-pitch ()
  "Enable mixed-pitch mode with auto-detected system fonts."
  (interactive)
  
  ;; 1. Setup the semantic faces dynamically
  (ike/set-font-face 'variable-pitch ike/variable-pitch-fonts)
  (ike/set-font-face 'fixed-pitch ike/fixed-pitch-fonts)
  
  ;; 2. Force specific Org elements to use the FIXED font
  (dolist (face '(org-code 
                  org-link 
                  org-block 
                  org-table 
                  org-verbatim 
                  org-block-begin-line 
                  org-block-end-line 
                  org-meta-line 
                  org-document-info-keyword 
                  org-checkbox 
                  org-priority 
                  org-tag))
    (set-face-attribute face nil :inherit 'fixed-pitch))

  ;; 3. Activate Variable Pitch Mode
  (variable-pitch-mode 1))

;; Automatically enable mixed pitch in Org Mode
(add-hook 'org-mode-hook #'ike/enable-mixed-pitch)

;;; --- GLOBAL DEFAULT FONT ---
;; Safely set the global UI font to my preference Jost (or the first available fallback)
(ike/set-font-face 'default ike/variable-pitch-fonts)
;;; --- GLOBAL DEFAULT FONT ---
;; Set UI font to my preference Jost if installed (or the first available fallback)
(ike/set-font-face 'default ike/fixed-pitch-fonts)
(provide 'ikemacs-ui)
