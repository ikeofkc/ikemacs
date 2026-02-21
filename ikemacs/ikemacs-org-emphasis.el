;;; ikemacs-org-emphasis.el --- Custom Org Emphasis Toggles -*- lexical-binding: t -*-
;; REQUIRES EMACS 29.1+ for keymap-set syntax

;; 1. GLOBAL SETUP (Must run immediately)
;; Ensure C-i is treated differently than TAB where possible
(keymap-set input-decode-map "C-i" "C-<i>")

;; 2. LAZY LOADING (Everything else waits for Org)
(with-eval-after-load 'org
  (require 'org-element)
  (require 'transient)

  ;;; ---------- helpers ----------
  (defun ike/org--bounds-of-word-or-region ()
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((bounds-of-thing-at-point 'symbol))
     ((bounds-of-thing-at-point 'word))))

  (defun ike/org--emphasis-type-for-marker (marker)
    (pcase marker
      (?* 'bold)
      (?/ 'italic)
      (?_ 'underline)
      (?+ 'strike-through)
      (?~ 'code)
      (?= 'verbatim)
      (_  nil)))

  (defun ike/org--inside-emphasis-of-type-p (type pt)
    (save-excursion
      (goto-char pt)
      (let ((elem (org-element-lineage (org-element-context)
                                       '(bold italic underline strike-through code verbatim) t)))
        (when (and elem (eq (org-element-type elem) type))
          elem))))

  ;;; ---------- core toggle ----------
  (defun ike/org-toggle-emphasis (marker)
    (let* ((type (ike/org--emphasis-type-for-marker marker))
           (bounds (ike/org--bounds-of-word-or-region)))
      (unless type (user-error "Unsupported marker: %S" marker))
      (cond
       (bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (mid (+ beg (/ (- end beg) 2)))
               (elem (ike/org--inside-emphasis-of-type-p type mid)))
          (if elem
              (let* ((b (org-element-property :begin elem))
                     (ce (org-element-property :contents-end elem))
                     (marker-len 1))
                (atomic-change-group
                  (delete-region ce (+ ce marker-len))
                  (delete-region b  (+ b marker-len))))
            (save-excursion
              (goto-char beg)
              (push-mark end t t)
              (org-emphasize marker)))))
       (t
        (let ((m (char-to-string marker)))
          (insert m m)
          (backward-char 1))))))

  (defun ike/bold () (interactive) (ike/org-toggle-emphasis ?*))
  (defun ike/italicize () (interactive) (ike/org-toggle-emphasis ?/))
  (defun ike/underline () (interactive) (ike/org-toggle-emphasis ?_))

  ;;; ---------- transient popup ----------
  (defvar ike/org--preview-overlays nil)
  (defun ike/org--clear-previews ()
    (mapc #'delete-overlay ike/org--preview-overlays)
    (setq ike/org--preview-overlays nil))

  (defun ike/org-preview-emphasis (marker)
    (ike/org--clear-previews)
    (let ((bounds (ike/org--bounds-of-word-or-region)))
      (when bounds
        (let ((ov (make-overlay (car bounds) (cdr bounds))))
          (overlay-put ov 'face
                       (pcase marker
                         (?* 'bold)
                         (?/ 'italic)
                         (?_ 'underline)
                         (?+ '(:strike-through t))
                         (?~ 'org-code)
                         (?= 'org-verbatim)))
          (push ov ike/org--preview-overlays)))))

  (defun ike/org-confirm-emphasis (marker)
    (ike/org--clear-previews)
    (ike/org-toggle-emphasis marker))

  (defun ike/org-cancel-preview ()
    (interactive)
    (ike/org--clear-previews))

  (transient-define-prefix ike/org-emphasis-menu ()
    "Popup menu for Org emphasis with live preview."
    [["Text Emphasis"
      ("b" "Bold"   (lambda () (interactive) (ike/org-preview-emphasis ?*) (ike/org-confirm-emphasis ?*)))
      ("i" "Italic" (lambda () (interactive) (ike/org-preview-emphasis ?/) (ike/org-confirm-emphasis ?/)))
      ("u" "Underline" (lambda () (interactive) (ike/org-preview-emphasis ?_) (ike/org-confirm-emphasis ?_)))
      ("s" "Strike" (lambda () (interactive) (ike/org-preview-emphasis ?+) (ike/org-confirm-emphasis ?+)))
      ("c" "Code"   (lambda () (interactive) (ike/org-preview-emphasis ?~) (ike/org-confirm-emphasis ?~)))
      ("v" "Verbatim" (lambda () (interactive) (ike/org-preview-emphasis ?=) (ike/org-confirm-emphasis ?=)))]
     ["Actions"
      ("q" "Cancel preview" ike/org-cancel-preview)]])

  (defun ike/org-mode-keybindings ()
    ;; Overwrite C-u (Warning: Disables Universal Argument in Org)
    (keymap-set org-mode-map "C-u" #'ike/underline)
    
    ;; Overwrite C-b (Warning: Disables Backward Char in Org)
    (keymap-set org-mode-map "C-b" #'ike/bold)
    
    ;; Bind custom C-i event (Preserves TAB for cycling)
    (keymap-set org-mode-map "C-<i>" #'ike/italicize)
    
    ;; Bind Emphasis Menu
    (keymap-set org-mode-map "C-e" #'ike/org-emphasis-menu))

  ;; 3. SETUP HOOK (Runs after Org loads)
  (add-hook 'org-mode-hook #'ike/org-mode-keybindings))

(provide 'ikemacs-org-emphasis)
