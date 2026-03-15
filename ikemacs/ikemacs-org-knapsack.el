;;; ikemacs-org-knapsack.el --- Custom Org-mode tools made for ikemacs with clickable menus (Knapsacks maybe? lol I don't know) -*- lexical-binding: t -*-
;; REQUIRES EMACS 29.1+ for keymap-set syntax

;; 1. GLOBAL SETUP (Must run immediately)
;; Ensure C-i is treated differently than TAB where possible
(keymap-set input-decode-map "C-i" "C-<i>")

;; 2. LAZY LOADING (Everything else waits for Org)
(with-eval-after-load 'org
  (require 'org-element)
  (require 'transient)

  ;; ==================================
  ;; KNAPSACK EMPHASIS FUNCTIONS
  ;; ==================================
  
  ;;; ---------- helpers ----------
  (defun org-knapsack--bounds-of-word-or-region ()
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((bounds-of-thing-at-point 'symbol))
     ((bounds-of-thing-at-point 'word))))

  (defun org-knapsack-emphasis-type-for-marker (marker)
    (pcase marker
      (?* 'bold)
      (?/ 'italic)
      (?_ 'underline)
      (?+ 'strike-through)
      (?~ 'code)
      (?= 'verbatim)
      (_  nil)))

  (defun org-knapsack-emphasis-inside-emphasis-of-type-p (type pt)
    (save-excursion
      (goto-char pt)
      (let ((elem (org-element-lineage (org-element-context)
                                       '(bold italic underline strike-through code verbatim) t)))
        (when (and elem (eq (org-element-type elem) type))
          elem))))

  ;;; ---------- core toggle ----------
  (defun org-knapsack-emphasis-toggle (marker)
    (let* ((type (org-knapsack-emphasis-type-for-marker marker))
           (bounds (org-knapsack--bounds-of-word-or-region)))
      (unless type (user-error "Unsupported marker: %S" marker))
      (cond
       (bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (mid (+ beg (/ (- end beg) 2)))
               (elem (org-knapsack-emphasis-inside-emphasis-of-type-p type mid)))
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

  (defun org-knapsack-bold () (interactive) (org-knapsack-emphasis-toggle ?*))
  (defun org-knapsack-italicize () (interactive) (org-knapsack-emphasis-toggle ?/))
  (defun org-knapsack-underline () (interactive) (org-knapsack-emphasis-toggle ?_))

  ;;; ---------- transient popup ----------
  (defvar org-knapsack--preview-overlays nil)
  (defun org-knapsack--clear-previews ()
    (mapc #'delete-overlay org-knapsack--preview-overlays)
    (setq org-knapsack--preview-overlays nil))

  (defun org-knapsack-emphasis-preview (marker)
    (org-knapsack--clear-previews)
    (let ((bounds (org-knapsack--bounds-of-word-or-region)))
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
          (push ov org-knapsack--preview-overlays)))))

  (defun org-knapsack-emphasis-confirm (marker)
    (org-knapsack--clear-previews)
    (org-knapsack-emphasis-toggle marker))

  (defun org-knapsack-emphasis-cancel-preview ()
    (interactive)
    (org-knapsack--clear-previews))

  (transient-define-prefix org-knapsack-emphasis-menu ()
    "Popup menu for Org emphasis with live preview."
    [["Format"
      ("b" "Bold"      (lambda () (interactive) (org-knapsack-emphasis-preview ?*) (org-knapsack-emphasis-confirm ?*)))
      ("i" "Italic"    (lambda () (interactive) (org-knapsack-emphasis-preview ?/) (org-knapsack-emphasis-confirm ?/)))
      ("u" "Underline" (lambda () (interactive) (org-knapsack-emphasis-preview ?_) (org-knapsack-emphasis-confirm ?_)))]
     ["Extra"
      ("s" "Strike"    (lambda () (interactive) (org-knapsack-emphasis-preview ?+) (org-knapsack-emphasis-confirm ?+)))
      ("c" "Code"      (lambda () (interactive) (org-knapsack-emphasis-preview ?~) (org-knapsack-emphasis-confirm ?~)))
      ("v" "Verbatim"  (lambda () (interactive) (org-knapsack-emphasis-preview ?=) (org-knapsack-emphasis-confirm ?=)))]]
    ["Actions"
     ("q" "Cancel preview" org-knapsack-emphasis-cancel-preview)])


  ;; ==================================
  ;; KNAPSACK SORT FUNCTIONS
  ;; ==================================
  
  (defun org-knapsack-sort (key)
    "Helper to execute the sort. 
If 'Reverse' is enabled in the transient, KEY is uppercased."
    (let* ((args (transient-args 'org-knapsack-sort-menu))
           (reverse (member "-r" args))
           ;; Org-sort uses uppercase letters to signal reverse sorting
           (final-key (if reverse (upcase key) (downcase key))))
      (message "Sorting with key: %c" final-key)
      (cond
       ((org-at-item-p)
        (org-sort-list nil final-key))
       ((org-at-heading-p)
        (org-sort-entries nil final-key))
       (t
        (user-error "Not at a list item or heading.")))))

  ;;; --- TRANSIENT MENU ---
 (transient-define-prefix org-knapsack-sort-menu ()
    "Transient menu for sorting Org entries or list items."
    [:description
     (lambda ()
       (format "Sorting: %s" 
               (cond ((org-at-item-p) (propertize "List Items" 'face 'font-lock-variable-name-face))
                     ((org-at-heading-p) (propertize "Subtree" 'face 'font-lock-function-name-face))
                     (t (propertize "Nothing found!" 'face 'error))))) ]
    
    ["Sort Direction"
     ("-r" "Reverse Sort" "-r")]
    
    ;; --- ROW 1 (Max 2 Columns) ---
    [["Basic Sort"
      ("a" "Alphabetical" (lambda () (interactive) (org-knapsack-sort ?a)))
      ("n" "Numeric"      (lambda () (interactive) (org-knapsack-sort ?n)))
      ("t" "Time/Date"    (lambda () (interactive) (org-knapsack-sort ?t)))]
     ["Status & Dates"
      ("k" "Todo Keyword" (lambda () (interactive) (org-knapsack-sort ?k)))
      ("d" "Deadline"     (lambda () (interactive) (org-knapsack-sort ?d)))
      ("s" "Scheduled"    (lambda () (interactive) (org-knapsack-sort ?s)))]]
    
    ;; --- ROW 2 (Max 2 Columns) ---
    [["Metadata"
      ("p" "Priority"     (lambda () (interactive) (org-knapsack-sort ?p)))
      ("c" "Created"      (lambda () (interactive) (org-knapsack-sort ?c)))]
     ["Advanced"
      ("r" "Property"     (lambda () (interactive) (org-knapsack-sort ?r)))
      ("f" "Function"     (lambda () (interactive) (org-knapsack-sort ?f)))]]
    
    (interactive)
    (if (or (org-at-item-p) (org-at-heading-p))
        (transient-setup 'org-knapsack-sort-menu)
      (user-error "Place point on a heading or list item to sort.")))



;; ==================================
;; KNAPSACK AGENDA MENU
;; ==================================
  
  (transient-define-prefix org-knapsack-agenda-menu ()
    "Touch-friendly Transient menu for Org Agenda dispatcher."
    ;; --- ROW 1 (Max 2 Columns) ---
    [["Agenda Views"
      ("d" "Day View"   (lambda () (interactive) (org-agenda-list nil nil 'day)))
      ("w" "Week View"  (lambda () (interactive) (org-agenda-list nil nil 'week)))
      ("m" "Month View" (lambda () (interactive) (org-agenda-list nil nil 'month)))]
     ["Tasks & TODOs"
      ("t" "Global TODO List"   (lambda () (interactive) (org-agenda nil "t")))
      ("T" "Specific TODO kwd"  (lambda () (interactive) (org-agenda nil "T")))
      ("<" "Restrict to Buffer" (lambda () (interactive) (org-agenda '(4) "a")))]]
     
    ;; --- ROW 2 (Max 2 Columns) ---
    [["Search & Match"
      ("#" "Match Tags/Props"  (lambda () (interactive) (org-agenda nil "m")))
      ("!" "Match TODOs only"  (lambda () (interactive) (org-agenda nil "M")))
      ("s" "Search Text/Kwds"  (lambda () (interactive) (org-agenda nil "s")))]
     ["Advanced"
      ("*" "Native Dispatcher" org-agenda)]])
;; ==================================
  ;; KNAPSACK TODO STATES MENU
  ;; ==================================
  
  (transient-define-prefix org-knapsack-todo-menu ()
    "Set Org TODO states without minibuffer prompts."
    ;; Note: Only ONE set of outer double-brackets means everything stays on the same row!
    [[
      ;; --- COLUMN 1 (Left Side) ---
      "Ikemacs: Minimal + tagging"
      ("1" "OPEN"        (lambda () (interactive) (org-todo "OPEN")))
      ("2" "ACTIVE"      (lambda () (interactive) (org-todo "ACTIVE")))
      ("3" "CLOSED"      (lambda () (interactive) (org-todo "CLOSED")))
      
      "Tag / remove"
      (">" "next"        (lambda () (interactive) (org-toggle-tag "NEXT")))
      ("b" "BLOCKED"     (lambda () (interactive) (org-toggle-tag "BLOCKED")))
      ("x" "Remove State" (lambda () (interactive) (org-todo 'none)))]
     
     [
      ;; --- COLUMN 2 (Right Side) ---
      "Traditional lifecycle"
      ("t" "TODO"        (lambda () (interactive) (org-todo "TODO")))
      ("n" "NEXT"        (lambda () (interactive) (org-todo "NEXT")))
      ("s" "STARTED"     (lambda () (interactive) (org-todo "STARTED")))
      ("d" "DONE"        (lambda () (interactive) (org-todo "DONE")))
      ("w" "WAITING"     (lambda () (interactive) (org-todo "WAITING")))
      ("c" "CANCELLED"   (lambda () (interactive) (org-todo "CANCELLED")))]])

  ;; ==================================
  ;; KNAPSACK PRIORITY MENU
  ;; ==================================

  (transient-define-prefix org-knapsack-priority-menu ()
    "Set Org priorities without minibuffer prompts."
    ;; --- ROW 1 ---
    [["Set Priority"
      ("a" "Priority A (High)"   (lambda () (interactive) (org-priority ?A)))
      ("b" "Priority B (Medium)" (lambda () (interactive) (org-priority ?B)))]
     [""
      ("c" "Priority C (Low)"    (lambda () (interactive) (org-priority ?C)))
      ("x" "Remove Priority"     (lambda () (interactive) (org-priority ?\s)))]]
    
    ;; --- ROW 2 ---
    [["Adjust"
      ("+" "Increase Priority"   (lambda () (interactive) (org-priority 'up)))
      ("-" "Decrease Priority"   (lambda () (interactive) (org-priority 'down)))]])

  ;; ==================================
  ;; KNAPSACK BLOCKS & TEMPLATES MENU
  ;; ==================================
  
  (transient-define-prefix org-knapsack-block-menu ()
    "Insert Org structure blocks with a single tap/click."
    ;; --- ROW 1 ---
    [["Common Blocks"
      ("s" "Code Snippet (src)" (lambda () (interactive) (org-insert-structure-template "src")))
      ("q" "Quote Block"        (lambda () (interactive) (org-insert-structure-template "quote")))]
     ["Formatting"
      ("c" "Center Text"        (lambda () (interactive) (org-insert-structure-template "center")))
      ("e" "Example / Verbatim" (lambda () (interactive) (org-insert-structure-template "example")))]]
    
    ;; --- ROW 2 ---
    [["Advanced"
      ("v" "Verse (Poetry)"     (lambda () (interactive) (org-insert-structure-template "verse")))
      ("n" "Note"               (lambda () (interactive) (org-insert-structure-template "note")))]])
  
(defun org-knapsack-keybindings ()
    "Apply all Knapsack keybindings for Org-mode."
    ;; Formatting Keybindings
    (keymap-set org-mode-map "C-u" #'org-knapsack-underline)
    (keymap-set org-mode-map "C-b" #'org-knapsack-bold)
    (keymap-set org-mode-map "C-<i>" #'org-knapsack-italicize)
    (keymap-set org-mode-map "C-e" #'org-knapsack-emphasis-menu)
    
    ;; Sort Keybindings
    (keymap-set org-mode-map "C-^" #'org-knapsack-sort-menu)
    
    ;; Touch-friendly Replacements for Native Org Prompts
    (keymap-set org-mode-map "C-c C-t" #'org-knapsack-todo-menu)     ;; Replaces TODO prompt
    (keymap-set org-mode-map "C-c ,"   #'org-knapsack-priority-menu) ;; Replaces Priority prompt
    (keymap-set org-mode-map "C-c C-," #'org-knapsack-block-menu))   ;; Replaces Block prompt

  ;; SETUP HOOK (Runs after Org loads)
  (add-hook 'org-mode-hook #'org-knapsack-keybindings))

;; ==================================
;; GLOBAL KNAPSACK KEYBINDINGS
;; ==================================
;; Placed outside the eval-after-load block so it works immediately on startup
(keymap-global-set "C-c a" #'org-knapsack-agenda-menu)

(provide 'ikemacs-org-knapsack)

