;;; org-gadget.el --- Touch-Friendly Org-mode Toolbar & Menus -*- lexical-binding: t; -*-

;; Author: Ikemacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.1.0") (transient "0.4.0") (org "9.6"))
;; Keywords: org, tools, convenience
;; URL: https://github.com/ikemacs/org-gadget

;;; Commentary:
;;
;; A standalone Org-mode enhancement providing:
;;  - A horizontal icon toolbar that auto-shows below Org file buffers
;;  - Touch-friendly transient menus for TODO, priority, agenda, sorting,
;;    emphasis, dates/recurring, links, blocks, and archiving
;;  - Smart interaction: tap/click (primary), long-press 300ms (secondary),
;;    right-click (secondary/menu)
;;  - Theme-adaptive icon colors pulled from semantic Emacs faces
;;  - Data-driven icon groups that are trivially reorderable
;;
;; Quick start:
;;   (require 'org-gadget)
;;   (org-gadget-mode 1)
;;
;; Reorder toolbar groups:
;;   (setq org-gadget-group-order '(content productivity organize))

;;; Code:

;; ═══════════════════════════════════════════════════════
;; DEPENDENCIES
;; ═══════════════════════════════════════════════════════

(require 'nerd-icons)
(require 'transient)
(require 'org)
(require 'org-element)

;; ═══════════════════════════════════════════════════════
;; CUSTOMIZATION
;; ═══════════════════════════════════════════════════════

(defgroup org-gadget nil
  "Touch-friendly Org-mode toolbar and transient menus."
  :group 'org
  :prefix "org-gadget-")

(defcustom org-gadget-group-order '(productivity organize content meta)
  "Display order of toolbar icon groups.
Rearrange to reorder, remove a symbol to hide that group.
Available: `productivity', `organize', `content', `meta'."
  :type '(repeat symbol)
  :group 'org-gadget)

(defcustom org-gadget-group-faces
  '((productivity . font-lock-keyword-face)
    (organize     . font-lock-function-name-face)
    (content      . font-lock-string-face)
    (meta         . font-lock-comment-face))
  "Alist mapping group symbols to the face for their icon color.
The toolbar adapts to any Emacs theme automatically."
  :type '(alist :key-type symbol :value-type face)
  :group 'org-gadget)

(defcustom org-gadget-auto-show t
  "When non-nil, auto show/hide toolbar based on major mode."
  :type 'boolean
  :group 'org-gadget)

;; ═══════════════════════════════════════════════════════
;; INTERNAL STATE
;; ═══════════════════════════════════════════════════════

(defvar org-gadget-buffer-name " *Org Gadget Toolbar*")
(defvar org-gadget--window nil "Window displaying the toolbar.")
(defvar org-gadget--last-org-window nil "Last window visiting an Org file.")

;; Long-press state
(defvar org-gadget--press-fired nil
  "Set to t when a long-press fires, to suppress the subsequent mouse-1.")

;; Emphasis preview
(defvar org-gadget--preview-overlays nil)

;; Arrow collapse state
(defvar org-gadget--arrows-expanded nil
  "When nil, collapsible arrow icons are hidden from the toolbar.")

;; ═══════════════════════════════════════════════════════
;; WINDOW TRACKING  (mirrors the working sidebar pattern)
;; ═══════════════════════════════════════════════════════

(defun org-gadget--record-org-window ()
  "Track the last window the user interacted with (excluding toolbar/minibuffer)."
  (let ((win (selected-window)))
    (when (and (window-live-p win)
               (not (window-minibuffer-p win))
               (not (window-dedicated-p win))
               (not (string= (buffer-name (window-buffer win))
                              org-gadget-buffer-name)))
      (setq org-gadget--last-org-window win))))

(defun org-gadget--select-main-window ()
  "Focus the last known editable window.
Uses the tracked window first, falls back to any non-dedicated window."
  (cond
   ((and org-gadget--last-org-window
         (window-live-p org-gadget--last-org-window)
         (not (string= (buffer-name (window-buffer org-gadget--last-org-window))
                        org-gadget-buffer-name)))
    (select-window org-gadget--last-org-window))
   (t
    (let ((win (get-window-with-predicate
                (lambda (w)
                  (and (not (window-dedicated-p w))
                       (not (window-minibuffer-p w))
                       (not (string= (buffer-name (window-buffer w))
                                     org-gadget-buffer-name)))))))
      (when (and win (window-live-p win))
        (select-window win))))))

;; ═══════════════════════════════════════════════════════
;; DEFERRED ACTION RUNNER
;; ═══════════════════════════════════════════════════════
;;
;; ALL toolbar actions MUST go through this.
;; Without the 0.05s delay, the mouse event is still being processed
;; when we try to switch windows, causing actions to silently fail.
;; This is the same pattern that makes the sidebar reliable.

(defun org-gadget--make-callback (fn)
  "Create a deferred callback that runs FN in the main Org window.
Returns an interactive lambda suitable for keymap bindings."
  (lambda (&rest _)
    (interactive)
    (run-with-timer
     0.05 nil
     (lambda ()
       (org-gadget--select-main-window)
       (if (commandp fn)
           (call-interactively fn)
         (funcall fn))))))

(defun org-gadget--make-lp-callback (fn)
  "Create a long-press callback that runs FN after exiting the touch handler.
Uses zero-delay timer so Emacs finishes processing the touch event
before we switch windows and open a transient menu."
  (lambda (&rest _)
    (interactive)
    (run-with-timer
     0 nil
     (lambda ()
       (org-gadget--select-main-window)
       (condition-case err
           (if (commandp fn)
               (call-interactively fn)
             (funcall fn))
         (error (message "org-gadget long-press: %S" err)))))))

;; ═══════════════════════════════════════════════════════
;; ICON HELPER
;; ═══════════════════════════════════════════════════════

(defun org-gadget--icon (name face)
  "Render nerd icon NAME colored by FACE foreground."
  (let* ((color (face-foreground (or face 'default) nil t))
         (icon-face `(:foreground ,color))
         (fn (cond
              ((string-prefix-p "nf-fa-"  name) #'nerd-icons-faicon)
              ((string-prefix-p "nf-cod-" name) #'nerd-icons-codicon)
              ((string-prefix-p "nf-oct-" name) #'nerd-icons-octicon)
              (t                                #'nerd-icons-mdicon))))
    (funcall fn name :face icon-face :height 1.2 :v-adjust 0.0)))

;; ═══════════════════════════════════════════════════════
;; EMPHASIS ENGINE
;; ═══════════════════════════════════════════════════════

(defun org-gadget--bounds-of-word-or-region ()
  "Return (BEG . END) of active region, symbol, or word at point."
  (cond
   ((use-region-p) (cons (region-beginning) (region-end)))
   ((bounds-of-thing-at-point 'symbol))
   ((bounds-of-thing-at-point 'word))))

(defun org-gadget--emphasis-type-for-marker (marker)
  "Map emphasis MARKER char to its Org element type symbol."
  (pcase marker
    (?* 'bold)  (?/ 'italic)  (?_ 'underline)
    (?+ 'strike-through)  (?~ 'code)  (?= 'verbatim)
    (_  nil)))

(defun org-gadget--inside-emphasis-p (type pt)
  "Return emphasis element of TYPE surrounding PT, or nil."
  (save-excursion
    (goto-char pt)
    (let ((elem (org-element-lineage (org-element-context)
                                     '(bold italic underline strike-through
                                       code verbatim)
                                     t)))
      (when (and elem (eq (org-element-type elem) type))
        elem))))

(defun org-gadget-emphasis-toggle (marker)
  "Toggle Org emphasis MARKER on region or word at point."
  (let* ((type (org-gadget--emphasis-type-for-marker marker))
         (bounds (org-gadget--bounds-of-word-or-region)))
    (unless type (user-error "Unsupported marker: %S" marker))
    (cond
     (bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (mid (+ beg (/ (- end beg) 2)))
             (elem (org-gadget--inside-emphasis-p type mid)))
        (if elem
            (let ((b  (org-element-property :begin elem))
                  (ce (org-element-property :contents-end elem)))
              (atomic-change-group
                (delete-region ce (+ ce 1))
                (delete-region b  (+ b  1))))
          (save-excursion
            (goto-char beg)
            (push-mark end t t)
            (org-emphasize marker)))))
     (t
      (let ((m (char-to-string marker)))
        (insert m m)
        (backward-char 1))))))

(defun org-gadget-bold ()      "Toggle bold."      (interactive) (org-gadget-emphasis-toggle ?*))
(defun org-gadget-italicize () "Toggle italic."    (interactive) (org-gadget-emphasis-toggle ?/))
(defun org-gadget-underline () "Toggle underline." (interactive) (org-gadget-emphasis-toggle ?_))

(defun org-gadget--clear-previews ()
  "Remove all emphasis preview overlays."
  (mapc #'delete-overlay org-gadget--preview-overlays)
  (setq org-gadget--preview-overlays nil))

(defun org-gadget--emphasis-apply (marker)
  "Clear previews and apply emphasis MARKER."
  (org-gadget--clear-previews)
  (org-gadget-emphasis-toggle marker))

;; ═══════════════════════════════════════════════════════
;; HELPER COMMANDS
;; ═══════════════════════════════════════════════════════

(defun org-gadget-priority-smart-cycle ()
  "Cycle priority: unset→A→B→C→unset."
  (interactive)
  (let ((current (org-entry-get nil "PRIORITY")))
    (if (or (null current)
            (string= current (char-to-string org-priority-lowest))
            (string= current ""))
        (org-priority ?A)
      (org-priority 'down))))

(defun org-gadget-agenda-combo ()
  "Show combined Agenda + all TODOs view."
  (interactive)
  (org-agenda nil "n"))

;; ═══════════════════════════════════════════════════════
;; FOLD / UNFOLD HELPERS
;; ═══════════════════════════════════════════════════════
;;
;; We cannot use `execute-kbd-macro (kbd "TAB")` because this config
;; remaps C-i (same keycode as TAB) to italicize via input-decode-map.
;; We also cannot just call `org-cycle' from a timer because it relies
;; on `last-command' / `this-command' to track its fold state.
;;
;; Solution: detect the ACTUAL visibility state of the heading each
;; time and decide the correct next action.  No stale state variables.

(defun org-gadget--heading-has-children-p ()
  "Return non-nil if heading at point has child headings."
  (save-excursion
    (org-back-to-heading t)
    (org-goto-first-child)))

(defun org-gadget--heading-has-body-p ()
  "Return non-nil if heading at point has body text (not just sub-headings)."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading-end (line-end-position))
          (subtree-end (save-excursion (org-end-of-subtree t) (point)))
          (child-start (save-excursion
                         (if (org-goto-first-child)
                             (line-beginning-position)
                           nil))))
      ;; There's body if there is content between heading line and
      ;; either the first child or the end of subtree
      (let ((body-end (or child-start subtree-end)))
        (> body-end (1+ heading-end))))))

(defun org-gadget--subtree-is-leaf-p ()
  "Return non-nil if heading at point has no content below it at all."
  (save-excursion
    (org-back-to-heading t)
    (= (line-end-position)
       (save-excursion (org-end-of-subtree t) (point)))))

(defun org-gadget--heading-folded-p ()
  "Return non-nil if any content under heading at point is invisible."
  (save-excursion
    (org-back-to-heading t)
    (let ((eol (line-end-position))
          (end (save-excursion (org-end-of-subtree t) (point))))
      (when (< eol end)
        ;; Check if the character right after the heading line is invisible
        (org-invisible-p (1+ eol))))))

(defun org-gadget--heading-fully-visible-p ()
  "Return non-nil if ALL content under heading at point is visible."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point)))
          (pos (1+ (line-end-position)))
          (all-visible t))
      (while (and all-visible (< pos end))
        (when (org-invisible-p pos)
          (setq all-visible nil))
        ;; Jump to next change in visibility to avoid checking every char
        (setq pos (next-single-char-property-change pos 'invisible nil end)))
      all-visible)))

(defun org-gadget-fold-cycle ()
  "Smart visibility cycling for the heading at point.
Detects current state and picks the right next action:
  FOLDED  → CHILDREN (if heading has children, else → SUBTREE)
  CHILDREN → SUBTREE
  SUBTREE  → FOLDED
Leaf headings (no content at all) are skipped with a message."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (cond
       ;; Leaf heading: nothing to fold/unfold
       ((org-gadget--subtree-is-leaf-p)
        (message "Nothing to fold (leaf heading)"))
       ;; Currently folded → unfold
       ((org-gadget--heading-folded-p)
        (if (org-gadget--heading-has-children-p)
            ;; Has children: show entry + children (one level)
            (progn
              (org-show-entry)
              (org-show-children)
              (message "CHILDREN"))
          ;; No children, just body text: show everything
          (progn
            (org-show-subtree)
            (message "SUBTREE"))))
       ;; Currently fully visible → fold
       ((org-gadget--heading-fully-visible-p)
        (outline-hide-subtree)
        (message "FOLDED"))
       ;; Partially visible (children shown, subtree not) → show all
       (t
        (org-show-subtree)
        (message "SUBTREE"))))))

(defvar org-gadget--global-fold-state nil
  "Global fold state: nil → 'overview → 'contents → 'show-all.")

(defun org-gadget-fold-cycle-global ()
  "Cycle global visibility: overview → contents → show all.
Works reliably from toolbar callbacks without depending on
`last-command' or the S-TAB keybinding."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (pcase org-gadget--global-fold-state
      ((or 'nil 'show-all)
       (org-overview)
       (setq org-gadget--global-fold-state 'overview)
       (message "OVERVIEW"))
      ('overview
       (org-content)
       (setq org-gadget--global-fold-state 'contents)
       (message "CONTENTS"))
      ('contents
       (org-show-all)
       (setq org-gadget--global-fold-state 'show-all)
       (message "SHOW ALL")))))

;; ═══════════════════════════════════════════════════════
;; SORT HELPER
;; ═══════════════════════════════════════════════════════

(defun org-gadget--sort (key)
  "Sort by KEY, respecting the Reverse flag."
  (let* ((args (transient-args 'org-gadget-sort-menu))
         (reverse (member "-r" args))
         (final-key (if reverse (upcase key) (downcase key))))
    (cond
     ((org-at-item-p)    (org-sort-list nil final-key))
     ((org-at-heading-p) (org-sort-entries nil final-key))
     (t (user-error "Place point on a heading or list item to sort")))))

;; ═══════════════════════════════════════════════════════
;; RECURRING EVENT HELPERS
;; ═══════════════════════════════════════════════════════

(defun org-gadget-schedule-repeating ()
  "Schedule with a repeating interval."
  (interactive)
  (let* ((choices '(("Every day (+1d)"     . "+1d")
                    ("Every week (+1w)"    . "+1w")
                    ("Every 2 weeks (+2w)" . "+2w")
                    ("Every month (+1m)"   . "+1m")
                    ("Every year (+1y)"    . "+1y")
                    ("Custom interval..."  . custom)))
         (choice (completing-read "Repeat every: " (mapcar #'car choices) nil t))
         (repeater (cdr (assoc choice choices))))
    (when (eq repeater 'custom)
      (let ((n (read-string "Every N: " "1"))
            (unit (completing-read "Unit: "
                                   '("d (days)" "w (weeks)" "m (months)" "y (years)")
                                   nil t)))
        (setq repeater (format "+%s%s" n (substring unit 0 1)))))
    (org-schedule nil)
    (save-excursion
      (org-back-to-heading t)
      (when (re-search-forward
             "SCHEDULED: <\\([^>]+\\)>" (line-end-position 3) t)
        (goto-char (match-end 1))
        (unless (string-match-p "\\+[0-9]" (match-string 1))
          (insert " " repeater))))))

(defun org-gadget-recurring-weekday ()
  "Build Nth-weekday schedule via diary-float sexp.
E.g. 'First Friday of every month' → <%%(diary-float t 5 1)>"
  (interactive)
  (let* ((occurrences '(("1st"  . 1) ("2nd" . 2) ("3rd" . 3)
                        ("4th"  . 4) ("Last" . -1)))
         (days '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2)
                 ("Wednesday" . 3) ("Thursday" . 4) ("Friday" . 5)
                 ("Saturday" . 6)))
         (occ-name (completing-read "Which occurrence? "
                                    (mapcar #'car occurrences) nil t))
         (occ-num  (cdr (assoc occ-name occurrences)))
         (day-name (completing-read "Which day? "
                                    (mapcar #'car days) nil t))
         (day-num  (cdr (assoc day-name days)))
         (sexp     (format "%%%%(diary-float t %d %d)" day-num occ-num)))
    (save-excursion
      (org-back-to-heading t)
      (org-end-of-meta-data t)
      (insert (format "<%s>\n" sexp)))
    (message "Recurring: %s %s of every month" occ-name day-name)))

;; ═══════════════════════════════════════════════════════
;; TRANSIENT MENUS  (max 2–3 columns for phone screens)
;; ═══════════════════════════════════════════════════════

(transient-define-prefix org-gadget-todo-menu ()
  "Set Org TODO states."
  [["Minimal"
    ("1" "OPEN"    (lambda () (interactive) (org-todo "OPEN")))
    ("2" "ACTIVE"  (lambda () (interactive) (org-todo "ACTIVE")))
    ("3" "CLOSED"  (lambda () (interactive) (org-todo "CLOSED")))
    ""
    (">" "Tag NEXT"    (lambda () (interactive) (org-toggle-tag "NEXT")))
    ("b" "Tag BLOCKED" (lambda () (interactive) (org-toggle-tag "BLOCKED")))
    ("x" "Remove State" (lambda () (interactive) (org-todo 'none)))]
   ["Traditional"
    ("t" "TODO"      (lambda () (interactive) (org-todo "TODO")))
    ("n" "NEXT"      (lambda () (interactive) (org-todo "NEXT")))
    ("s" "STARTED"   (lambda () (interactive) (org-todo "STARTED")))
    ("d" "DONE"      (lambda () (interactive) (org-todo "DONE")))
    ("w" "WAITING"   (lambda () (interactive) (org-todo "WAITING")))
    ("c" "CANCELLED" (lambda () (interactive) (org-todo "CANCELLED")))]]
  [("q" "Dismiss" transient-quit-one)])

(transient-define-prefix org-gadget-priority-menu ()
  "Set Org priorities."
  [["Set Priority"
    ("a" "A (High)"   (lambda () (interactive) (org-priority ?A)))
    ("b" "B (Medium)" (lambda () (interactive) (org-priority ?B)))
    ("c" "C (Low)"    (lambda () (interactive) (org-priority ?C)))]
   ["Adjust"
    ("+" "Increase" (lambda () (interactive) (org-priority 'up)))
    ("-" "Decrease" (lambda () (interactive) (org-priority 'down)))
    ("x" "Remove"   (lambda () (interactive) (org-priority ?\s)))]]
  [("q" "Dismiss" transient-quit-one)])

(transient-define-prefix org-gadget-agenda-menu ()
  "Org Agenda dispatcher."
  [["Views"
    ("d" "Day"    (lambda () (interactive) (org-agenda-list nil nil 'day)))
    ("w" "Week"   (lambda () (interactive) (org-agenda-list nil nil 'week)))
    ("m" "Month"  (lambda () (interactive) (org-agenda-list nil nil 'month)))]
   ["Tasks"
    ("t" "Global TODOs"     (lambda () (interactive) (org-agenda nil "t")))
    ("T" "Specific keyword" (lambda () (interactive) (org-agenda nil "T")))
    ("<" "Buffer only"      (lambda () (interactive) (org-agenda '(4) "a")))]]
  [["Search"
    ("#" "Tags/Properties" (lambda () (interactive) (org-agenda nil "m")))
    ("!" "Match TODOs"     (lambda () (interactive) (org-agenda nil "M")))
    ("s" "Full-text"       (lambda () (interactive) (org-agenda nil "s")))]
   ["More"
    ("n" "Agenda + TODOs"  org-gadget-agenda-combo)
    ("*" "Native Dispatch" org-agenda)]]
  [("q" "Dismiss" transient-quit-one)])

(transient-define-prefix org-gadget-sort-menu ()
  "Sort Org entries or list items."
  [:description
   (lambda ()
     (format "Sorting: %s"
             (cond ((org-at-item-p)    (propertize "List Items" 'face 'font-lock-variable-name-face))
                   ((org-at-heading-p) (propertize "Subtree"    'face 'font-lock-function-name-face))
                   (t                  (propertize "Nothing!"   'face 'error)))))]
  ["Direction"
   ("-r" "Reverse" "-r")]
  [["Basic"
    ("a" "Alphabetical" (lambda () (interactive) (org-gadget--sort ?a)))
    ("n" "Numeric"      (lambda () (interactive) (org-gadget--sort ?n)))
    ("t" "Time/Date"    (lambda () (interactive) (org-gadget--sort ?t)))]
   ["Status"
    ("k" "TODO keyword" (lambda () (interactive) (org-gadget--sort ?k)))
    ("d" "Deadline"     (lambda () (interactive) (org-gadget--sort ?d)))
    ("s" "Scheduled"    (lambda () (interactive) (org-gadget--sort ?s)))]]
  [["Metadata"
    ("p" "Priority" (lambda () (interactive) (org-gadget--sort ?p)))
    ("c" "Created"  (lambda () (interactive) (org-gadget--sort ?c)))]
   ["Advanced"
    ("r" "Property" (lambda () (interactive) (org-gadget--sort ?r)))
    ("f" "Function" (lambda () (interactive) (org-gadget--sort ?f)))]]
  [("q" "Dismiss" transient-quit-one)]
  (interactive)
  (if (or (org-at-item-p) (org-at-heading-p))
      (transient-setup 'org-gadget-sort-menu)
    (user-error "Place point on a heading or list item to sort")))

;; ═══════════════════════════════════════════════════════
;; ARROW TOGGLE
;; ═══════════════════════════════════════════════════════

(defun org-gadget-toggle-arrows ()
  "Toggle visibility of the collapsible arrow icons and re-render."
  (interactive)
  (setq org-gadget--arrows-expanded (not org-gadget--arrows-expanded))
  (when (and org-gadget--window (window-live-p org-gadget--window))
    (with-selected-window org-gadget--window
      (org-gadget--render))))

(transient-define-prefix org-gadget-emphasis-menu ()
  "Apply Org emphasis formatting."
  [["Format"
    ("b" "Bold"      (lambda () (interactive) (org-gadget--emphasis-apply ?*)))
    ("i" "Italic"    (lambda () (interactive) (org-gadget--emphasis-apply ?/)))
    ("u" "Underline" (lambda () (interactive) (org-gadget--emphasis-apply ?_)))]
   ["Extra"
    ("s" "Strike"    (lambda () (interactive) (org-gadget--emphasis-apply ?+)))
    ("c" "Code"      (lambda () (interactive) (org-gadget--emphasis-apply ?~)))
    ("v" "Verbatim"  (lambda () (interactive) (org-gadget--emphasis-apply ?=)))]]
  [("q" "Dismiss" transient-quit-one)])

(transient-define-prefix org-gadget-block-menu ()
  "Insert Org structure blocks."
  [["Common"
    ("s" "Source (code)" (lambda () (interactive) (org-insert-structure-template "src")))
    ("q" "Quote"         (lambda () (interactive) (org-insert-structure-template "quote")))
    ("e" "Example"       (lambda () (interactive) (org-insert-structure-template "example")))]
   ["Formatting"
    ("c" "Center"   (lambda () (interactive) (org-insert-structure-template "center")))
    ("v" "Verse"    (lambda () (interactive) (org-insert-structure-template "verse")))
    ("n" "Note"     (lambda () (interactive) (org-insert-structure-template "note")))]]
  [("Q" "Dismiss" transient-quit-one)])

(transient-define-prefix org-gadget-date-menu ()
  "Schedule, deadlines, and recurring events."
  [["Dates"
    ("s" "Schedule"       org-schedule)
    ("d" "Deadline"       org-deadline)
    ("t" "Timestamp"      org-time-stamp)
    ("T" "Inactive Stamp" org-time-stamp-inactive)]
   ["Recurring"
    ("r" "Repeating..."   org-gadget-schedule-repeating)
    ("f" "Nth Weekday..." org-gadget-recurring-weekday)]]
  [("q" "Dismiss" transient-quit-one)])

(transient-define-prefix org-gadget-archive-menu ()
  "Confirm before archiving."
  [:description
   (lambda ()
     (let ((heading (org-get-heading t t t t)))
       (format "Archive: %s"
               (propertize (truncate-string-to-width (or heading "?") 40 nil nil "…")
                           'face 'font-lock-warning-face))))]
  [["Confirm"
    ("y" "Yes, archive subtree" org-archive-subtree)
    ("r" "Refile instead"       org-refile)
    ("q" "Dismiss"               transient-quit-one)]])

(transient-define-prefix org-gadget-link-menu ()
  "Insert and manage Org links."
  [["Insert"
    ("l" "Insert Link"  org-insert-link)
    ("s" "Store Link"   org-store-link)]
   ["Navigate"
    ("n" "Next Link"     org-next-link)
    ("p" "Previous Link" org-previous-link)
    ("o" "Open at Point" org-open-at-point)]]
  [("q" "Dismiss" transient-quit-one)])

;; --- Transient drag-to-select fix ---
;; When a user long-presses an icon, the transient menu opens.
;; If they drag their finger/mouse to a transient item and release,
;; Emacs sees a drag-mouse-1 event which transient doesn't handle.
;; This advice converts drags into clean clicks at the release point.

(defun org-gadget--transient-drag-fix (orig-fn &rest args)
  "If transient-push-button receives a drag, convert it to a click at the release point."
  (if (eq (car-safe last-command-event) 'drag-mouse-1)
      (let* ((end-pos (event-end last-command-event))
             (click-event (list 'mouse-1 end-pos)))
        (setq unread-command-events
              (cons click-event unread-command-events)))
    (apply orig-fn args)))

(with-eval-after-load 'transient
  (advice-add 'transient-push-button :around #'org-gadget--transient-drag-fix)
  (keymap-set transient-base-map "<drag-mouse-1>" #'transient-push-button)

  ;; --- Hover highlight on transient buttons ---
  ;; After transient renders, walk the buffer and add mouse-face
  ;; to all buttons so they visually highlight on hover/touch.
  ;; Uses a short delay to ensure buttons are fully rendered.
  (defun org-gadget--transient-add-hover (&rest _)
    "Add mouse-face highlight to all buttons in the transient buffer."
    (run-with-timer
     0.05 nil
     (lambda ()
       (let ((buf (get-buffer " *transient*")))
         (when buf
           (with-current-buffer buf
             (save-excursion
               (let ((inhibit-read-only t))
                 (goto-char (point-min))
                 (while (not (eobp))
                   (let ((btn (button-at (point))))
                     (if btn
                         (progn
                           (put-text-property (button-start btn) (button-end btn)
                                              'mouse-face 'highlight)
                           (goto-char (button-end btn)))
                       (forward-char 1))))))))))))

  (advice-add 'transient--show :after #'org-gadget--transient-add-hover)
  (add-hook 'transient-exit-hook
            (lambda () (setq org-gadget--press-fired nil))))

;; ═══════════════════════════════════════════════════════
;; TOOLBAR ICON INSERTION
;; ═══════════════════════════════════════════════════════

(defun org-gadget--insert-icon (icon-str data)
  "Insert ICON-STR with interaction keybindings baked into closures.
DATA is an icon plist. No text-property lookups at event time."
  (let* ((inhibit-read-only t)
         (map       (make-sparse-keymap))
         (click-fn  (plist-get data :click))
         (lp-fn     (plist-get data :long-press))
         (menu-fn   (plist-get data :menu))
         (tooltip   (concat (or (plist-get data :tooltip) "Action")
                            (cond
                             ((and lp-fn (not (eq lp-fn click-fn)))
                              "\n  Hold or right-click for more")
                             (menu-fn "\n  Right-click for menu")
                             (t ""))))
         (click-cb  (when click-fn (org-gadget--make-callback click-fn)))
         (menu-cb   (when menu-fn  (org-gadget--make-callback menu-fn)))
         (lp-cb     (when lp-fn    (org-gadget--make-lp-callback lp-fn))))

    ;; --- LEFT CLICK (Touch release) ---
    (if lp-fn
        (define-key map [mouse-1]
          (lambda (&rest _)
            (interactive)
            (if org-gadget--press-fired
                (setq org-gadget--press-fired nil) ; Reset after long press
              (when click-cb (funcall click-cb)))))
      (define-key map [mouse-1] (or click-cb #'ignore)))

    ;; --- RIGHT CLICK ---
    (define-key map [down-mouse-3] #'ignore)
    (define-key map [mouse-3] (or menu-cb click-cb #'ignore))

    ;; --- KEYBOARD ---
    (define-key map [return] (or click-cb #'ignore))

    ;; --- INSERT ---
    (insert " ")
    (insert (propertize icon-str
                        'help-echo tooltip
                        'mouse-face 'highlight
                        'keymap map
                        'pointer 'hand
                        ;; Store the long-press callback for the local handler
                        'org-gadget-lp-cb lp-cb))
    (insert " ")))

;; ═══════════════════════════════════════════════════════
;; GROUP DEFINITIONS  (data-driven)
;; ═══════════════════════════════════════════════════════

(defvar org-gadget-group-definitions
  `((productivity
     . ((:icon "nf-fa-circle_check"   :tooltip "TODO State"
         :click ,#'org-todo
         :long-press ,#'org-gadget-todo-menu
         :menu ,#'org-gadget-todo-menu)

        (:icon "nf-md-tag"            :tooltip "Set Tags"
         :click ,#'org-set-tags-command
         :long-press nil  :menu nil)

        (:icon "nf-md-alert_decagram" :tooltip "Priority"
         :click ,#'org-gadget-priority-smart-cycle
         :long-press ,#'org-gadget-priority-menu
         :menu ,#'org-gadget-priority-menu)

        (:icon "nf-fa-calendar_plus"  :tooltip "Dates & Schedule"
         :click ,#'org-gadget-date-menu
         :long-press ,#'org-gadget-date-menu
         :menu ,#'org-gadget-date-menu)

        (:icon "nf-fa-calendar"       :tooltip "Agenda"
         :click ,#'org-gadget-agenda-combo
         :long-press ,#'org-gadget-agenda-menu
         :menu ,#'org-gadget-agenda-menu)))

    (organize
     . ((:icon "nf-md-arrow_all"      :tooltip "Show/Hide arrow controls"
         :click ,#'org-gadget-toggle-arrows
         :long-press nil  :menu nil)

        (:icon "nf-fa-arrow_left"     :tooltip "Promote Subtree  (hold: single level)"
         :click ,#'org-promote-subtree
         :long-press ,#'org-do-promote
         :menu ,#'org-do-promote
         :collapsible t)

        (:icon "nf-fa-arrow_right"    :tooltip "Demote Subtree  (hold: single level)"
         :click ,#'org-demote-subtree
         :long-press ,#'org-do-demote
         :menu ,#'org-do-demote
         :collapsible t)

        (:icon "nf-fa-arrow_up"       :tooltip "Move Subtree Up  (hold: single line)"
         :click ,#'org-metaup
         :long-press ,#'org-shiftmetaup
         :menu ,#'org-shiftmetaup
         :collapsible t)

        (:icon "nf-fa-arrow_down"     :tooltip "Move Subtree Down  (hold: single line)"
         :click ,#'org-metadown
         :long-press ,#'org-shiftmetadown
         :menu ,#'org-shiftmetadown
         :collapsible t)

        (:icon "nf-cod-fold"          :tooltip "Fold/Unfold  (hold: global cycle)"
         :click ,#'org-gadget-fold-cycle
         :long-press ,#'org-gadget-fold-cycle-global
         :menu ,#'org-gadget-fold-cycle-global)

        (:icon "nf-fa-sort"           :tooltip "Sort"
         :click ,#'org-gadget-sort-menu
         :long-press ,#'org-gadget-sort-menu
         :menu ,#'org-gadget-sort-menu)

        (:icon "nf-cod-archive"       :tooltip "Refile  (hold: archive)"
         :click ,#'org-refile
         :long-press ,#'org-gadget-archive-menu
         :menu ,#'org-gadget-archive-menu)))

    (content
     . ((:icon "nf-md-format_font"    :tooltip "Emphasis / Formatting"
         :click ,#'org-gadget-emphasis-menu
         :long-press ,#'org-gadget-emphasis-menu
         :menu ,#'org-gadget-emphasis-menu)

        (:icon "nf-md-link"           :tooltip "Links"
         :click ,#'org-gadget-link-menu
         :long-press ,#'org-gadget-link-menu
         :menu ,#'org-gadget-link-menu)

        (:icon "nf-md-table"          :tooltip "Insert Table"
         :click ,#'org-table-create-or-convert-from-region
         :long-press nil  :menu nil)

        (:icon "nf-md-code_tags"      :tooltip "Blocks & Templates"
         :click ,#'org-gadget-block-menu
         :long-press ,#'org-gadget-block-menu
         :menu ,#'org-gadget-block-menu)))

    (meta
     . ((:icon "nf-md-lightning_bolt"  :tooltip "Command Palette (M-x)"
         :click ,#'execute-extended-command
         :long-press nil  :menu nil))))
  "Alist of group symbol → list of icon plists.
Display order controlled by `org-gadget-group-order'.")

;; ═══════════════════════════════════════════════════════
;; TOOLBAR RENDERING
;; ═══════════════════════════════════════════════════════

(defun org-gadget--render ()
  "Draw all icon groups into the toolbar buffer."
  (with-current-buffer (get-buffer-create org-gadget-buffer-name)
    (let ((inhibit-read-only t)
          (sep-color (face-foreground 'font-lock-comment-face nil t))
          (first-group t))
      (erase-buffer)

      (dolist (group-sym org-gadget-group-order)
        (let ((icons (cdr (assq group-sym org-gadget-group-definitions)))
              (face  (cdr (assq group-sym org-gadget-group-faces))))
          (when icons
            (unless first-group
              (insert (propertize " | " 'face `(:foreground ,sep-color))))
            (setq first-group nil)
            (dolist (icon-data icons)
              (unless (and (plist-get icon-data :collapsible)
                           (not org-gadget--arrows-expanded))
                (let ((icon-str (org-gadget--icon (plist-get icon-data :icon) face)))
                  (org-gadget--insert-icon icon-str icon-data)))))))

      (goto-char (point-min))
      (read-only-mode 1)
      (setq-local truncate-lines t)
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil)
      (setq-local window-size-fixed 'height)
      
;; Intercept the native long-press to prevent "Mark set"
      (local-set-key [touchscreen-hold]
                     (lambda (event)
                       (interactive "e")
                       (let* ((posn (cadr event))
                              (pt (and posn (posn-point posn)))
                              (cb (and pt (get-text-property pt 'org-gadget-lp-cb))))
                         (when cb
                           (setq org-gadget--press-fired t)
                           (funcall cb))))))))

;; ═══════════════════════════════════════════════════════
;; SHOW / HIDE / AUTO-TOGGLE
;; ═══════════════════════════════════════════════════════

(defun org-gadget-show ()
  "Show the toolbar below the currently selected window.
Only renders on first display; subsequent calls just ensure the window exists."
  (interactive)
  (let ((buf (get-buffer-create org-gadget-buffer-name)))
    (if (and org-gadget--window (window-live-p org-gadget--window))
        ;; Already visible — just ensure properties, do NOT re-render
        (progn
          (set-window-dedicated-p org-gadget--window t)
          (set-window-parameter org-gadget--window 'no-other-window t)
          (set-window-parameter org-gadget--window 'no-delete-other-windows t))
      ;; First display — create window and render once
      (setq org-gadget--window
            (display-buffer buf
                            '((display-buffer-below-selected)
                              (window-height . 2)
                              (inhibit-same-window . t))))
      (set-window-dedicated-p org-gadget--window t)
      (set-window-parameter org-gadget--window 'no-other-window t)
      (set-window-parameter org-gadget--window 'no-delete-other-windows t)
      (with-selected-window org-gadget--window
        (org-gadget--render)))))

(defun org-gadget-hide ()
  "Hide the toolbar."
  (interactive)
  (when (and org-gadget--window (window-live-p org-gadget--window))
    (delete-window org-gadget--window)
    (setq org-gadget--window nil)))

(defun org-gadget-toggle ()
  "Toggle toolbar visibility."
  (interactive)
  (if (and org-gadget--window (window-live-p org-gadget--window))
      (org-gadget-hide)
    (org-gadget-show)))

(defun org-gadget--any-org-file-visible-p ()
  "Return non-nil if any visible window is displaying an Org file."
  (let ((found nil))
    (walk-windows
     (lambda (w)
       (let ((buf (window-buffer w)))
         (when (and (eq (buffer-local-value 'major-mode buf) 'org-mode)
                    (buffer-file-name buf)
                    (not (string= (buffer-name buf) org-gadget-buffer-name)))
           (setq found t))))
     nil t)
    found))

(defun org-gadget--auto-toggle (&optional frame-or-window)
  "Show toolbar if any Org file is visible, hide otherwise.
Works from `window-selection-change-functions' and
`window-buffer-change-functions'."
  (when org-gadget-auto-show
    (let* ((win (if (windowp frame-or-window) frame-or-window (selected-window)))
           (buf (window-buffer win)))
      ;; Always track the last usable window
      (org-gadget--record-org-window)
      ;; Skip events inside toolbar or minibuffer
      (unless (or (string= (buffer-name buf) org-gadget-buffer-name)
                  (window-minibuffer-p win))
        (if (org-gadget--any-org-file-visible-p)
            (org-gadget-show)
          (org-gadget-hide))))))

;; ═══════════════════════════════════════════════════════
;; KEYBINDINGS
;; ═══════════════════════════════════════════════════════

;; Distinguish C-i from TAB
(keymap-set input-decode-map "C-i" "C-<i>")

(defun org-gadget--setup-keybindings ()
  "Apply org-gadget keybindings to `org-mode-map'."
  (keymap-set org-mode-map "C-b"     #'org-gadget-bold)
  (keymap-set org-mode-map "C-<i>"   #'org-gadget-italicize)
  (keymap-set org-mode-map "C-u"     #'org-gadget-underline)
  (keymap-set org-mode-map "C-e"     #'org-gadget-emphasis-menu)
  (keymap-set org-mode-map "C-^"     #'org-gadget-sort-menu)
  (keymap-set org-mode-map "C-c C-t" #'org-gadget-todo-menu)
  (keymap-set org-mode-map "C-c ,"   #'org-gadget-priority-menu)
  (keymap-set org-mode-map "C-c C-," #'org-gadget-block-menu))

;; ═══════════════════════════════════════════════════════
;; MINOR MODE
;; ═══════════════════════════════════════════════════════

;;;###autoload
(define-minor-mode org-gadget-mode
  "Global minor mode for the Org-gadget toolbar and keybindings."
  :global t
  :group 'org-gadget
  (if org-gadget-mode
      (progn
        (add-hook 'window-selection-change-functions #'org-gadget--auto-toggle)
        (add-hook 'window-buffer-change-functions #'org-gadget--auto-toggle)
        (add-hook 'org-mode-hook #'org-gadget--setup-keybindings)
        (org-gadget--setup-keybindings)
        (org-gadget--auto-toggle))
    (remove-hook 'window-selection-change-functions #'org-gadget--auto-toggle)
    (remove-hook 'window-buffer-change-functions #'org-gadget--auto-toggle)
    (remove-hook 'org-mode-hook #'org-gadget--setup-keybindings)
    (org-gadget-hide)))

;; Global binding (available outside Org buffers)
(keymap-global-set "C-c a" #'org-gadget-agenda-menu)

(provide 'org-gadget)
;;; org-gadget.el ends here
