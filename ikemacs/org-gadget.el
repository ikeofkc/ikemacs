;;; org-gadget.el --- Touch-Friendly Org-mode Toolbar & Menus -*- lexical-binding: t; -*-

;; Author: Ikemacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.1.0") (org "9.6"))
;; Keywords: org, tools, convenience
;; URL: https://github.com/ikemacs/org-gadget

;;; Commentary:
;;
;; A standalone Org-mode enhancement providing:
;;  - A horizontal icon toolbar that auto-shows below Org file buffers
;;  - Native touch-friendly popup menus for TODO, priority, agenda, sorting,
;;    emphasis, dates/recurring, links, blocks, and archiving
;;  - Smart interaction: tap/click (primary), long-press (native touch),
;;    right-click (secondary/menu)
;;  - Theme-adaptive icon colors pulled from semantic Emacs faces
;;  - Data-driven icon groups that are trivially reorderable
;;
;; Quick start:
;;   (require 'org-gadget)
;;   (org-gadget-mode 1)

;;; Code:

;; ═══════════════════════════════════════════════════════
;; DEPENDENCIES
;; ═══════════════════════════════════════════════════════

(require 'nerd-icons)
(require 'org)
(require 'org-element)

;; ═══════════════════════════════════════════════════════
;; CUSTOMIZATION
;; ═══════════════════════════════════════════════════════

(defgroup org-gadget nil
  "Touch-friendly Org-mode toolbar and native popup menus."
  :group 'org
  :prefix "org-gadget-")

(defcustom org-gadget-group-order '(productivity organize content meta)
  "Display order of toolbar icon groups.
Rearrange to reorder, remove a symbol to hide that group."
  :type '(repeat symbol)
  :group 'org-gadget)

(defcustom org-gadget-group-faces
  '((productivity . font-lock-keyword-face)
    (organize     . font-lock-function-name-face)
    (content      . font-lock-string-face)
    (meta         . font-lock-comment-face))
  "Alist mapping group symbols to the face for their icon color."
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

(defvar org-gadget--press-fired nil
  "Set to t when a long-press fires, to suppress the subsequent mouse-1.")

(defvar org-gadget--preview-overlays nil)

(defvar org-gadget--arrows-expanded nil
  "When nil, collapsible arrow icons are hidden from the toolbar.")

;; ═══════════════════════════════════════════════════════
;; WINDOW TRACKING
;; ═══════════════════════════════════════════════════════

(defun org-gadget--record-org-window ()
  "Track the last window the user interacted with."
  (let ((win (selected-window)))
    (when (and (window-live-p win)
               (not (window-minibuffer-p win))
               (not (window-dedicated-p win))
               (not (string= (buffer-name (window-buffer win))
                              org-gadget-buffer-name)))
      (setq org-gadget--last-org-window win))))

(defun org-gadget--select-main-window ()
  "Focus the last known editable window."
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
;; DEFERRED ACTION RUNNERS
;; ═══════════════════════════════════════════════════════

(defun org-gadget--make-callback (fn)
  "Create a deferred callback that runs FN in the main Org window."
  (lambda (&optional event)
    (interactive "e")
    (run-with-timer
     0.05 nil
     (lambda (evt)
       (org-gadget--select-main-window)
       ;; Try passing the event so x-popup-menu knows where to draw
       (condition-case nil
           (funcall fn evt)
         ;; Fallback if the function doesn't accept coordinate arguments
         (wrong-number-of-arguments
          (if (commandp fn)
              (call-interactively fn)
            (funcall fn)))))
     event)))

(defun org-gadget--make-lp-callback (fn)
  "Create a long-press callback that dynamically handles native popup menus."
  (lambda (&optional event)
    (interactive "e")
    (run-with-timer
     0 nil
     (lambda (evt)
       (org-gadget--select-main-window)
       (condition-case err
           (condition-case nil
               (funcall fn evt)
             (wrong-number-of-arguments
              (if (commandp fn)
                  (call-interactively fn)
                (funcall fn))))
         (error (message "org-gadget menu error: %S" err))))
     event)))

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
  (cond
   ((use-region-p) (cons (region-beginning) (region-end)))
   ((bounds-of-thing-at-point 'symbol))
   ((bounds-of-thing-at-point 'word))))

(defun org-gadget--emphasis-type-for-marker (marker)
  (pcase marker
    (?* 'bold)  (?/ 'italic)  (?_ 'underline)
    (?+ 'strike-through)  (?~ 'code)  (?= 'verbatim)
    (_  nil)))

(defun org-gadget--inside-emphasis-p (type pt)
  (save-excursion
    (goto-char pt)
    (let ((elem (org-element-lineage (org-element-context)
                                     '(bold italic underline strike-through
                                       code verbatim)
                                     t)))
      (when (and elem (eq (org-element-type elem) type))
        elem))))

(defun org-gadget-emphasis-toggle (marker)
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
  (mapc #'delete-overlay org-gadget--preview-overlays)
  (setq org-gadget--preview-overlays nil))

(defun org-gadget--emphasis-apply (marker)
  (org-gadget--clear-previews)
  (org-gadget-emphasis-toggle marker))

;; ═══════════════════════════════════════════════════════
;; HELPER COMMANDS
;; ═══════════════════════════════════════════════════════

(defun org-gadget-priority-smart-cycle ()
  (interactive)
  (let ((current (org-entry-get nil "PRIORITY")))
    (if (or (null current)
            (string= current (char-to-string org-priority-lowest))
            (string= current ""))
        (org-priority ?A)
      (org-priority 'down))))

(defun org-gadget-agenda-combo ()
  (interactive)
  (org-agenda nil "n"))

;; ═══════════════════════════════════════════════════════
;; FOLD / UNFOLD HELPERS
;; ═══════════════════════════════════════════════════════

(defun org-gadget--heading-has-children-p ()
  (save-excursion
    (org-back-to-heading t)
    (org-goto-first-child)))

(defun org-gadget--heading-has-body-p ()
  (save-excursion
    (org-back-to-heading t)
    (let ((heading-end (line-end-position))
          (subtree-end (save-excursion (org-end-of-subtree t) (point)))
          (child-start (save-excursion
                         (if (org-goto-first-child)
                             (line-beginning-position)
                           nil))))
      (let ((body-end (or child-start subtree-end)))
        (> body-end (1+ heading-end))))))

(defun org-gadget--subtree-is-leaf-p ()
  (save-excursion
    (org-back-to-heading t)
    (= (line-end-position)
       (save-excursion (org-end-of-subtree t) (point)))))

(defun org-gadget--heading-folded-p ()
  (save-excursion
    (org-back-to-heading t)
    (let ((eol (line-end-position))
          (end (save-excursion (org-end-of-subtree t) (point))))
      (when (< eol end)
        (org-invisible-p (1+ eol))))))

(defun org-gadget--heading-fully-visible-p ()
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point)))
          (pos (1+ (line-end-position)))
          (all-visible t))
      (while (and all-visible (< pos end))
        (when (org-invisible-p pos)
          (setq all-visible nil))
        (setq pos (next-single-char-property-change pos 'invisible nil end)))
      all-visible)))

(defun org-gadget-fold-cycle ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (cond
       ((org-gadget--subtree-is-leaf-p)
        (message "Nothing to fold (leaf heading)"))
       ((org-gadget--heading-folded-p)
        (if (org-gadget--heading-has-children-p)
            (progn (org-show-entry) (org-show-children) (message "CHILDREN"))
          (progn (org-show-subtree) (message "SUBTREE"))))
       ((org-gadget--heading-fully-visible-p)
        (outline-hide-subtree) (message "FOLDED"))
       (t
        (org-show-subtree) (message "SUBTREE"))))))

(defvar org-gadget--global-fold-state nil)

(defun org-gadget-fold-cycle-global ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (pcase org-gadget--global-fold-state
      ((or 'nil 'show-all)
       (org-overview) (setq org-gadget--global-fold-state 'overview) (message "OVERVIEW"))
      ('overview
       (org-content) (setq org-gadget--global-fold-state 'contents) (message "CONTENTS"))
      ('contents
       (org-show-all) (setq org-gadget--global-fold-state 'show-all) (message "SHOW ALL")))))

(defun org-gadget-toggle-arrows ()
  (interactive)
  (setq org-gadget--arrows-expanded (not org-gadget--arrows-expanded))
  (when (and org-gadget--window (window-live-p org-gadget--window))
    (with-selected-window org-gadget--window
      (org-gadget--render))))

;; ═══════════════════════════════════════════════════════
;; SORT / RECURRING EVENT HELPERS
;; ═══════════════════════════════════════════════════════

(defun org-gadget--sort (key)
  (cond
   ((org-at-item-p)    (org-sort-list nil key))
   ((org-at-heading-p) (org-sort-entries nil key))
   (t (user-error "Place point on a heading or list item to sort"))))

(defun org-gadget-schedule-repeating ()
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
            (unit (completing-read "Unit: " '("d (days)" "w (weeks)" "m (months)" "y (years)") nil t)))
        (setq repeater (format "+%s%s" n (substring unit 0 1)))))
    (org-schedule nil)
    (save-excursion
      (org-back-to-heading t)
      (when (re-search-forward "SCHEDULED: <\\([^>]+\\)>" (line-end-position 3) t)
        (goto-char (match-end 1))
        (unless (string-match-p "\\+[0-9]" (match-string 1))
          (insert " " repeater))))))

(defun org-gadget-recurring-weekday ()
  (interactive)
  (let* ((occurrences '(("1st" . 1) ("2nd" . 2) ("3rd" . 3) ("4th" . 4) ("Last" . -1)))
         (days '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3) 
                 ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)))
         (occ-name (completing-read "Which occurrence? " (mapcar #'car occurrences) nil t))
         (occ-num  (cdr (assoc occ-name occurrences)))
         (day-name (completing-read "Which day? " (mapcar #'car days) nil t))
         (day-num  (cdr (assoc day-name days)))
         (sexp     (format "%%%%(diary-float t %d %d)" day-num occ-num)))
    (save-excursion
      (org-back-to-heading t)
      (org-end-of-meta-data t)
      (insert (format "<%s>\n" sexp)))
    (message "Recurring: %s %s of every month" occ-name day-name)))

;; ═══════════════════════════════════════════════════════
;; NATIVE POPUP MENUS  (x-popup-menu engine)
;; ═══════════════════════════════════════════════════════

(defun org-gadget--popup (event title &rest panes)
  "Show a native popup menu. Executes the chosen ACTION automatically."
  (let ((choice (x-popup-menu (or event t) (cons title panes))))
    (when choice
      (if (commandp choice)
          (call-interactively choice)
        (funcall choice)))))

(defun org-gadget-todo-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Set TODO State"
    '("Minimal"
      ("OPEN"         . (lambda () (org-todo "OPEN")))
      ("ACTIVE"       . (lambda () (org-todo "ACTIVE")))
      ("CLOSED"       . (lambda () (org-todo "CLOSED")))
      (""             . nil)
      ("Tag NEXT"     . (lambda () (org-toggle-tag "NEXT")))
      ("Tag BLOCKED"  . (lambda () (org-toggle-tag "BLOCKED")))
      ("Remove State" . (lambda () (org-todo 'none))))
    '("Traditional"
      ("TODO"         . (lambda () (org-todo "TODO")))
      ("NEXT"         . (lambda () (org-todo "NEXT")))
      ("STARTED"      . (lambda () (org-todo "STARTED")))
      ("DONE"         . (lambda () (org-todo "DONE")))
      ("WAITING"      . (lambda () (org-todo "WAITING")))
      ("CANCELLED"    . (lambda () (org-todo "CANCELLED"))))))

(defun org-gadget-priority-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Priority"
    '("Set Priority"
      ("A (High)"   . (lambda () (org-priority ?A)))
      ("B (Medium)" . (lambda () (org-priority ?B)))
      ("C (Low)"    . (lambda () (org-priority ?C))))
    '("Adjust"
      ("Increase"   . (lambda () (org-priority 'up)))
      ("Decrease"   . (lambda () (org-priority 'down)))
      ("Remove"     . (lambda () (org-priority ?\s))))))

(defun org-gadget-agenda-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Org Agenda"
    '("Views"
      ("Day"   . (lambda () (org-agenda-list nil nil 'day)))
      ("Week"  . (lambda () (org-agenda-list nil nil 'week)))
      ("Month" . (lambda () (org-agenda-list nil nil 'month))))
    '("Tasks & Search"
      ("Global TODOs"     . (lambda () (org-agenda nil "t")))
      ("Specific keyword" . (lambda () (org-agenda nil "T")))
      ("Tags/Properties"  . (lambda () (org-agenda nil "m")))
      ("Match TODOs"      . (lambda () (org-agenda nil "M")))
      ("Full-text"        . (lambda () (org-agenda nil "s"))))
    '("More"
      ("Agenda + TODOs"   . org-gadget-agenda-combo)
      ("Buffer only"      . (lambda () (org-agenda '(4) "a")))
      ("Native Dispatch"  . org-agenda))))

(defun org-gadget-sort-menu (&optional event)
  (interactive "e")
  (if (not (or (org-at-item-p) (org-at-heading-p)))
      (user-error "Place point on a heading or list item to sort")
    (org-gadget--popup event "Sort By"
      '("Basic"
        ("Alphabetical" . (lambda () (org-gadget--sort ?a)))
        ("Numeric"      . (lambda () (org-gadget--sort ?n)))
        ("Time/Date"    . (lambda () (org-gadget--sort ?t))))
      '("Status"
        ("TODO keyword" . (lambda () (org-gadget--sort ?k)))
        ("Deadline"     . (lambda () (org-gadget--sort ?d)))
        ("Scheduled"    . (lambda () (org-gadget--sort ?s))))
      '("Advanced"
        ("Priority"     . (lambda () (org-gadget--sort ?p)))
        ("Created"      . (lambda () (org-gadget--sort ?c)))
        ("Property"     . (lambda () (org-gadget--sort ?r)))
        ("Function"     . (lambda () (org-gadget--sort ?f)))))))

(defun org-gadget-emphasis-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Formatting"
    '("Standard"
      ("Bold"      . (lambda () (org-gadget--emphasis-apply ?*)))
      ("Italic"    . (lambda () (org-gadget--emphasis-apply ?/)))
      ("Underline" . (lambda () (org-gadget--emphasis-apply ?_))))
    '("Extra"
      ("Strike"    . (lambda () (org-gadget--emphasis-apply ?+)))
      ("Code"      . (lambda () (org-gadget--emphasis-apply ?~)))
      ("Verbatim"  . (lambda () (org-gadget--emphasis-apply ?=))))))

(defun org-gadget-block-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Insert Block"
    '("Blocks"
      ("Source (code)" . (lambda () (org-insert-structure-template "src")))
      ("Quote"         . (lambda () (org-insert-structure-template "quote")))
      ("Example"       . (lambda () (org-insert-structure-template "example")))
      ("Center"        . (lambda () (org-insert-structure-template "center")))
      ("Verse"         . (lambda () (org-insert-structure-template "verse")))
      ("Note"          . (lambda () (org-insert-structure-template "note"))))))

(defun org-gadget-date-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Dates & Scheduling"
    '("Set Date"
      ("Schedule"       . org-schedule)
      ("Deadline"       . org-deadline)
      ("Timestamp"      . org-time-stamp)
      ("Inactive Stamp" . org-time-stamp-inactive))
    '("Recurring"
      ("Repeating..."   . org-gadget-schedule-repeating)
      ("Nth Weekday..." . org-gadget-recurring-weekday))))

(defun org-gadget-archive-menu (&optional event)
  (interactive "e")
  (let* ((heading (org-get-heading t t t t))
         (title (format "Archive: %s" (truncate-string-to-width (or heading "?") 30 nil nil "…"))))
    (org-gadget--popup event "Confirm Archive"
      `(,title
        ("Yes, archive subtree" . org-archive-subtree)
        ("Refile instead"       . org-refile)))))

(defun org-gadget-link-menu (&optional event)
  (interactive "e")
  (org-gadget--popup event "Links"
    '("Insert"
      ("Insert Link"   . org-insert-link)
      ("Store Link"    . org-store-link))
    '("Navigate"
      ("Next Link"     . org-next-link)
      ("Previous Link" . org-previous-link)
      ("Open at Point" . org-open-at-point))))

;; ═══════════════════════════════════════════════════════
;; TOOLBAR ICON INSERTION
;; ═══════════════════════════════════════════════════════

(defun org-gadget--insert-icon (icon-str data)
  "Insert ICON-STR with interaction keybindings baked into closures."
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
         (menu-cb   (when menu-fn  (org-gadget--make-lp-callback menu-fn)))
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
    (define-key map [return] (or click-cb #'ignore))

    ;; --- INSERT ---
    (insert " ")
    (insert (propertize icon-str
                        'help-echo tooltip
                        'mouse-face 'highlight
                        'keymap map
                        'pointer 'hand
                        'org-gadget-lp-cb lp-cb))
    (insert " ")))

;; ═══════════════════════════════════════════════════════
;; GROUP DEFINITIONS
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
  "Alist of group symbol → list of icon plists.")

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
      (setq-local auto-hscroll-mode nil)     ;; Fixes jumping/scrolling
      (setq-local touch-screen-delay 0.3)    ;; Speeds up the hold action
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil)
      (setq-local window-size-fixed 'height)
      
      ;; Catch the native touch hold, pass the event directly to the menu callback!
      (local-set-key [touchscreen-hold]
                     (lambda (event)
                       (interactive "e")
                       (let* ((posn (event-start event))
                              (pt   (posn-point posn))
                              (win  (posn-window posn))
                              (buf  (when (windowp win) (window-buffer win)))
                              (cb   (when (and pt buf)
                                      (get-text-property pt 'org-gadget-lp-cb buf))))
                         (when cb
                           (setq org-gadget--press-fired t)
                           (funcall cb event))))))))

;; ═══════════════════════════════════════════════════════
;; SHOW / HIDE / AUTO-TOGGLE
;; ═══════════════════════════════════════════════════════

(defun org-gadget-show ()
  "Show the toolbar below the currently selected window."
  (interactive)
  (let ((buf (get-buffer-create org-gadget-buffer-name)))
    (if (and org-gadget--window (window-live-p org-gadget--window))
        (progn
          (set-window-dedicated-p org-gadget--window t)
          (set-window-parameter org-gadget--window 'no-other-window t)
          (set-window-parameter org-gadget--window 'no-delete-other-windows t))
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
  "Show toolbar if any Org file is visible, hide otherwise."
  (when org-gadget-auto-show
    (let* ((win (if (windowp frame-or-window) frame-or-window (selected-window)))
           (buf (window-buffer win)))
      (org-gadget--record-org-window)
      (unless (or (string= (buffer-name buf) org-gadget-buffer-name)
                  (window-minibuffer-p win))
        (if (org-gadget--any-org-file-visible-p)
            (org-gadget-show)
          (org-gadget-hide))))))

;; ═══════════════════════════════════════════════════════
;; KEYBINDINGS & MINOR MODE
;; ═══════════════════════════════════════════════════════

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

;;;###autoload
(define-minor-mode org-gadget-mode
  "Global minor mode for the Org-gadget toolbar and native popups."
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

(keymap-global-set "C-c a" #'org-gadget-agenda-menu)

(provide 'org-gadget)
;;; org-gadget.el ends here
