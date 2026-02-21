;;; ikemacs-org.el --- Org Mode Configuration -*- lexical-binding: t; -*-

;; Set the standard Org directory (Primary Location)
(setq org-directory "~/Org Notes/")

;; Cache for sidebar data
(defvar ike/overdue-tasks-cache nil 
  "Cache of overdue task markers to display in the sidebar.")

(auto-save-visited-mode 1)
(setq auto-save-visited-interval 30)
(setq default-directory org-directory)

;; --- STRICT DATE LOGIC HELPER ---

(defun ike/is-task-strict-overdue-p ()
  "Return t if the task is strictly overdue or due today.
   Ignores warning periods. Ignores inheritance.
   Reads the date literally from the current line."
  (let* ((deadline-str (org-entry-get (point) "DEADLINE"))
         (sched-str    (org-entry-get (point) "SCHEDULED"))
         (today        (org-today)) ;; Absolute day number for today
         (d-day        (and deadline-str (org-time-string-to-absolute deadline-str)))
         (s-day        (and sched-str    (org-time-string-to-absolute sched-str))))
    
    ;; Logic: TRUE if (Deadline exists AND <= Today) OR (Schedule exists AND <= Today)
    (or (and d-day (<= d-day today))
        (and s-day (<= s-day today)))))

;; --- PERFORMANCE: ASYNC UPDATE CYCLE ---

(defun ike/update-sidebar-data ()
  "Query org-ql for overdue tasks and refresh the sidebar."
  (interactive)
  (when org-agenda-files
    (require 'org-ql)
    
    ;; QUERY EXPLANATION:
    ;; 1. (not (done)) -> Filter out closed tasks first (fastest check).
    ;; 2. (funcall ...) -> Run our custom strict math function.
    (setq ike/overdue-tasks-cache
          (org-ql-select org-agenda-files
            '(and (not (done))
                  (funcall 'ike/is-task-strict-overdue-p)) 
            :action '(list (org-get-heading t t t t) (point-marker))
            :sort '(date priority)))
    
    (message "Sidebar updated. Found %d active overdue tasks." (length ike/overdue-tasks-cache))
    
    (when (fboundp 'ike/sidebar-update)
      (ike/sidebar-update))))

(defun ike/refresh-agenda-files ()
  "Refresh the file list recursively from `org-directory`."
  (interactive)
  (when (file-directory-p org-directory)
    (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
    (message "Org Agenda files refreshed from %s" org-directory)
    (ike/update-sidebar-data)))

(use-package org
  :ensure t
  :hook (org-mode . visual-line-mode)
  :config
  (require 'org-indent)
  (require 'org-element)
  (require 'transient)
  (require 'org-mouse)
  
  ;; --- WINDOW MANAGEMENT ---
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  
  (setq org-M-RET-may-split-line t)
  (setq org-startup-indented t)
  (setq org-level-color-stars-only t)
  (setq org-startup-folded 'nofold)
  (setq org-adapt-indentation nil)
  (setq org-indent-indentation-per-level 3)
  (setq org-list-allow-alphabetical t)
  (setq-default line-spacing 3)
  (setq org-export-with-sub-superscripts nil)
  
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
  
  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
  (setq org-todo-keywords '((sequence "TODO" "UNDERWAY" "|" "DONE")))

  (define-key org-mode-map (kbd "<S-return>") nil)
  (define-key org-mode-map (kbd "<S-return>") 'org-return)
  (define-key org-mode-map (kbd "C-^") 'org-sort)
  (define-key org-mode-map (kbd "RET") 'org-dynamic-return))

;; --- LAZY LOAD: ORG-QL ---
(use-package org-ql 
  :ensure t
  :defer t
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*Org QL" display-buffer-same-window))) 

(use-package htmlize :ensure t :defer t)

(use-package org-sticky-header
  :ensure t
  :after org
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full)
  (setq org-sticky-header-outline-path-separator " > "))

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . global-org-modern-mode) (org-agenda-finalize . org-modern-agenda))
  :config
   (set-face-attribute 'org-modern-label nil :height 0.9)
   (setq org-tags-column 0
         org-catch-invisible-edits 'show-and-error
         org-special-ctrl-a/e t
         org-hide-emphasis-markers t
         org-pretty-entities t
         org-ellipsis "…"
         org-modern-fold-stars '(("▷" . "▽") ("▷" . "▽") ("▷" . "▽") ("▷" . "▽"))
         org-modern-todo t
         org-modern-todo-faces '(("TODO" :background "#6a5acd" :foreground "white" :weight bold) 
                                 ("UNDERWAY" :background "dark cyan" :foreground "white" :weight bold) 
                                 ("DONE" :background "dark slate gray" :foreground "white" :weight bold))))

(defun org-cycle-hide-drawers (state)
  (when (and (derived-mode-p 'org-mode) (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max) (if (eq state 'children) (save-excursion (outline-next-heading) (point)) (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0))) (limit (save-excursion (outline-next-heading) (point))))
                (if (re-search-forward "^[ \t]*:END:" limit t) (outline-flag-region start (point-at-eol) t))))))))))

(defun my-org-has-child-p () (interactive) (save-excursion (org-goto-first-child)))

(defun org-dynamic-return () (interactive)
  (let ((element (org-element-at-point)))
    (if (and (eq (org-element-type element) 'headline) (org-element-property :todo-keyword element))
        (org-insert-todo-heading t) (org-meta-return))))

;; Initial Scan (2 seconds after load)
(run-with-idle-timer 2 nil #'ike/refresh-agenda-files)

;; Auto-update when saving
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ike/update-sidebar-data nil 'make-it-local)))

(provide 'ikemacs-org)
