(defun ike/gui-visit-file-dialog ()
  "Prompt for a file using the system GUI dialog.
   Forces the dialog even if triggered by keyboard (C-o).
   If the file is new, it is created (saved) immediately."
  (interactive)
  ;; TRICK: Set last-nonmenu-event to nil. 
  ;; This makes Emacs think the command wasn't triggered by a key, 
  ;; so it honors 'use-dialog-box' and pops the GUI.
  (let* ((last-nonmenu-event nil)
         (use-dialog-box t)
         (use-file-dialog t)
         ;; manual read-file-name ensures we get the string before opening
         (filename (read-file-name "Open/Create File: ")))
    
    ;; 1. Visit the file (create the buffer)
    (find-file filename)
    
    ;; 2. Auto-Create: If the file is new (doesn't exist on disk yet), 
    ;; save it immediately so you don't have to 'Save As' later.
    (when (and filename 
               (not (file-exists-p filename)))
      (save-buffer))))

(defun ike/gui-save-file ()
  "Save the current buffer. 
   - If it already has a filename, just save it (no dialog).
   - If it's a new/scratch buffer, pop up the GUI dialog (Save As)."
  (interactive)
  (if (buffer-file-name)
      ;; Scenario A: File already has a path -> Just save quietly
      (save-buffer)
    ;; Scenario B: Untitled buffer -> Ask user for a name/location
    (ike/gui-save-file-as)))

(defun ike/gui-save-file-as ()
  "Force the system 'Save As' dialog, even if triggered by keyboard."
  (interactive)
  (let* ((last-nonmenu-event nil) ; The Trick: Force Emacs to think this is a mouse event
         (use-dialog-box t)
         (use-file-dialog t)
         ;; Explicitly ask for the filename first using the dialog
         (filename (read-file-name "Save As: ")))
    ;; Then write the file to that location
    (write-file filename)))

(provide 'ikemacs-dialogs)
