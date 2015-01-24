;;; lab-notebook.el --- lab-notebook-mode



;;; Commentary:
;;

;;; Code:

(defvar lab-notebook
  "/Users/jkitchin/Dropbox/org-mode/notebook.org"
  "File to store notebook entries in.")

(defun ln-add-entry (title &optional date)
  "Add entry with TITLE into the lab notebook. With optional DATE, file it."
  (interactive "sTitle: ")
  (find-file lab-notebook)
  (org-datetree-find-date-create (or date (calendar-current-date)))
  (end-of-line)
  (save-restriction
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (insert (format "\n**** %s\n" title))))

(defun dwin-vc ()
  "Do what I need in vc next.
Add current file if not in vc, then prompt for commit message"
  (interactive)
  (save-buffer)

  (let* ((deduction (vc-deduce-fileset nil t 'state-model-only-files))
	 (backend (elt deduction 0))
	 (fileset (elt deduction 1))
	 (state (elt deduction 3)))

    (cond
     (backend  ;; we are in a vc directory
      (when (file-exists-p (buffer-file-name))
	;; register the file if it is not
	(when (eq state 'unregistered)
	  (vc-register))

	;; Now commit it if needed. We know about edited, added,
	;; unregistered files. these should all be committed.
	(cond
	 ((or
	   (eq state 'edited)
	   (eq state 'added)
	   (eq state 'unregistered))
	  (vc-checkin (list (buffer-file-name))
		      backend
		      (read-string "Commit message: ")
		      t
		      ))
	 ((eq state 'up-to-date)
	  (message "up-to-date. No commit needed"))
	 ;; catch other states we do not have a plan for yet.
	 (t
	  (message "state of %s = %s. No action coded." (buffer-file-name) state)))))
     ;; catch case not in vc
     (t
      (message "Not in a VC repo. Perhaps run vc-create-repo?"))
     )))



;;;###autoload
(define-minor-mode lab-notebook-mode
  "Toggle lab notebook mode.
This will prompt you to commit a file when you kill a buffer
"
  ;; initial value
  nil
  ;; indicator
  :lighter " LN"
  ;; keybindings
  :keymap (let ((map (make-sparse-keymap)))
            ;(define-key map (kbd "<f5>") 'export-bibtex)
            map)
  ;; body
  (add-hook 'kill-buffer-hook 'dwin-vc nil 'make-it-local))


(provide 'lab-notebook)

;;; lab-notebook.el ends here
