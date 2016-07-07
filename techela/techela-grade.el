;;; techela-grade.el --- Functions for grading techela assignments

;;; Commentary:
;; These are functions to facilitate grading in techela courses

;;; Code:

(defvar gb-MULTIPLIERS
  '(("A++" . 1.0)
    ("A+" . 0.95)
    ("A" . 0.9)
    ("A-" . 0.85)
    ("A/B" . 0.8)
    ("B+" . 0.75)
    ("B" . 0.7)
    ("B-" . 0.65)
    ("B/C" . 0.6)
    ("C+" . 0.55)
    ("C" . 0.5)
    ("C-" . 0.45)
    ("C/D" . 0.4)
    ("D+" . 0.35)
    ("D" . 0.3)
    ("D-" . 0.25)
    ("D/R" . 0.2)
    ("R+" . 0.15)
    ("R" . 0.1)
    ("R-" . 0.05)
    ("R--" . 0.0)
    ("P" . 1.0)    ;; Pass
    ("F" . 0.0)    ;; Fail
    ("WAIVED" . nil))
  "Numeric multipliers for letter grades.")


(defun gb-fraction-to-lettergrade (fraction)
  "Return the letter grade associated with FRACTION."
  (let ((tuples (reverse gb-MULTIPLIERS))
	(last-letter-grade))
    (setq tuples (remove '("WAIVED" . nil) tuples))
    (setq tuples (remove '("P" . 1.0) tuples))
    (setq tuples (remove '("F" . 1.0) tuples))


    (catch 'grade
      (dolist (tuple tuples)
	(let ((lettergrade (car tuple))
	      (multiplier (cdr tuple)))
	  (when multiplier
	    (when (< fraction multiplier)
	      (throw 'grade last-letter-grade))
	    (setq last-letter-grade lettergrade)))))))

(defun gb-feedback (comment)
  "Insert feedback in an org file.  Bound to \\[gb-feedback].

This is a little fragile because it uses positions in links.  These change if you add COMMENTs, or footnotes, or modify the document.  Markers would be one solution to this, but I am not sure these can be saved."
  (interactive "sComment: ")

  ;; get location
  (let ((current-point (point))
        (current-line (count-lines (point-min) (point))))

    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "* feedback\n" (point-max) 'end)
        (insert "\n* feedback\n"))

      (save-restriction
	;; (org-narrow-to-subtree)
	(goto-char (point-max))
	(insert  (format "[[elisp:(goto-char %s)][(%s) line %s:]] %s\n\n"
			 current-point (user-login-name)  current-line comment ))))))


(defun gb-insert-footnote (note)
  "Create a new footnote with NOTE in the current file.
The footnote will be surrounded by a space on each side.  Cursor is
placed after the new link when it is done."
  (interactive "sNote: ")
  (insert " ")
  (org-mark-ring-push)
  (org-footnote-new)
  (insert note)
  (org-mark-ring-goto)
  (forward-char 6)
  (insert " "))


(defun gb-insert-comment ()
  "Insert a comment line of form \"# (ta-userid): \"."
  (interactive)
  (insert (format "\n# (%s): \n" ta-userid))
  (previous-line)
  (end-of-line))


(defun gb-feedback-typo()
  "insert typo feedback. Bound to \\[gb-feedback-typo]."
  (interactive)

  ;; get location
  (let ((current-point (point))
        (current-line (count-lines (point-min) (point))))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "* feedback\n" (point-max) 'end)
        (insert "\n* feedback\n"))

      (goto-char (point-max))
      (let ((ntypos (gb-get-filetag "NTYPOS")))
	(if ntypos
	    (gb-set-filetag "NTYPOS" (+ 1 (string-to-int ntypos)))
	  (gb-set-filetag "NTYPOS" 1)))
      (insert  (format "[[elisp:(goto-char %s)][(%s) line %s:]] typo\n\n"
                       current-point (user-login-name)  current-line)))))


(defun gb-ontime-p ()
  "Return if the assignment is on time."
  (interactive)

  (let ((duedate (replace-regexp-in-string
		  ">"
		  " 23:59:59>" (gb-get-filetag "DUEDATE")))
	(turned-in (format-time-string
		    "[%Y-%m-%d %a %H:%M]"
		    (date-to-time (gb-get-filetag "TURNED-IN")))))
    (org-time< turned-in duedate)))


(defun gb-grade ()
  "Run a rubric function to insert the grade.
This assumes the assignment label is the filename you are in."
  (interactive)
  (save-window-excursion
    (let ((rubric)
	  (label (file-name-sans-extension
		  (file-name-nondirectory (buffer-name))))
	  (cb (current-buffer))
	  (tbuf (find-file-noselect (expand-file-name "syllabus.org" ta-course-dir))))
      (unless (-contains? (ta-get-assigned-assignments) label)
	(error "%s is not an assignment" label))

      ;; get the rubric from the syllabus, to make sure it has not been
      ;; altered by a student
      (set-buffer tbuf)
      (goto-char (point-min))
      (org-open-link-from-string
       ;; there must be a section with a custom_id in the syllabus
       (format "[[#%s]]" label) tbuf)
      (setq rubric (read (org-entry-get (point) "RUBRIC")))
      (set-buffer cb)
      (kill-buffer tbuf)

      ;; Now, loop over rubric
      (setq categories (mapcar (lambda (x) (car x)) rubric))
      (setq LGS (mapcar (lambda (cell)
			  (ido-completing-read
			   (concat
			    (cond
			     ((symbolp (car cell))
			      (symbol-name (car cell)))
			     ((stringp (car cell))
			      (car cell)))
			    ": ")
			   (mapcar (lambda (x) (car x)) gb-MULTIPLIERS))) rubric))

      (setq multipliers (mapcar (lambda (LG) (cdr (assoc LG gb-MULTIPLIERS)))
				LGS))
      (setq weights (mapcar (lambda (x) (cdr x)) rubric))
      (setq grade (reduce '+ (cl-mapcar (lambda (weight multiplier) (* weight multiplier))
					weights multipliers)))

      (goto-char (point-min))
      (unless (re-search-forward "* Grade" (point-max) 'end)
	(insert "\n* Grade\n"))

					; (org-open-link-from-string "[[*Grade]]")
      (cl-mapcar (lambda (category grade) (gb-set-filetag category grade))
		 categories LGS)
      (gb-set-filetag "GRADE" (format "%1.3f" grade))
      (gb-set-filetag "GRADED-BY" user-full-name)
      (unless (gb-ontime-p)
	(gb-set-filetag "LATE" "Your assignment was late. You may be subject to a 50% penalty in the future."))
      (save-buffer)
      (kill-buffer))))


;; see http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun gb-get-grade (fname)
  "Open file FNAME and get grade."
  (interactive "fFile: ")
  (when (and (file-exists-p fname) (file-readable-p fname))
    (with-temp-buffer
      (insert-file-contents fname)
      (org-mode)
      (gb-get-filetag "GRADE"))))


(defun gb-set-filetag (tag value)
  "Set filetag TAG to VALUE."
  (interactive "sTag: \nsValue: ")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "#\\+%s:" tag) (point-max) 'end)
	;; replace existing filetag
	(progn
	  (beginning-of-line)
	  (kill-line)
	  (when value
	    (insert (format "#+%s: %s\n" tag value))))
      ;; add new filetag
      (if (eq (line-beginning-position) (point))
	  ;; at beginning of line
	  (when value
	    (insert (format "#+%s: %s\n" tag value)))
	;; at end of some line, so add a new line
	(when value
	  (insert (format "\n#+%s: %s\n" tag value)))))))


(defun gb-get-filetag (tag)
  "Return value for TAG in the org-file."
  (interactive "sTag: ")
  ;; (setq kwds (org-element-map (org-element-parse-buffer 'element) 'keyword
  ;;	       (lambda (keyword)
  ;;		 (cons (org-element-property :key keyword)
  ;;		       (org-element-property :value keyword)))))
  ;; (cdr (assoc tag kwds))

  ;; Apr 13, 2016 I am not sure why the code above stopped working. It seems to
  ;; be the parse buffer command that hangs.
  (save-excursion
    (goto-char (point-min))
    (if	(re-search-forward (format "#\\+%s:\\(.*\\)" tag) (point-max) t)
	(match-string 1)
      nil)))


(defun gb-save-and-close-buffer ()
  "Save current buffer and kill it."
  (interactive)
  (save-buffer)
  (kill-buffer))


(defun gb-return ()
  "Return current buffer. assumes you are on an assignment"
  (interactive)
  (ta-return-to
   (gb-get-filetag "ASSIGNMENT")
   ;; hackery to get userid from the directory name. Basically
   ;; splitting the path to get the directory this buffer is in,
   ;; splitting that directory by "-" and taking the first part as the
   ;; userid
   (car
    (split-string
     (car
      (last (butlast (split-string (buffer-file-name) "/")))) "-"))))


;; here I rebind Alt-s as a prefix to these functions.
;; it seems like a reasonable choice.
(defvar grade-mode-map
  (let ((gb-map (make-sparse-keymap)))
    (define-key gb-map (kbd "M-s f") 'gb-feedback)
    (define-key gb-map (kbd "M-s n") 'gb-insert-footnote)
    (define-key gb-map (kbd "M-s c") 'gb-insert-comment)
    (define-key gb-map (kbd "M-s t") 'gb-feedback-typo)
    (define-key gb-map (kbd "M-s g") 'gb-grade)
    (define-key gb-map (kbd "M-s r") 'gb-return)
    (define-key gb-map (kbd "M-s q") 'gb-save-and-close-buffer)
    gb-map)
  "Keymap for function `grade-mode'.")





(easy-menu-define grade-menu grade-mode-map "Grade menu"
   '("grade"
     ["Insert feedback" gb-feedback t]
     ["Insert footnote" gb-insert-footnote t]
     ["Insert comment" gb-insert-comment t]
     ["Insert typo" gb-feedback-typo t]
     ["Assign grade" gb-grade t]
     ["Return assignment" gb-return t]
     ["Save and close buffer" gb-save-and-close-buffer t]
 ))

(define-minor-mode grade-mode
  "Minor mode for grade

\\{grade-mode-map}"
  :lighter " grade"
  :global t
  :keymap grade-mode-map)

(provide 'techela-grade)

;;; techela-grade.el ends here
