;;; techela-grade.el --- Functions for grading techela assignments
;;; techela-grade.el
;;; Commentary
;;; These are functions to facilitate grading in techela courses


;;; Commentary:
;; 

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
    ("WAIVED" . nil))
  "Numeric multipliers for letter grades.")


(defun gb-fraction-to-lettergrade (fraction)
  "Return the letter grade associated with FRACTION."
  (message "%s" (reverse gb-MULTIPLIERS))
  (let (last-letter-grade)
    (dolist (tuple (reverse gb-MULTIPLIERS) value)
      (let ((lettergrade (car tuple))
            (multiplier (cdr tuple)))
        (message "%s %s %s %s" fraction multiplier lettergrade last-letter-grade)
        (when (< fraction multiplier)
          (setq value last-letter-grade)
          (return ))
        (setq last-letter-grade lettergrade))) value))


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
	(org-narrow-to-subtree)
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
  "Insert a comment line of form \"# (*andrewid*): \"."
  (interactive)
  (insert (format "\n# (%s): \n" *andrewid*))
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
      (insert  (format "[[elisp:(goto-char %s)][(%s) line %s:]] typo\n\n"
                       current-point (user-login-name)  current-line)))))


(defun gb-assign-grade (lettergrade)
  "Add LETTERGRADE to the feedback section."
  (interactive
   (list (ido-completing-read
	  "Letter grade: "
	  (mapcar (lambda (x) (car x)) gb-MULTIPLIERS)
	  nil t)))
  (goto-char (point-min))
  (if (search-forward "#+GRADE:" (point-max) t)
      (progn
	(beginning-of-line)
	(kill-line)
	(insert (format "#+GRADE: %s" lettergrade)))
    ;else no grade found yet
    (goto-char (point-max))
    ;; save grade in a section so it does not get erased by new footnotes.
    (insert "* Grade\n")
    (insert (format "\n#+GRADE: %s" lettergrade)))
  (save-buffer))


;; see http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun gb-get-grade (fname)
  "Open file FNAME and get grade."
  (interactive "fFile: ")
  (if (and (file-exists-p fname) (file-readable-p fname))
      (progn
	(let ((b (find-file-noselect fname))
	      (kwds))
	  (set-buffer b)
	  (setq kwds (org-element-map (org-element-parse-buffer 'element) 'keyword
		       (lambda (keyword) (cons (org-element-property :key keyword)
					       (org-element-property :value keyword)))))
	  ;(kill-buffer b)
	  (or (cdr (assoc "GRADE" kwds))
	      (error "No grade found in %s" fname))))))

(defun gb-save-and-close-buffer ()
  "Save current buffer and kill it."
  (interactive)
  (save-buffer)
  (kill-buffer))

;; here I rebind Alt-s as a prefix to these functions.
;; it seems like a reasonable choice.
(defvar grade-mode-map
  (let ((gb-map (make-sparse-keymap)))
    (define-key gb-map (kbd "M-s f") 'gb-feedback)
    (define-key gb-map (kbd "M-s n") 'gb-insert-footnote)
    (define-key gb-map (kbd "M-s c") 'gb-insert-comment)
    (define-key gb-map (kbd "M-s t") 'gb-feedback-typo)
    (define-key gb-map (kbd "M-s g") 'gb-assign-grade)
    (define-key gb-map (kbd "M-s q") 'gb-save-and-close-buffer)
    gb-map)
  "Keymap for function `grade-mode'.")


(easy-menu-define grade-menu grade-mode-map "Grade menu"
   '("grade"
     ["Insert feedback" gb-feedback t]
     ["Insert footnote" gb-insert-footnote t]
     ["Insert comment" gb-insert-comment t]
     ["Insert typo" gb-feedback-typo t]
     ["Assign grade" gb-assign-grade t]
     ["Save and close buffer" gb-save-and-close-buffer t]
 ))

(define-minor-mode grade-mode
  "Minor mode for grade

\\{grade-mode-map}"
  :lighter " grade"
  :keymap grade-mode-map)

(provide 'techela-grade)

;;; techela-grade.el ends here
