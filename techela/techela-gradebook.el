;;; techela-gradebook.el --- Gradebook



;;; Commentary:
;; This is a library of function for computing course grades in techela.

;;; Code:

(defun ta-get-assigned-assignments-points-categories ()
  "Return a list of assignments from the syllabus.
Assignments are headings that are tagged with :assignment: and
which are GRADED.  This returns a list of (assignment points
category)"
  (interactive)
  (save-current-buffer

    (with-current-buffer
	(find-file-noselect  (expand-file-name "syllabus.org" ta-course-dir))
      (org-map-entries
       (lambda ()
	 (list
	  (org-entry-get (point) "CUSTOM_ID")
	  (string-to-number (org-entry-get (point) "POINTS"))
	  (org-entry-get (point) "CATEGORY")))
       "+assignment+TODO=\"GRADED\""))))


(defun ta-get-assignments-in-category (category)
  "Get a list of assignments, points and categories for CATEGORY."
  (-filter
   (lambda (x)
     (string= (elt x 2) category))
   (ta-get-assigned-assignments-points-categories)))


(defun ta-insert-student-work-link (label userid)
  "insert link  to open a student assignment."
    (interactive
     (list
      (ido-completing-read "Label: " (ta-get-assigned-assignments) nil t)
      (ido-completing-read "Userid: " (ta-get-userids))))
    (let ((fname (expand-file-name
		  (format "%s.org" label)
		  (expand-file-name
		   (format "%s-%s" userid label)
		   (expand-file-name
		    label
		    ta-course-student-work-dir)))))
      (insert (format "[[%s][%s]]"
		      fname
		      (format "%s-%s.org" userid label)))))


(defun ta-get-user-grade (label userid)
  "Returns the numeric fractional grade in a file."
  (interactive
   (list
    (ido-completing-read "Label: " (ta-get-assigned-assignments) nil t)
    (ido-completing-read "Userid: " (ta-get-userids))))

  (let ((fname (expand-file-name
		(format "%s.org" label)
		(expand-file-name
		 (format "%s-%s" userid label)
		 (expand-file-name
		  label
		  ta-course-student-work-dir)))))
    (when (file-exists-p fname)
      (string-to-number (or (gb-get-grade fname) "")))))


(defun ta-get-user-grades (userid)
  "Get array of user grades for each assigned, graded assignment.
column 0 is the assignment name
column 1 is the fractional grade
column 2 is the points the problem is worth
column 3 is the category
column 4 is the category weight
column 5 is the fractional grade * points * category weight"
  (interactive
   (list
    (ido-completing-read "Userid: " (ta-get-userids))))

  ;; body of function
  (mapcar
   (lambda (label)
     (let* ((e (assoc label (ta-get-assigned-assignments-points-categories)))
	    (grade (ta-get-user-grade label userid))
	    (points (nth 1 e))
	    (category (nth 2 e))
	    (category-weight (cdr (assoc category (ta-get-categories-weights)))))
       ;; here we construct the list to return for each assighment
       (list
	label
	grade
	points
	category
	category-weight
	(if grade
	    (*  grade points category-weight)
	  0)
	(* points category-weight)
	)))
   ;; map over list of graded assignments
   (mapcar 'car (ta-get-assigned-assignments-points-categories))))


(defun ta-get-user-overall-grade (userid)
  "Return list of lastname, firstname, userid, fractional and letter grade for USERID."
  (interactive
   (list
    (ido-completing-read "Userid: " (ta-get-userids))))

  ;; start body of function
  (let* ((grades (ta-get-user-grades userid))
	 (earned (mapcar (lambda (x) (nth 5 x)) grades))
	 (possible (mapcar (lambda (x) (nth 6 x)) grades))
	 (fgrade (/ (apply '+ earned) (apply '+ possible)))
	 (lg (gb-fraction-to-lettergrade fgrade)))

    ;; getting names of the student from the roster
    (catch 'grade
      (dolist (roster-file ta-roster)
	(with-temp-buffer
	  (insert-file-contents roster-file)
	  (let* ((contents (cdr (csv-parse-buffer nil))) ;; first line is header
		 (roster (mapcar (lambda (x)
				   (list (nth 8 x)	      ; userid
					 :firstname (nth 6 x) ; first name
					 :lastname (nth 5 x)))
				 contents))
		 (firstname (plist-get (cdr (assoc userid roster)) :firstname))
		 (lastname  (plist-get (cdr (assoc userid roster)) :lastname)))
	    (when firstname
	      (throw 'grade (list
			     lastname
			     firstname
			     userid
			     fgrade			; fractional grade
			     (gb-fraction-to-lettergrade fgrade))))))))))


(defun ta-get-user-category-grades (userid category)
  "Return list of USERID, assignment, points and grade for each graded assignment in CATEGORY."
  (interactive
   (list
    (ido-completing-read "Userid: " (ta-get-userids))
    (ido-completing-read "Category: " (ta-get-categories))))

  (mapcar
   (lambda (x)
     (list
      userid
      (elt x 0)
      (elt x 2)
      (elt x 1)))
   (-filter
    (lambda (x)
      (string= category (elt x 3)))
    (ta-get-user-grades userid))))


;; helm interface to gradebook

(defun ta-gradebook-candidates ()
  (loop for row in (ta-roster)
	collect
	(let* ((userid (car row))
	       (plist (cdr row))
	       (name (plist-get plist :name)))
	  (cons (format "%10s | %s" userid name) userid))))

(defvar helm-source-gradebook-students '())

(setq helm-source-gradebook-students
      '((name . "Students")
	(candidates . ta-gradebook-candidates)
	(action . (("Individual grades" . ta-helm-student-grades)
		   ("Overall grade" . ta-helm-student-overall-grade)))))


(defun ta-helm-student-overall-grade (userid)
  "Print overall grade for USERID in a buffer."
  (switch-to-buffer (get-buffer-create "*gradebook*"))
  (erase-buffer)
  (insert
   (destructuring-bind (lname fname id fgrade lgrade)
       (ta-get-user-overall-grade userid)

     (format "%s %s (%s) %1.3f %s"
	     fname lname id fgrade lgrade))))


(defun ta-student-overall-grades ()
  "Print overall grades in a buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*overall gradebook*"))
  (erase-buffer)
  (org-mode)
  (insert "| First name | Last Name | id | Grade | Letter grade |\n|-\n")
  (dolist (userid (mapcar 'car (ta-roster)))
    (insert
     (destructuring-bind (lname fname id fgrade lgrade)
	 (ta-get-user-overall-grade userid)

       (format "| %s | %s | [[elisp:(ta-helm-student-grades \"%s\")][%s]] |  %1.3f | %s | \n"
	       fname lname id id fgrade lgrade))))
  (previous-line 2)
  (org-table-align)
  (goto-char (point-max)))


(defun ta-helm-student-grades (userid)
  "Generates a buffer with the grades for USERID."
  (switch-to-buffer (get-buffer-create "*gradebook*"))
  (erase-buffer)
  (insert (format "#+TITLE: Grade report for %s\n"
		  (destructuring-bind (lname fname id fgrade lgrade)
		      (ta-get-user-overall-grade userid)
		    (format "%s %s (%s)
Final grade: %1.3f %s\n"
			    fname lname id fgrade lgrade))))
  (insert "#+tblname: grades\n")
  (insert
   (mapconcat 'identity
	      (loop for row in (ta-get-user-grades userid)
		    collect
		    (format "|%20s| %15s| %6.3f | %6s|"
			    (elt row 0)  ; assignment
			    (elt row 3)  ; category
			    (or (elt row 1) 0.0) ; fractional grade
			    (elt row 2)  ; points
			    ))
	      "\n"))
  (goto-char (point-min))
  (org-mode))


(defun ta-helm-gradebook ()
  "A helm interface to the gradebook."
  (interactive)
  (helm :sources '(helm-source-gradebook-students)))

(provide 'techela-gradebook)

;;; techela-gradebook.el ends here
