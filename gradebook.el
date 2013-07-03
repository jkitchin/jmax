(require 'org-table) ; needed for access to org-table api

(defvar gb/MULTIPLIERS
  '(("A++" . 1.0)
    ("A+"   . 0.95)
    ("A"    . 0.9)
    ("A-"   . 0.85)
    ("A/B"  . 0.8)
    ("B+"   . 0.75)
    ("B"    . 0.7)
    ("B-"   . 0.65)
    ("B/C"  . 0.6)
    ("C+"   . 0.55)
    ("C"    . 0.5)
    ("C-"   . 0.45)
    ("C/D"  . 0.4)
    ("D+"   . 0.35)
    ("D"    . 0.3)
    ("D-"   . 0.25)
    ("D/R"  . 0.2)
    ("R+"   . 0.15)
    ("R"    . 0.1)
    ("R-"   . 0.05)
    ("R--"  . 0.0))
  "Numeric multipliers for letter grades")

(defun gb/goto-table (tblname)
  "move cursor into the table labeled tblname"
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp (format "^#\\+tblname:\s+%s" tblname))
  (next-line))

(defun gb/get-gradebook-lisp ()
  (interactive)
  (save-excursion
    (gb/goto-table "gradebook")
    (org-table-to-lisp)))

(defun gb/get-multiplier (LG)
  "return numeric multiplier for a letter grade"
  (interactive)
  (cdr (assoc (upcase LG) gb/MULTIPLIERS)))

(defun gb/get-multipliers(LGS)
  "apply get-multiplier to a list of letter grades"
  (interactive)
  (mapcar 'gb/get-multiplier LGS))

(defun gb/get-all-student-multipliers ()
  (mapcar 'gb/get-multipliers
	  (mapcar (lambda (x) 
		    (nthcdr 3 x)) 
		  (cdddr (gb/get-gradebook-lisp)))))

(defun gb/get-earned-points (multipliers)
  (mapcar* (lambda (a  b) (* a b)) multipliers gb/ASSIGNMENT-POINTS))

(defun gb/get-all-earned-points ()
  "returns total points earned by each student"
  (mapcar 'gb/get-earned-points  (gb/get-all-student-multipliers)))

(defun gb/get-all-numeric-grades ()
  (mapcar (lambda (x) 
	    (/ (apply '+ x) gb/TOTAL-POINTS))
	  (gb/get-all-earned-points)))

(defun gb/get-final-letter-grade (grade)
 (dolist (pair gb/MULTIPLIERS letter-grade)
   (if (< (cdr pair) grade) 
       (progn
	 (setq letter-grade (car pair))
	 (return letter-grade)))))

(defun gb/generate-report()
  (interactive)
 
  (let ((row1 (car (gb/get-gradebook-lisp))))
    (setq gb/ASSIGNMENTS (mapcar 'identity (nthcdr 3 row1))))

  (let ((row2 (cadr (gb/get-gradebook-lisp))))
    (setq gb/ASSIGNMENT-POINTS (mapcar 'string-to-number (nthcdr 3 row2)))
    (setq gb/TOTAL-POINTS (apply '+ gb/ASSIGNMENT-POINTS)))

;; report

  (let ((emails (mapcar '(lambda (x) (nth 2 x)) (cdddr (gb/get-gradebook-lisp))))
	(first-names (mapcar '(lambda (x) (nth 0 x)) (cdddr (gb/get-gradebook-lisp))))
	(last-names (mapcar '(lambda (x) (nth 1 x)) (cdddr (gb/get-gradebook-lisp))))
	(final-grades (mapcar 'gb/get-final-letter-grade (gb/get-all-numeric-grades))))
    (mapcar* (lambda (fn ln em fg)
	       `(,fn ,ln ,em ,fg)) 
	     first-names
	     last-names
	     emails
	     final-grades)))
