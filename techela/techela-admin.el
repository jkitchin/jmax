;;; techela-admin.el --- Techela Administration functions
;; 

;;; Commentary:
;; Techela is a Technology enhanced learning and assessment
;; environment.  A techela course is hosted on a gitolite server.  The
;; public course is available at
;; coursename@techela.cheme.cmu.edu:course.
;; See README.org and FAQ.org
;;
;; TODO: Reports
;; Student grade report
;; Assignment summary
;; Participation summary
;; tagged assignment summary (ta-show-performance tag-expression)
;;     TAG-EXPRESSION should select the assignments, then we aggregate
;;     the scores

(require 'cl-lib)
(require 'csv)
(require 'techela)
(require 'techela-roster)
(require 'techela-grade)
(require 'f) ; file utilities

;;; Code:

(defvar ta-course-server "techela.cheme.cmu.edu" "Hostname where the course is served.")
(defvar ta-course-name nil "The course currently in action.")
(defvar ta-root-dir nil "The root directory for course files.")

(defvar ta-gitolite-admin-dir nil "Derived variable that stores the location of the admin directory.")
(defvar ta-roster nil "Derived variable absolute path to the roster file")
(defvar ta-course-dir nil "Derived variable absolute path to the public course file.")
(defvar ta-course-student-work-dir nil "Derived variable to the location of student work.")

(defvar ta-rubrics '(("participation" . (("participation" . 1.0)))
		     ("homework" . (("technical" . 0.7) ("presentation" . 0.2) ("typography" . 0.1))))
  "List of rubrics for assignments. Each element should be a list of components in alist form.")

;; i would like to deprecate this so techela is not CMU centric
(defvar ta-email-host "andrew.cmu.edu" "Hostname to construct email address from for userids with no @ in them.")

(defun techela-admin (course-name)
  "Open the course dashboard for COURSE-NAME.

The root directory is retrieved from custom.el if it exists,
otherwise you are prompted for a location to download the course
to, and that is saved in custom.el."
  (interactive
   (list
    (ido-completing-read
     "Course name: "
     (tq-config-get-admin-courses))))

  (setq ta-course-name course-name)

  (let ((course-hash (tq-config-get-admin-course course-name)))
    (if course-hash      
	(setq ta-root-dir (gethash "root-dir" course-hash)
	      ta-userid (gethash "userid" course-hash))
    ;; else no entry get info and add one.
    (setq ta-root-dir
	  (file-name-as-directory (expand-file-name ta-course-name "~/techela-admin" ))
	  ta-userid (read-from-minibuffer "Enter admin userid: "))
    ;; save course for future
    (tq-config-set-admin-course ta-course-name ta-userid ta-root-dir)))

  (ta-setup-user)
  
  ;; we should have a ta-root-dir now, so we set all the derived variables
  (setq ta-gitolite-admin-dir (file-name-as-directory
			       (expand-file-name
				"gitolite-admin"
				ta-root-dir))

	;; path to the roster file
	ta-roster (expand-file-name "roster.dat" ta-gitolite-admin-dir)

	;; local directory where assignment folders are
	ta-course-assignments-dir (file-name-as-directory
				  (expand-file-name
				   "assignments"
				   ta-root-dir))
	;; public facing course
	ta-course-dir (file-name-as-directory
		       (expand-file-name
			"course"
			ta-root-dir))

	;; local place where userid folders can be found
	ta-course-student-work-dir (file-name-as-directory
				    (expand-file-name
				     "student-work"
				     ta-root-dir))

	;; local place where class exercise folders can be found
	ta-course-class-work-dir (file-name-as-directory
				  (expand-file-name
				   "class-work"
				   ta-root-dir)))

  ;; Check that we have everything in place
  (unless (file-exists-p ta-root-dir)
    (make-directory ta-root-dir t))

  ;; where common class repos will be
  (unless (file-exists-p ta-course-class-work-dir)
    (make-directory ta-course-class-work-dir t))

  ;; where individual student repos will be
  (unless (file-exists-p ta-course-student-work-dir)
    (make-directory ta-course-student-work-dir))

  ;; make sure we have the gitolite admin folder
  (unless (file-exists-p ta-gitolite-admin-dir)
    (with-current-directory
     ta-root-dir
     (mygit (format "git clone %s@%s:gitolite-admin" ta-course-name ta-course-server))))

  ;; Make sure we have the course folder
  (unless (file-exists-p ta-course-dir)
    (with-current-directory
     ta-root-dir
     (mygit (format "git clone %s@%s:course" ta-course-name ta-course-server))))

  ;; open with the status view
  (ta-status))


(defun ta-get-userids ()
  "Return list of userids from a roster file."
  (mapcar (lambda (x) (car x)) (ta-roster)))


(defun ta-email (userid)
  "Email USERID with completion.  Select *all* to email everyone.  multiple selections are not possible."
  (interactive (list (ido-completing-read "Userid: " (append '("*all*") (ta-get-userids)))))

  (cond
   ((string= userid "*all*")
    (setq userid
	  (mapconcat
	   (lambda (x)
	     (cond
	      ((string-match "@" x) ;assume a userid with @ in it is an email address
	       x)
	      (t
	       ;; construct the email address
	       (format "%s@%s" x ta-email-host)))) (ta-get-userids) ",")))
    
   ((string-match "\\w+\\(\\.\\w+\\)?@\\(\\w\\|\\.\\)+" userid)
    nil) ;; valid email found, no need to set anything.
   (t ;; construct the email address
    (setq userid (format "%s@%s" userid ta-email-host))))

  (compose-mail-other-frame)
  (message-goto-to)
  (insert userid)
  (message-goto-subject)
  (insert (format "[%s]" ta-course-name))
  ;; we end here. you fill in the subject and finish the email.
  )


(defun ta-email@assignment ()
  "Generate email about visited assignment"
  (interactive)
  (let* ((USERID) (CATEGORY) (ASSIGNMENT)
	 (info (org-element-map
		  (org-element-parse-buffer 'element) 'keyword
		 (lambda (keyword) (cons (org-element-property :key keyword)
					(org-element-property :value keyword))))))
    (setq USERID (cdr (assoc "USERID" info))
	  CATEGORY (cdr (assoc "AUTHOR" info))
	  ASSIGNMENT (cdr (assoc "ASSIGNMENT" info)))
    (unless (and USERID CATEGORY ASSIGNMENT)
      (error "You do not appear to be on an assignment"))
    (compose-mail-other-window)
    (message-goto-to)
    (insert (if (string-match "@" USERID)
		USERID
	      (concat USERID "@" ta-email-host)))
    (message-goto-subject)
    (insert (format "[%s] @%s" ta-course-name ASSIGNMENT))
    (message-goto-body)))



;;; ==========================================================================================
;;; REPO creation functions

(defun ta-get-repo-name (label userid)
  "Return student private reponame on server for assignment LABEL of USERID.

For example: student-work/userid//userid-label."
  (format "student-work/%s/%s-%s" label userid label))


(defun ta-create-edit-repo-conf (reponame &optional R RW RW+)
  "Create a repo file for REPONAME with permissions. REPONAME may be a path.
R is a list of users with Read permission
RW is a list of users with Read/Write permission
RW+ is a list of users with force permission.

This creates a repo.conf file in the admin conf directory, then
commits it.  No push is done to create the repo on the server.
This is intentional, as sometimes you may want to create several
conf files before pushing. See `ta-create-edit-repo' which will
create and push the repo.

An existing conf file is overwritten, which allows you to change
permissions on an existing repo."

  (let ((repo-conf-dir (file-name-as-directory
			(expand-file-name "conf" ta-gitolite-admin-dir))))
    (with-current-directory
     repo-conf-dir
     (let ((repo-file
	    (expand-file-name
	     (concat reponame ".conf")
	     repo-conf-dir)))
       ;; make sure we have the folder to put it in. This mirrors what
       ;; is on gitolite
       (unless (file-exists-p (file-name-directory repo-file))
	 (make-directory (file-name-directory repo-file) t))

       (with-temp-file repo-file
	 (insert
	  (concat "repo " reponame "\n"
		  (when R
		    (format "    R = %s\n" (mapconcat 'identity R " ")))
		  (when RW
		    (format "    RW = %s\n" (mapconcat 'identity RW " ")))
		  (when RW+
		    (format "    RW+ = %s\n" (mapconcat 'identity RW+ " ")))
		  "\n")))
       
       (tq-log "-------------------- ta-create-edit-repo ---------------------\n")

       (mygit (format "git add %s" (file-relative-name repo-file repo-conf-dir)))
       
       (mygit (format "git commit %s -m \"create/edit %s"
		      (file-relative-name repo-file repo-conf-dir)
		      reponame))
       (tq-log "--------------------------------------------------------------\n")))))


(defun ta-create-edit-repo (reponame &optional R RW RW+)
  "This create the repo.conf file for REPONAME and pushes it.
Optional argument R read-only permission.
Optional argument RW read-write permission."
  (ta-create-edit-repo-conf reponame R RW RW+)
  (let ((repo-conf-dir (file-name-as-directory
			(expand-file-name "conf" ta-gitolite-admin-dir))))
    (with-current-directory
     repo-conf-dir
     (mygit "git push"))))


(defun ta-assign-to (label userid)
  "Assign assignment LABEL to USERID.
This gives the student RW access to a repo they can push to. You
need to make sure the student is in the roster, and that
`ta-roster-update' has been run so the student also has R access
to assignments."
  (interactive
   (list
    (ido-completing-read "Label: " (ta-get-assigned-assignments) nil t)
    (ido-completing-read "Userid: " (ta-get-userids))))
  ;; first, create the repo and push it.
  (let* ((repo-name (ta-get-repo-name label userid)))
    ;; this pushes so the effect is immediate
    (ta-create-edit-repo repo-name
			 nil        ;;R permission
			 (list userid)) ;; RW permission
    ))


(defun ta-collect-from (label userid)
  "Collect assignment LABEL from USERID.
This sets that repo to R access for USERID. We do not pull the assignment here."
  (interactive
   (list
    (ido-completing-read "Label: " (ta-get-assigned-assignments) nil t)
    (ido-completing-read "Userid: " (ta-get-userids))))
  (let* ((repo-name (ta-get-repo-name label userid)))
    ;; this pushes so the effect is immediate
    (ta-create-edit-repo repo-name			 
			 (list userid)))) ;; R permission


(defun ta-return-to (label userid)
  "Commit changes to LABEL and push to USERID repo."
  (interactive
   (list
    (ido-completing-read "Label: " (ta-get-assigned-assignments) nil t)
    (ido-completing-read "Userid: " (ta-get-userids))))
  (let* ((repo-name (ta-get-repo-name label userid))
	 (repo-dir-name (expand-file-name
			 repo-name
			 ta-root-dir))
	 (repo-dir (file-name-as-directory
		    (expand-file-name
		     repo-name
		     ta-root-dir))))
    (with-current-directory
     repo-dir
     (let ((process-environment (cons *GIT_SSH* process-environment)))
       (start-process-shell-command
	"ta-return"
	"*ta return*"
	"git add * && git commit -am \"Returning\" && git push")))))


(defun ta-create-assignment (label)
  "Create or edit an assignment LABEL. Interactively prompt for points, category, rubric and due date."
  (interactive (list
    (ido-completing-read "Label: " (ta-get-possible-assignments))))

  (let ((assignment-dir (file-name-as-directory
			 (expand-file-name
			  label
			  ta-course-assignments-dir))))
    (unless (file-exists-p assignment-dir)
      ;; no dir found. make one.
      (with-current-directory
       ta-course-assignments-dir
       (mygit (format "git clone %s@%s:a/%s"
		      ta-course-name ta-course-server label))))

    ;; create the org file and make sure it has the right filetags.
    (find-file (expand-file-name
		    (concat label ".org")
		    assignment-dir))
    (gb-set-filetag "ASSIGNMENT" label)
    (goto-char (point-min))
    ;; we assume that unless points is defined, we need to insert all
    ;; these things. We use completion where possible.
    (unless (re-search-forward "#\\+POINTS:" nil 'end)
      (insert (format "
#+POINTS: %s
#+CATEGORY: %s
#+RUBRIC: %s
#+DUEDATE: "
		      (read-input "Points: ")
		      (ido-completing-read "Category: " (ta-get-categories))
		      (cdr (assoc (ido-completing-read "Rubric: " (mapcar 'car ta-rubrics)) ta-rubrics))))
      (goto-char (point-max))
      ;; insert a due date.
      (org-time-stamp '()))))
		      
;;; These are class functions which should efficiently operate on each entry of the roster.

(defun ta-create-assignment-repos (label)
  "Create repos for all students in the roster for an assignment LABEL.

1. This will prompt you for a LABEL, which is a directory in the
assignment dir.

2. Create empty repo confs for all students in the course, with
instructor only access, and push that to create the repos on the
server. Students cannot access these repos yet. That is where
they will push their work.

We do not clone these yet, because they are empty.

You must \"assign\" the assignment in another step, which
involves giving the students read/write permissions. See
`ta-assign-assignment'."
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-possible-assignments)
		 nil ; predicate
		 t ; require match
		 )))

  (let* ((userids (ta-get-userids))
	 (repos (mapcar
		 (lambda (userid)
		   (ta-get-repo-name label userid))
		 userids)))

    ;; create all the conf files
    (mapcar
     (lambda (reponame) (ta-create-edit-repo-conf reponame nil '("@instructors")))
     repos)

    ;; push them all at once.
    (with-current-directory
     ta-gitolite-admin-dir
     (mygit "git push"))))


(defun ta-assign-assignment (label)
  "Assign LABEL to students.

1. This will prompt you for a LABEL, which is a directory in the
assignment dir.

2. Set repos to RW for students.

3. Update the syllabus with the assignment.

The assignment must have POINTS, CATEGORY, RUBRIC and DUEDATE
defined.  The syllabus must be in the right place:
course/syllabus.org, and it must have a categories table, and an
assignments section. This function updates the assignments
section.
"
  (interactive (list
		(ido-completing-read
		 "Label: "		 
		 (ta-get-possible-assignments)
		 nil ; predicate
		 t ; require match
		 )))

  ;; First we update the syllabus. Let us get the details from the assignment file
  (let (POINTS CATEGORY DUEDATE RUBRIC)
    (with-current-buffer (find-file-noselect
			  (expand-file-name
			   (format "%s.org" label) ;; the org-file
			   (expand-file-name label ta-course-assignments-dir)))
 
      (setq POINTS (gb-get-filetag "POINTS")
	    CATEGORY (gb-get-filetag "CATEGORY")
	    RUBRIC (gb-get-filetag "RUBRIC")
	    DUEDATE (gb-get-filetag "DUEDATE"))
    
      (unless (and POINTS CATEGORY RUBRIC DUEDATE)
	(error "You must define the points, category, duedate and rubric in the assignment file"))

      ;; Get syllabus
      (with-current-buffer (find-file-noselect
			    (expand-file-name "syllabus.org"
					      ta-course-dir))
	(save-restriction
	  (widen)
	  (beginning-of-buffer)
	  ;; This link relies on a CUSTOM_ID
	  (org-open-link-from-string "[[#assignments]]")
	  (org-narrow-to-subtree)
	  ;; we add an assignment headline, as long as here is not one already
	  (let ((entries
		 (org-map-entries
		  (lambda ()
		    (nth 4 (org-heading-components))))))
	    (unless (-contains? entries (format "assignment:%s" label))
	      ;; add new entry
	      (goto-char (point-max))
	      (insert "\n** TODO assignment:" label)
	      (org-set-tags-to ":assignment:")
	      (goto-char (point-max))
	      (org-entry-put (point) "CATEGORY" CATEGORY)
	      (org-entry-put (point) "POINTS" POINTS)
	      (org-entry-put (point) "CUSTOM_ID" label)	      
	      (org-entry-put (point) "RUBRIC" RUBRIC)
	      (org-deadline nil DUEDATE)
	      (goto-char (point-max))
	      (insert "\n")
	      (save-buffer)))))
      ;; Finally, we need to commit the syllabus change, and push it.
      (with-current-directory
       ta-course-dir
       (mygit (format "git commit syllabus.org -m \"added assignment %s" label))
       (mygit "git push"))

    
      ;; update repo permissions
      (mapcar
       (lambda (userid)
	 (ta-create-edit-repo-conf
	  (ta-get-repo-name label userid)
	  nil           ;; R
	  (list userid)))  ;; RW
       (ta-get-userids))

      ;; push them all at once.
      (with-current-directory
       ta-gitolite-admin-dir
       (mygit "git push"))

      ;; Now, give them read access on the assignment. The assignment
      ;; is created as a wild repo, so we do permissions different on
      ;; these than on other types of repos.
      (shell-command (format "ssh org-course@techela.cheme.cmu.edu perms a/%s + READERS @students" label))

      )))


(defun ta-collect(label)
  "Collect LABEL from students

1. This will prompt you for a LABEL, which is a directory in the
assignments dir.
2. Set student permission to R in each repo.conf.
3. Push the new conf files to the server.

This does not pull the repos. See `ta-pull-repos'.
"
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))

  (dolist (userid (ta-get-userids))
    (ta-create-edit-repo-conf
      (ta-get-repo-name label userid)
      (list userid) ;; R
      nil))         ;; RW

  ;; push the repo permission changes all at once.
  (with-current-directory
   ta-gitolite-admin-dir
   (mygit "git push")))


(defun ta-pull-repos (label)
  "Pull the assignment LABEL repo for each student.  This is not
a fast operation because it requires a pull for every
student. You may want to run `ta-collect' to change the
permissions of the repo to Read-only first."
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))
  (dolist (userid (ta-get-userids))
    (let* ((repo-name (ta-get-repo-name label userid))
	   (repo-dir-name (expand-file-name
		       repo-name
		       ta-root-dir))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo-name
		       ta-root-dir))))

      ;; make sure we have the root dir to work in up to the repo, but
      ;; not including. Later we clone it if it does not exist.
      (unless (file-exists-p repo-dir-name)
	(make-directory  (file-name-directory repo-dir-name) t))
      
      (if (file-exists-p repo-dir)
	  ;; we have a copy fo the work so we pull it.
	  (with-current-directory
	   repo-dir
	   ;; should perhaps consider making sure we are clean before we pull?
	   (let ((process-environment (cons *GIT_SSH* process-environment)))
	     (start-process-shell-command "git-pull"  ; process anme
					  "*git pull*" ; buffer for output
					  "git pull")))		 
	;; repo-dir did not exist. So we clone it.
	(with-current-directory
	 (file-name-directory repo-dir-name)
	 (let ((process-environment (cons *GIT_SSH* process-environment)))
	   (start-process-shell-command "git-clone"
					"*git clone*"
					(format "git clone %s@%s:%s"
						ta-course-name
						ta-course-server
						repo-name))))))))


(defun ta-return (label)
  "Return assignment LABEL for each student.

This means go into each repo, commit all changes, and push them."
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))
  (dolist (userid (ta-get-userids))
    (let* ((repo-name (ta-get-repo-name label userid))
	   (repo-dir-name (expand-file-name
		       repo-name
		       ta-root-dir))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo-name
		       ta-root-dir))))
      (with-current-directory
       repo-dir
       ;; only push if changes detected
       (if (not (string= "" (shell-command-to-string "git status --porcelain")))
	   (let ((process-environment (cons *GIT_SSH* process-environment)))
	     (start-process-shell-command
	      "ta-return"
	      "*ta return*"
	      "git add * && git commit -am \"Returning\" && git push")
	     (message "returned %s" userid))
	 (message "no changes detected for %s" userid))))))


(defun ta-grade (label)
  "Collect and pull repos for assignment LABEL. Open the grading org-file.

This is not fast. 
"
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))

  ;; set permissions to R for students
  (ta-collect label)
  (message "%s has been collected" label)
  
  ;; now pull them
  (ta-pull-repos label)
  (message "%s has been pulled" label)
  ;; Now, make org-file
  (let ((grading-file (expand-file-name
		       (format "gradebook/grading-%s.org" label)
		       ta-gitolite-admin-dir)))
    (if (file-exists-p grading-file)
	(find-file grading-file)
      ;; else, we have to make one
      (unless (file-exists-p (expand-file-name
			      "gradebook"
			      ta-gitolite-admin-dir))
	(make-directory (expand-file-name
			      "gradebook"
			      ta-gitolite-admin-dir) t))
      (find-file grading-file)
      (insert "#+TITLE: Grading
#+AUTHOR: " (user-full-name) "
#+DATE: " (format-time-string "[%Y-%m-%d %a]" (current-time)) "

* Grading for " label " [/]\n")
      (dolist (userid (sort (ta-get-userids) 'string-lessp))
	(let* ((repo-name (ta-get-repo-name label userid))
	       (repo-dir (file-name-as-directory
			  (expand-file-name
			   repo-name
			   ta-root-dir)))
	       (student-org-file (expand-file-name
			  (concat label ".org")
			  repo-dir)))
	   (if (file-exists-p student-org-file)
	       (insert (format "** TODO [[%s][%s]]\n"
			       (file-relative-name
				student-org-file
				(expand-file-name "gradebook"
						  ta-gitolite-admin-dir))
			       repo-name))
	     ;; missing org file						   
	     (insert (format "** TODO %s missing\n" repo-name)))))

      ;; loop is over. Put in last section
      (insert (format "
* Finishing up
1. Create the [[elisp:(ta-summarize \"%s\")][summary report]]
" label)
		   (format "
2. [[elisp:(ta-return \"%s\")][Return the assignments]]
" label)
		   (format "
3. [[elisp:ta-save-commit-and-push][Save and push this file]]" grading-file grading-file))
	 )))


(defun ta-update-all-student-work ()
  "Loop through all assignment repos and pull/clone them locally.

We do not check if our local copy is up to date first.  we probably should."
  (interactive)
  (let ((userids (ta-get-userids))
	(assigned-assignments (ta-get-assigned-assignments))
    (dolist (userid userids)
      (dolist (label assigned-assignments nil)
	(ta-pull-repos label))))))


;; (defun ta-save-commit-and-push ()
;;   "Save current buffer. Commit changes and push it."
;;   (interactive)
;;   (save-buffer)
;;   (mygit (format "git add %s" (buffer-file-name)))
;;   (mygit (format "git commit %s -m\"saving grade file\"" (buffer-file-name)))
;;   (mygit "git push"))


(defun ta-summarize (label)
  "Insert a summary of grades for assignment LABEL."
  (forward-line 2)
  (insert (format "#+tblname: summary-%s\n" label)
	  "| userid | grade |\n|-\n")
  (dolist (userid (sort (ta-get-userids) 'string-lessp))
    (let* ((label-dir (file-name-as-directory
		      (expand-file-name
		       label
		       ta-course-student-work-dir)))
	   (repo (f-filename (ta-get-repo-name label userid)))	  
	   (repo-dir (file-name-as-directory
			 (expand-file-name
			  repo
			  label-dir)))
	   (org-file (expand-file-name
		      (concat label ".org") repo-dir))
	   (grade))
      (message "label-dir %s
repo %s
repo-dir %s
org-file %s" label-dir repo repo-dir org-file)
      (message "looking at %s" org-file)
      (setq grade (gb-get-grade org-file))
      (insert (format "| %15s | %s |\n" (format "[[%s][%s]]" (file-relative-name org-file) userid) grade))))
  (insert "\n\n")
  ;; realign table
  (previous-line 3)
  (org-ctrl-c-ctrl-c))


(defun ta-get-categories ()
  "Read categories from the syllabus."
  (let ((table (with-current-buffer
		   (find-file-noselect (expand-file-name "syllabus.org" ta-course-dir))
		 (beginning-of-buffer)
		 (re-search-forward "#\\+tblname:\\s-*categories")
		 (forward-line)
		 (org-table-to-lisp))))
    (mapcar (lambda (x) (car x)) (cddr table))))


(defun ta-get-possible-assignments ()
  "Return list of assignments in the assignments directory."
  (with-current-directory
   ta-course-assignments-dir
   (remove-if-not
    'file-directory-p
    ;; list directories, except for . and ..
    (directory-files ta-course-assignments-dir nil "[^.{1,2}]"))))


(defun ta-get-assigned-assignments ()
  "Return a list of assignments from the syllabus.
Assignments are headings that are tagged with :assignment:.  The assignment is
a link in the heading."
  (interactive)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "syllabus.org" ta-course-dir))
    (org-mode)
    (org-map-entries
     (lambda ()
       (org-entry-get (point) "CUSTOM_ID"))
     "assignment")))


(defun ta-open-assignment (label userid)
  "Open the USERID assignment LABEL.
This will be in student-work/label/userid-label/userid-label.org."
  (interactive (list (ido-completing-read "Label: " (ta-get-assigned-assignments))
		     (ido-completing-read "Userid: " (ta-get-userids))))
  (let* ((repo (ta-get-repo-name label userid))
	 (repo-dir (file-name-as-directory
			(expand-file-name
			 repo
			 ta-root-dir))))
    (message "looking for %s %s"  repo repo-dir)
    (if (file-exists-p repo-dir)	
	(with-current-directory
	 repo-dir
	 ;; initially there may not be tracking information, so we are specific in the pull
	 (mygit "git pull origin master")
	 (find-file (concat label ".org"))
	 (grade-mode))
      ;; else. no dir. make the student dir if needed, and clone the repo
      (let ((label-dir (expand-file-name
			  label
			  ta-course-student-work-dir)))
	(unless (file-exists-p label-dir)
	  (make-directory label-dir t))
	
	(with-current-directory
	 label-dir
	 (mygit (format "git clone %s@%s:%s"
			ta-course-name
			ta-course-server
			repo))))
      ;; now open it.
      (with-current-directory
       repo-dir
       (find-file (concat label ".org"))
       (grade-mode)))))


(defun ta-show-assigned-assignments ()
 "Show assigned assignments in an org buffer."
 (interactive)
 (switch-to-buffer-other-frame (get-buffer-create "*techela-assignments*"))
 (erase-buffer)

 (dolist (label (ta-get-assigned-assignments))
   ;; get the directory if we do not have it.
   (unless (file-exists-p (expand-file-name label ta-course-assignments-dir))
     (unless (file-exists-p ta-course-assignments-dir)
       (make-directory ta-course-assignments-dir t))
     (with-current-directory
      ta-course-assignments-dir
      (mygit (format "git clone %s@%s:a/%s" ta-course-name ta-course-server label))))
   
   (let* ((org-file (concat label ".org"))
	  (org-file-path (expand-file-name
			  org-file
			  (expand-file-name label ta-course-assignments-dir))))
     (insert (format "- [[file:%s][%s]]\n" org-file-path label))))

 (org-mode))


(defun ta-save-commit-and-push ()
  "Save current buffer, commit changes, and push."
  (interactive)
  (save-buffer)
  (mygit (format "git commit %s -m \"save changes\"" (buffer-file-name)))
  (mygit "git push"))


(defun ta-status ()
  "Switch to *techela-admin* and show git status on these directories:
1. gitolite-admin
2. ../course"
  (interactive)

  (switch-to-buffer-other-frame (get-buffer-create "*techela-admin*"))
  (erase-buffer)
  (insert "#+STARTUP: showall\n")

  (with-current-directory
   ta-gitolite-admin-dir
   (let* ((git-status (shell-command-to-string "git status --porcelain"))
	  (clean (string= "" git-status))
	  (n (tq-get-num-incoming-changes)))
     (if clean
	 (insert (format "* gitolite-admin is clean (remote changes = %s)\n" n))

       ;; Dirty course
       (insert (format (concat "* gitolite-admin is "
			       (propertize "dirty" 'font-lock-face '(:foreground "red"))
			       " (remote changes = %s)
  :PROPERTIES:
  :VISIBILITY: folded
  :END:
git status:
%s") n git-status))
       (insert "

#+BEGIN_SRC emacs-lisp
 (with-current-directory ta-gitolite-admin-dir
   (mygit \"git add *\")
   (mygit \"git commit -m \\\"committing everything\\\"\")
   (mygit \"git push\"))
#+END_SRC

"))))

  (with-current-directory
   ta-course-dir
   (let* ((git-status (shell-command-to-string "git status --porcelain"))
	  (clean (string= "" git-status))
	  (n (tq-get-num-incoming-changes)))
     (if clean
	 (insert (format "* Course is clean (remote changes = %s)\n" n))

       ;; Dirty course
       (insert (format (concat "* Course is "
			       (propertize "dirty" 'font-lock-face '(:foreground "red"))
			       " (remote changes = %s)

  :PROPERTIES:
  :VISIBILITY: folded
  :END:
git status:
%s") n git-status))
       
       (insert "

#+BEGIN_SRC emacs-lisp
 (with-current-directory ta-course-dir
   (mygit \"git add *\")
   (mygit \"git commit -m \\\"committing everything\\\"\")
   (mygit \"git push\"))
#+END_SRC

"))))

  ;;; now we get each assignment
  (insert "* Assignment directory statuses\n")
  (dolist (label (ta-get-possible-assignments))
    (with-current-directory
     (expand-file-name
      label ta-course-assignments-dir)
          
    (let ((git-status (shell-command-to-string "git status --porcelain")))
      (if (string= "" git-status)
	  (insert "** " label
		  (if (-contains? (ta-get-assigned-assignments) label)
		      " (assigned)"
		    " (not assigned)")
		  " is clean")
	(insert "** " label " is " (propertize "dirty\n" 'font-lock-face '(:foreground "red"))
		(shell-command-to-string "git status")
		(format "
#+BEGIN_SRC emacs-lisp
 (with-current-directory (expand-file-name \"%s\" ta-course-assignments-dir)
   (mygit \"git add *\")
   (mygit \"git commit -m \\\"committing everything\\\"\")
   (mygit \"git push\"))
#+END_SRC
" label))
		))
    (insert (format "\n    [[file:%s][%s]]"
		    (expand-file-name
		     label ta-course-assignments-dir)
		    label) "\n")))

    (insert "
* Menu of options

** Admin Actions

- [[elisp:(find-file ta-gitolite-admin-dir)][Open the admin directory]]

** Course Actions

- [[elisp:(find-file ta-course-dir)][Open the course directory]]

- [[elisp:(ta-email \"*all*\")][Email the class]]    

- [[elisp:(find-file (expand-file-name \"roster.dat\" ta-gitolite-admin-dir))][Open the roster.dat]]   [[elisp:(find-file (expand-file-name \"roster.org\" ta-gitolite-admin-dir))][Open the roster.org]]
- [[elisp:ta-update-roster][Update the roster]] (do this after you change roster.dat)

** Assignment Actions

- [[elisp:(find-file ta-course-assignments-dir)][Open the assignments directory]]
- [[elisp:(find-file ta-course-student-work-dir)][Open student work directory]]
- [[elisp:ta-pull-repos][Update student repos]] (pulls them all locally.)

- [[elisp:ta-create-assignment][Create or edit an assignment]]
- [[elisp:ta-create-assignment-repos][Create class repos for an assignment]] (no student access until you assign it.)

- [[elisp:ta-assign-assignment to class][Assign an assignment]] (give students RW access)
- [[elisp:ta-collect][Collect an assignment from class]] (change students to R access. Does not pull.)
- [[elisp:ta-pull-repos][Pull an assignment from class]] (get local copies of assignment. Does not change permissions.)
- [[elisp:ta-return][Return an assignment to class]] (push local copies to server)

- [[elisp:ta-grade][Grade an assignment for class]] (collect and pull repos. create grading list)

- [[elisp:ta-show-assigned-assignments][Show list of assigned assignments]]

*** Individual Student Actions

- [[elisp:ta-assign-to][Assign assignment to a student. give RW access]]
- [[elisp:ta-collect-from][Collect assignment from a student. Make R access]]
- [[elisp:ta-open-assignment][Open a student assignment. Pulls first.]]
- [[elisp:ta-return-to][Return your changes in an assignment to a student]]

- [[elisp:ta-email][Email a student]]

")
    (goto-char (point-min))
    (org-mode))


  
(provide 'techela-admin)

;;; techela-admin.el ends here

