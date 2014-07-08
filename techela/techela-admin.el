;;; techela-admin.el --- Techela Administration functions
;; 

;;; Commentary:
;; Techela is a Technology enhanced learning and assessment
;; environment.  A techela course is hosted on a gitolite server.  The
;; public course is available at
;; coursename@techela.cheme.cmu.edu:course.
;; See README.org and FAQ.org

(require 'cl)
(require 'csv)
(require 'techela-roster)
(require 'techela-grade)

;;; Code:

(defvar ta-course-server "techela.cheme.cmu.edu")
(defvar ta-course-name nil "The course currently in action.")
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
     (if (boundp 'techela-admin-courses) ; this means it is in custom.el
	 (mapcar (lambda (x) (car x)) techela-admin-courses)
       '()))))

  (setq ta-course-name course-name)

  ;; load directories if they exist. We only need ta-root-dir,
  ;; everything else is derived.
  (when (and (boundp 'techela-admin-courses) techela-admin-courses)
    (let ((vars (cdr (assoc ta-course-name techela-admin-courses))))
      (if vars
	  (setq ta-root-dir (nth 0 vars))
	;; no course found, set these to nil so we have to start over
	(setq ta-root-dir nil))))

  (unless (and (boundp 'ta-root-dir) ta-root-dir)
    (setq ta-root-dir
	  (file-name-as-directory
	   (ido-read-directory-name "Enter course admin directory: " nil
				    (format "~/Desktop/%s" ta-course-name)))))

  ;; we should have a ta-root-dir now, so we set all the derived variables
  (setq ta-gitolite-admin-dir (file-name-as-directory
			       (expand-file-name
				"gitolite-admin"
				ta-root-dir))
	
	ta-roster (expand-file-name "roster.dat" ta-gitolite-admin-dir)
	
	ta-course-assignments-dir (file-name-as-directory
				  (expand-file-name
				   "assignments"
				   ta-gitolite-admin-dir))
	
	ta-course-dir (file-name-as-directory
		       (expand-file-name
			"course"
			ta-root-dir))
	
	ta-course-student-work-dir (file-name-as-directory
				    (expand-file-name
				     "student-work"
				     ta-root-dir))
	
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

  ;; update the techela-admin-courses with the new root directory
  (if (and (boundp 'techela-admin-courses) techela-admin-courses)
      (add-to-list 'techela-admin-courses (cons ta-course-name (list ta-root-dir)))
    (setq techela-admin-courses (list (cons ta-course-name (list ta-root-dir)))))

  ;; and save the variable so we can use it next time.
  (customize-save-variable 'techela-admin-courses techela-admin-courses)

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
  
(defun ta-create-edit-repo-conf (reponame &optional R RW RW+)
  "Create a repo file for REPONAME with permissions.
R is a list of users with Read permission
RW is a list of users with Read/Write permission
RW+ is a list of users with force permission.

This creates a repo.conf file at conf/repos/repo.conf, then
commits it.  No push is done to create the repo on the
server.  This is intentional, as sometimes you may want to create
several conf files before pushing.

An existing conf file is overwritten, which allows you to change
permissions on an existing repo."

  (let ((repo-conf-dir (file-name-as-directory
			(expand-file-name "conf/repos" ta-gitolite-admin-dir))))
    (with-current-directory
     repo-conf-dir
     ;; write out repo.conf to ta-gitolite-admin-dir/conf/repos/repo.conf
     (let ((repo-file (expand-file-name (concat reponame ".conf") repo-conf-dir)))
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
	   ;; add conf/repos/repo.conf
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
			(expand-file-name "conf/repos" ta-gitolite-admin-dir))))
    (with-current-directory
     repo-conf-dir
     (mygit "git push"))))


(defun ta-pull ()
  "Run git pull in the current directory."
  (interactive)
  (mygit "git pull"))


(defun ta-return ()
  "Commit directory, and push it."
  (interactive)
  (mygit "git add *")
  (mygit "git commit -m \"returning work\"")
  (mygit "git push"))


(defun ta-assign-to (label userid)
  "Assign assignment LABEL to USERID."
  (interactive
   (list
    (ido-completing-read "Label: " (ta-get-assigned-assignments) nil t)
    (ido-completing-read "Userid: " (ta-get-userids))))
  ;; first, create the repo and push it.
  (let* ((repo-name (concat ta-course-name "-" userid "-" label))
	 (conf-repo-dir (file-name-as-directory
			 (expand-file-name
			  "conf/repos"
			  ta-gitolite-admin-dir)))
	 (student-dir (file-name-as-directory
		       (expand-file-name
			userid
			ta-course-student-work-dir)))
	 (student-repo-dir (file-name-as-directory
			    (expand-file-name
			     repo-name
			     student-dir)))
	 (assignment-dir (file-name-as-directory
			  (expand-file-name
			   label
			   ta-course-assignments-dir))))

    ;; this pushes 
    (ta-create-edit-repo reponame nil '(userid))

    ;; remote repo exists now. we need to check if local repo
    ;; exists. We assume if it exists, it has what it needs in it.
    (unless (file-exists-p student-repo-dir)
      (copy-directory assignment-dir student-repo-dir nil t))

    ;; insert name into student org-file
    (let ((tbuf (find-file-noselect (expand-file-name
				     (format "%s.org" label) ;; the org-file
				     student-repo-dir))))
      (with-current-buffer tbuf
	(goto-char (point-min))
	(insert (format "#+USERID: %s\n#+AUTHOR: %s\n#+ASSIGNMENT: %s\n"
			userid
			(plist-get (cdr (assoc userid (ta-roster))) :name) label)))
      (set-buffer tbuf)
      (save-buffer)
      (kill-buffer tbuf))
      
    ;; now make it a git repo and push it remotely
    (with-current-directory
     student-repo-dir
     (mygit "git init")
     (mygit "git add *")
     (mygit "git commit -m \"initial commit\"")
     (mygit (format "git remote add origin \"%s@%s:%s.git\""
		    ta-course-name ta-course-server
		    (format "%s-%s-%s" ta-course-name userid label)))
     (mygit "git push -u origin master"))))


(defun ta-create-assignment-repos (label)
  "Create an assignment LABEL.

1. This will prompt you for a LABEL, which is a directory in the
assignment dir.

2. Create repo confs for all students in the course, with
instructor only access, and push that to create the repos on the
server. Students cannot access these repos yet.

3. Create the local repos by copying the assignment dir to the
student dir, making a local git repo.

4. push the local repos to the server.

You must \"assign\" the assignment in another step, which
involves giving the students read/write permissions.

This step is slow right now, so you would normally do this in
advance of assigning, which is comparatively fast."
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (with-current-directory
		  ta-course-assignments-dir
		  (remove-if-not
		   'file-directory-p
		   ;; list directories, except for . and ..
		   (directory-files ta-course-assignments-dir nil "[^.{1,2}]")))
		 nil ; predicate
		 t ; require match
		 )))

  (let* ((userids (ta-get-userids))
	 (repos (mapcar (lambda (userid) (format "%s-%s-%s" ta-course-name userid label)) userids)))

    ;; create all the conf files
    (mapcar (lambda (reponame) (ta-create-edit-repo-conf reponame nil '("@instructors"))) repos)

    ;; push them all at once.
    (with-current-directory
     ta-gitolite-admin-dir
     (mygit "git push"))

    ;; Now we want to create the local directories
    ;; the assignment is in ta-course-assignments-dir/label
    ;; we want to copy it to ta-course-student-work-dir/userid/label

    (with-current-directory
     ta-course-student-work-dir
     (message "working in %s" default-directory)
     ;; make sure each directory exists.
     (mapcar (lambda (userid)
	       (let ((assignment-source (expand-file-name label ta-course-assignments-dir))
		     (assignment-dest (expand-file-name
				       (concat ta-course-name "-" userid "-" label)
				       (expand-file-name userid ta-course-student-work-dir))))
		 (unless (file-exists-p assignment-dest)
		   (copy-directory assignment-source assignment-dest nil t)

		   ;; Now we really should update the assignment-file. Let us get the details from the file
		   (let (POINTS CATEGORY DUEDATE)
		     (with-current-buffer (find-file-noselect (expand-file-name
							       (format "%s.org" label) ;; the org-file
							       (expand-file-name label ta-course-assignments-dir)))
		       (let ((info (org-element-map
				       (org-element-parse-buffer 'element) 'keyword
				     (lambda (keyword) (cons (org-element-property :key keyword)
							     (org-element-property :value keyword))))))
			 (setq POINTS (cdr (assoc "POINTS" info))
			       CATEGORY (cdr (assoc "CATEGORY" info))
			       DUEDATE (cdr (assoc "DUEDATE" info)))))

		     ;; insert name into document
		     (let ((tbuf (find-file-noselect (expand-file-name
							       (format "%s.org" label) ;; the org-file
							       assignment-dest))))
		       (with-current-buffer tbuf
			 (goto-char (point-min))
			 (insert (format "#+USERID: %s\n#+AUTHOR: %s\n#+ASSIGNMENT: %s\n"
					 userid
					 (plist-get (cdr (assoc userid (ta-roster))) :name) label)))
		       (set-buffer tbuf)
		       (save-buffer)
		       (kill-buffer tbuf)))
    
		   ;; now make it a git repo and push it remotely
		   (with-current-directory
		    (file-name-as-directory assignment-dest)
		    (mygit "git init")
		    (mygit "git add *")
		    (mygit "git commit -m \"initial commit\"")
		    (mygit (format "git remote add origin \"%s@%s:%s.git\""
				   ta-course-name ta-course-server
				   (format "%s-%s-%s" ta-course-name userid label)))
		    (mygit "git push -u origin master")))))
	     userids))))


(defun ta-assign-assignment (label)
  "Assign LABEL to students.

1. This will prompt you for a LABEL, which is a directory in the
assignment dir.

2. Set repos to RW for students"
  (interactive (list
		(ido-completing-read
		 "Label: "		 
		 (ta-get-possible-assignments)
		 nil ; predicate
		 t ; require match
		 )))


  ;; update reponames
  (mapcar
   (lambda (userid)
     (ta-create-edit-repo-conf
      (format "%s-%s-%s" ta-course-name userid label) ;; the reponame
      nil           ;; R
      `(,userid)))  ;; RW
     (ta-get-userids))

  ;; push them all at once.
  (with-current-directory
   ta-gitolite-admin-dir
   (mygit "git push"))

  ;; Now we really should update the syllabus. Let us get the details from the file
  (let (POINTS CATEGORY DUEDATE)
    (with-current-buffer (find-file-noselect
			  (expand-file-name
			   (format "%s.org" label) ;; the org-file
			   (expand-file-name label ta-course-assignments-dir)))
      (let ((info (org-element-map
		      (org-element-parse-buffer 'element) 'keyword
		    (lambda (keyword) (cons (org-element-property :key keyword)
					    (org-element-property :value keyword))))))
	(setq POINTS (cdr (assoc "POINTS" info))
	      CATEGORY (cdr (assoc "CATEGORY" info))
	      DUEDATE (cdr (assoc "DUEDATE" info)))))

    ;; Get syllabus
    (with-current-buffer (find-file-noselect
			  (expand-file-name "syllabus.org"
					    ta-course-dir))
      (save-restriction
	(widen)
	(beginning-of-buffer)
	(re-search-forward "* Assignments")
	(org-narrow-to-subtree)

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
	    (goto-char (point-max))
	    (org-entry-put (point) "POINTS" POINTS)
	    (goto-char (point-max))
	    (org-deadline nil DUEDATE)
	    (save-buffer)))))
    ;; Finally, we need to commit the syllabus change, and push it.
    (with-current-directory
     ta-course-dir
     (mygit (format "git commit syllabus.org -m \"added assignment %s" label))
     (mygit "git push"))
			    
    ))


(defun ta-collect(label)
  "Collect LABEL from students

1. This will prompt you for a LABEL, which is a directory in the
assignments dir.
2. Set student permission to R in each repo.conf.
3. Push the new conf files to the server.

This does not pull the repos.
"
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))
  ;; TODO use dolist instead of side-effect in mapcar
  ;; update reponames
  (mapcar
   (lambda (userid)
     (ta-create-edit-repo-conf
      (format "%s-%s-%s" ta-course-name userid label) ;; the reponame
      (list userid) ;; R
      nil)) ;; RW
     (ta-get-userids))

  ;; push them all at once.
  (with-current-directory
   ta-gitolite-admin-dir
   (mygit "git push")))


(defun ta-pull-repos (label)
  "Pull the assignment LABEL repo for each student.  This is not
a fast operation because it requires n pulls. It assumes you have
run `ta-collect'."
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))
  (dolist (userid (ta-get-userids) nil)
    (let* ((repo-name (concat ta-course-name "-" userid "-" label))
	   (student-dir (file-name-as-directory
			 (expand-file-name
			  userid
			  ta-course-student-work-dir)))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo-name
		       student-dir))))
      (if (file-exists-p repo-dir)
	  (with-current-directory
	   repo-dir
	   ;; should perhaps consider making sure we are clean before we pull?
	   (mygit "git pull"))
	;; repo-dir did not exist. So we clone it.
	(with-current-directory
	 student-dir
	 (mygit (format "git clone %s@%s:%s" ta-course-name
			ta-course-server
			reponame)))))))


(defun ta-update-all-student-work ()
  "Update all student work.

We do not check if our local copy is up to date first.  we probably should."
  (interactive)
  (let ((userids (ta-get-userids))
	(assigned-assignments (ta-get-assigned-assignments))
	(student-assignment-dir)
	(student-assignment)
	(student-dir)
	(value))
    (dolist (userid userids value)
      (dolist (assignment assigned-assignments nil)
	(setq student-dir (expand-file-name
			   userid
			   ta-student-work-dir))
	(setq student-assignment (format "%s-%s-%s" ta-course-name userid assignment))
	(setq student-assignment-dir (file-name-as-directory
				       student-assignment
				       student-dir))

	;; check if the directory exists
	(if (file-exists-p student-assignment-dir)
	    ;; we have a directory. We should go in and pull
	    (with-current-directory
	     student-assignment-dir
	     (mygit "git pull"))
	  ;; We did not find the directory. We should clone it.
	  (with-current-directory
	   student-dir
	   (mygit (format "git clone %s@%s:%s"
			  ta-course-name ta-course-server student-assignment))))))))


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
    (let* ((repo-name (concat ta-course-name "-" userid "-" label))
	   (student-dir (file-name-as-directory
			 (expand-file-name
			  userid
			  ta-course-student-work-dir)))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo-name
		       student-dir))))
      (with-current-directory
       repo-dir
       (mygit "git add *")
       (mygit "git commit -am \"Returning\"")
       (mygit "git push")))))



(defun ta-grade (label)
  "Collect and pull repos for assignment LABEL. Open the grading org-file"
  (interactive (list
		(ido-completing-read
		 "Label: "
		 (ta-get-assigned-assignments)
		 nil ; predicate
		 t ; require match
		 )))

  ;; set permissions to R for students
  (ta-collect label)

  ;; now pull them
  (ta-pull-repos label)

  ;; Now, make org-file
  (let ((grading-file (expand-file-name
		       (format "gradebook/%s.org" label)
		       ta-gitolite-admin-dir)))
    (if (file-exists-p grading-file)
	(find-file grading-file)
      ;; else, we have to make one
      (find-file grading-file)
      (insert "#+TITLE: Grading
#+AUTHOR: " (user-full-name) "
#+DATE: " (format-time-string "[%Y-%m-%d %a]" (current-time)) "

* Grading for " label " [/]\n")
      (dolist (userid (ta-get-userids))
	(let* ((repo-name (format "%s-%s-%s" ta-course-name
				  userid
				  label))
	       (student-dir (file-name-as-directory
			     (expand-file-name
			      userid
			      ta-course-student-work-dir)))
	       
	       (repo-dir (file-name-as-directory
			  (expand-file-name
			   repo-name
			   student-dir)))
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
3. [[elisp:(progn (save-buffer) (mygit \"git add %s\") (mygit \"git commit %s -m \\\"save and commit\\\") (mygit \"git push\"))][Save and push this file]]" grading-file grading-file))
	 )))

(defun ta-summarize (label)
  "Insert a summary of grades for assignment LABEL"
  ;; TODO
  )



(defun ta-get-categories ()
  "Read categories from the syllabus."
  (let ((table (with-current-buffer
		   (find-file-noselect (expand-file-name "syllabus.org" ta-course-dir))
		 (beginning-of-buffer)
		 (re-search-forward "#\\+tblname: categories")
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
    (mapcar (lambda (entry)
	      (string-match "assignment:\\(.*\\)" entry)
	      (match-string 1 entry))
	    (org-map-entries
	     (lambda ()
	       (nth 4 (org-heading-components)))
	     "assignment"))))


(defun ta-open-assignment (userid label)
  "Open the USERID assignment LABEL."
  (interactive (list (ido-completing-read "Userid: " (ta-get-userids))
		     (ido-completing-read "Label: " (ta-get-assigned-assignments))))
  (with-current-directory
   (expand-file-name
    (concat ta-course-name "-" userid "-" label)
    (expand-file-name
     userid
     ta-course-student-work-dir))
   ;; initially there may not be tracking information, so we are specific in the pull
   (mygit "git pull origin master")
   (find-file (concat label ".org"))
   (grade-mode)))


(defun ta-show-assigned-assignments ()
 "Show assigned assignments in an org buffer."
 (interactive)
 (switch-to-buffer-other-frame (get-buffer-create "*techela-assignments*"))
 (erase-buffer)
 (mapcar (lambda (label)
	   (let* ((label-org (concat label ".org"))
		 (label-file (expand-file-name
			      label-org
			      (expand-file-name label ta-course-assignments-dir))))
	     
	     (insert (format "- [[file:%s][%s]]\n" label-file label ))))
	 (ta-get-assigned-assignments))
 (org-mode))


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
	 (insert (format "* gitolite-admin is clean (remote changes = %s)" n))

       ;; Dirty course
       (insert (format "* gitolite-admin is dirty (remote changes = %s)
  :PROPERTIES:
  :VISIBILITY: folded
  :END:

%s" n git-status))
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
	 (insert (format "* Course is clean (remote changes = %s)" n))

       ;; Dirty course
       (insert (format "* Course is dirty (remote changes = %s)

  :PROPERTIES:
  :VISIBILITY: folded
  :END:

%s" n git-status))
       
       (insert "

#+BEGIN_SRC emacs-lisp
 (with-current-directory ta-course-dir
   (mygit \"git add *\")
   (mygit \"git commit -m \\\"committing everything\\\"\")
   (mygit \"git push\"))
#+END_SRC

"))))

  (insert "* TODO Student repos
- write a function to check the repos?
")

    (insert "
* Menu of options

** Admin

- [[elisp:(find-file ta-gitolite-admin-dir)][Open the admin directory]]

** Course

- [[elisp:(find-file ta-course-dir)][Open the course directory]]

- [[elisp:(ta-email \"*all*\")][Email the class]]
- [[elisp:ta-email][Email a student]]

- [[elisp:(find-file (expand-file-name \"roster.dat\" ta-gitolite-admin-dir))][Open the roster.dat]]
- [[elisp:(find-file (expand-file-name \"roster.org\" ta-gitolite-admin-dir))][Open the roster.org]]

** Assignments

- [[elisp:ta-create-assignment-repos][Create repos for an assignment]]
- [[elisp:ta-assign-assignment][Assign an assignment]]
- [[elisp:ta-collect][Collect an assignment]]
- [[elisp:ta-pull-repos][Pull an assignment]]
- [[elisp:ta-return][Return an assignment]]

- [[elisp:ta-open-assignment][Open student assignment]]

- [[elisp:ta-show-assigned-assignments][Show assigned assignments]]

- [[elisp:(find-file ta-course-assignments-dir)][Open the assignments directory]]
- [[elisp:(find-file ta-course-student-work-dir)][Open student work directory]]

** TODO Reports
Stay tuned for this.
")
    (org-mode))

  
(provide 'techela-admin)

;;; techela-admin.el ends here

