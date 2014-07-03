;;; techela-admin.el --- Techela Administration functions
;; 

;;; Commentary:
;; Techela is a Technology enhanced learning and assessment
;; environment. A techela course is hosted on a gitolite server. The
;; public course is available at
;; coursename@techela.cheme.cmu.edu:course. See file:README.org.

(require 'magit)
(require 'csv)

;;; Code:

(defvar ta-course-server "techela.cheme.cmu.edu")
(defvar ta-course-name nil "The course currently in action")

(defun techela-admin (course-name)
  "Open the course dashboard for TA-COURSE-NAME.

The root directory is retrieved from custom.el if it exists,
otherwise you are prompted for a location to download the course
to, and that is saved in custom.el.
"
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
	
	ta-course-assignents-dir (file-name-as-directory
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
  
(defun ta-pull ()
  "Run git pull in the current directory."
  (interactive)
  (mygit "git pull"))


(defun ta-return ()
  "commit directory, and push it."
  (interactive)
  (mygit "git add *")
  (mygit "git commit -m \"returning work\"")
  (mygit "git push"))


(defun ta-create-edit-repo (reponame &optional R RW RW+ push)
  "create a repo file with permissions.
R is a list of users with Read permission
RW is a list of users with Read/Write permission
RW+ is a list of users with force permission.

This creates a repo.conf file at conf/repos/repo.conf, then
commits it. If PUSH is non-nil, then push to the server. That
creates the repo on server side. This allows you to separate
creating the conf files and then doing one push.

An existing conf file is overwritten, which allows you to change
permissions on an existing repo."
  
  (with-current-directory
     ta-gitolite-admin-dir
     ;; write out repo.conf to ta-gitolite-admin-dir/conf/repos/repo.conf
     (let ((repo-file (expand-file-name (concat reponame ".conf") "conf/repos")))
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
	   (mygit (format "git add %s" (file-relative-name repo-file ta-gitolite-admin-dir)))
     
	   (mygit (format "git commit %s -m \"create/edit %s"
			  (file-relative-name repo-file ta-gitolite-admin-dir)
			  reponame))
	   
	   (when push	    
	     (mygit "git push"))
	   (tq-log "--------------------------------------------------------------\n"))))

(defun ta-assign (label userid)
  "Assign an assigment LABEL to USERID.

This means
1. create a repo with the assignment name giving USERD RW access
2. make local copy of the repo
3. copy files into it
4. commit them and push to the server, so that students can access them."
  
  (let* ((reponame (concat ta-course-name "-" userid "-" label))

	 ;; ta-course-student-work-dir/userid/
	 (student-dir (file-name-as-directory
		       (expand-file-name userid ta-course-student-work-dir)))

	 ;; ta-course-student-work-dir/userid/reponame
	 (student-repo-dir (file-name-as-directory
			    (expand-file-name
			     reponame student-dir)))
	 
	 ;; ta-course-assignents-dir/label
	 (assignment-dir (file-name-as-directory
			  (expand-file-name label ta-course-assignents-dir))))

    (tq-log "---------------------------- ta-assign ----------------------------\n")

    ;; make the repo with userid with RW. Make sure to push
    (ta-create-edit-repo reponame nil (list userid) nil t)

    (unless (file-exists-p student-dir)
	(make-directory student-dir t))
    
    (unless (file-exists-p student-repo-dir)
      (with-current-directory
       student-dir
       
       (mygit (format "git clone %s@%s:%s"
		      ta-course-name
		      ta-course-server
		      reponame))))
    
    ;; now, copy files from assignments/label to the new repo
    (tq-log "copying %s to %s" assignment-dir student-repo-dir)
    (copy-directory assignment-dir student-repo-dir nil nil t)

    ;; stage contents to git repo then commit and push
    ;; we have to switch temporarily into this directory so we use the right git repo
    (with-current-directory
     student-repo-dir
     ;; at one time I though i needed to set this branch. now it does
     ;; not seem necessary
      ;(mygit "git branch --set-upstream master origin/master")
     (mygit "git add *")
     (mygit "git commit -am \"added files\"" )
     (mygit "git push origin master")
     (tq-log "-----------------------------------------------------------------\n")
     t
     )))


(defun ta-collect (label userid &optional pull)
  "Set the repo corresponding to LABEL to Read only for USERID.
if PULL is non-nil, then pull the content. That makes making the
repos read-only very fast, at the expense of needing a second
pull step."
  (let* ((reponame (concat ta-course-name "-" userid "-" label))
         ;; absolute path to student repo
	 (student-repo-dir (file-name-as-directory
			    (expand-file-name
			     reponame
			     (expand-file-name userid ta-course-student-work-dir)))))

    ;; TODO what about push here? to change permissions?
    ;; make the repo with userid with R permissions
    (ta-create-edit-repo reponame (list userid) nil nil t)
    
    ;; pull current work
    (when pull
      (with-current-directory
       student-repo-dir
       (mygit "git pull")))))



(defun ta-get-andrewids ()
  "Return list of andrewids from a roster file."
  (with-temp-buffer
    (insert-file-contents ta-roster)
    (let ((contents (csv-parse-buffer nil)))
      (mapcar (lambda (x) (nth 1 x)) contents))))


;; TODO figure out best push/pull places
(defun ta-assign-to-class (label)
  "Assign LABEL to each andrewid in the roster file. Each user
gets their own repo."
  (interactive "sLabel: ")
  (mapcar (lambda (andrewid) (ta-assign label andrewid)) (ta-get-andrewids)))

;; TODO figure out best push/pull places
(defun ta-collect-from-class (label)
  "Collect LABEL for each andrewid in the roster file. Sets each
repo to read-only and pulls the content."
  (interactive "sLabel: ")
  (mapcar (lambda (andrewid) (ta-collect label andrewid)) (ta-get-andrewids)))


;; (defun ta-class-assign (label userids)
;;   "Assign a class assigment LABEL to USERIDS.

;; USERIDS is a list of userids

;; A class assignment does not have userid in the name, it is just the label. It is also cloned to ta-course-class-work-dir.

;; This means
;; 1. create a repo with the class assignment name giving USERD RW access
;; 2. make local copy of the repo
;; 3. copy files into it
;; 4. commit them and push to the server, so that students can access them."
  
;;   (let* ((reponame label)

;; 	 ;; ta-course-class-work-dir/reponame
;; 	 (class-repo-dir (file-name-as-directory
;; 			  (expand-file-name
;; 			   reponame
;; 			   ta-course-class-work-dir)))

;; 	 ;; ta-course-assignents-dir/label
;; 	 (assignment-dir (file-name-as-directory
;; 			  (expand-file-name label ta-course-assignents-dir)))

;; 	 ;; We run this after creating the repo
;; 	 (clone-cmd (format "git clone %s@%s:%s %s"
;; 			    ta-course-name
;; 			    ta-course-server
;; 			    reponame
;; 			    class-repo-dir)))

;;     ;; make the repo with userid with RW
;;     (ta-create-edit-repo reponame nil userids nil)

;;     ;; now, we need to clone the repo
;;     (unless (file-exists-p class-repo-dir)
;;       (message "running clone cmd: %s" clone-cmd)
;;       (shell-command clone-cmd))

;;     ;; now, copy files from assignments/label to the new repo
;;     (message "copying %s to %s" assignment-dir class-repo-dir)
;;     (copy-directory assignment-dir class-repo-dir nil nil t)

;;     ;; stage contents to git repo then commit and push
;;     ;; we have to switch temporarily into this directory so we use the right git repo
;;     (let ((default-directory class-repo-dir))
;;       ;; set tracking for branch
;;       (message "running commands in %s" default-directory)
;;       (shell-command "git branch --set-upstream master origin/master")
;;       (shell-command "git add *")
;;       (shell-command "git commit -am \"added files\"" )
;;       (shell-command "git push origin master"))))



;; (defun ta-class-collect (label userids)
;;   "Set the class repo corresponding to LABEL to Read only for
;; USERIDS.  Then pull the content."
;;   (let* ((reponame label)
;;          ;; absolute path to student repo
;; 	 (class-repo-dir (file-name-as-directory
;; 			    (expand-file-name
;; 			     reponame
;; 			     ta-course-class-work-dir))))
	 
;;     ;; make the repo with userid with R permissions
;;     (ta-create-edit-repo reponame userids nil nil)

;;     ;; this may be done separately so the collection is fast
;;     (let ((default-directory class-repo-dir))
;;       (shell-command "git pull"))))




(defun ta-grading-list (label)
  "Print a grading list of org-mode links."
  (interactive "sLabel: ")
  (let* ((USERIDS (ta-get-andrewids))
	 (reponame)
	 (orgfile (concat label ".org"))
	 (pdffile (concat label ".pdf")))
    (mapcar
     (lambda (userid)
       (setq reponame (expand-file-name
		       (concat ta-course-name "-" userid "-" label) ; reponame
		       (expand-file-name userid
					 ta-course-student-work-dir)))

       (if (file-exists-p (expand-file-name orgfile reponame))
	   (princ (format "- [ ] [[%s][%s]]\n" (expand-file-name orgfile reponame) reponame))
	 (princ (format "- [ ] %s missing\n" reponame))))
     USERIDS)))


(defun ta-status ()
  "Switch to *techela-admin* and show git status on these directories:
1. gitolite-admin
2. ../course
"
  (interactive)
  (winner-mode 1)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*techela-admin*"))
  (erase-buffer)

  (with-current-directory ta-gitolite-admin-dir
    (insert (format "%s status\n" default-directory))
    (insert (shell-command-to-string "git status")))

  (insert "\n
================================================================\n")

  (with-current-directory ta-course-dir
    (insert (format "%s status\n" default-directory))
    (insert (shell-command-to-string "git status")))
  
  (split-window-horizontally)
  (other-window 1)
  (find-file ta-gitolite-admin-dir)
  (split-window-vertically)
  (other-window 1)
  (find-file ta-course-student-work-dir)
  (message "C-c leftarrow to get back to your previous window."))
  
(provide 'techela-admin)

;;; techela-admin.el ends here

