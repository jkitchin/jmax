;;; techela.el --- functions for students to use in techela courses

;;; Commentary:
;; A techela course has a course-name, and is served on a techela server at
;; git clone course-name@techela.cheme.cmu.edu:course
;;

;;; Code:

(require 'net-utils)
(require 'f)
(require 'techela-utils)
(require 'techela-setup)
(require 'techela-grade)

(defvar tq-git-server "techela.cheme.cmu.edu"
  "The git server where techela courses are served.")

(defvar tq-root-directory nil
  "Location to clone the course and student work to.  This location is not a git repository, but will be the name of the course.")

(defvar tq-course-directory nil
  "Directory where the course content is.  This will be a git repository.  It is inside `tq-root-directory'.")

(defvar tq-current-course nil "Store value of current course.")

(defvar tq-userid nil "Global variable to store a course userid.")

(defvar tq-course-files nil "List of org-files that constitute the course.")

(defun techela (course)
  "Open COURSE.
If you have not registered for the course, you will be prompted
for setup information.

The course should exist at COURSE@techela.cheme.cmu.edu:course

The user ssh.pub key must be registered in the course."
  (interactive
   (list
    (ido-completing-read
     "Course name: "
     (tq-config-get-user-courses))))
  
  ;; Set this for the current session
  (setq tq-current-course course)

  ;; initialize to nil, just in case they were previously set
  (setq tq-root-directory (file-name-as-directory
			   (expand-file-name
			    course
			    (expand-file-name "~/techela"))))

  (setq tq-course-directory (expand-file-name "course" tq-root-directory)
	tq-config-file (expand-file-name ".techela" tq-root-directory))

  ;; make root directory if needed, including parents
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory t))
  
  ;; load directories to variables if they exist
  (let ((data (tq-config-read-data)))
   
    (unless (setq tq-userid (gethash "userid" data))
      (setq tq-userid (read-from-minibuffer "Enter userid: "))
      (puthash "userid" tq-userid data)
      (tq-config-write-data data))

    (ta-setup-user)
    (ta-setup-ssh)

    ;; clone course if we need it. This will be in a repo called "course"
    ;; do not clone if the directory exists.
    (unless (and tq-course-directory (file-exists-p tq-course-directory))
      (let ((default-directory (file-name-as-directory tq-root-directory)))
	(mygit (format "git clone git://%s/course" tq-git-server)))))
  
  ;; let user know if an update is needed
  (when (> (tq-get-num-incoming-changes) 0)
    (message "%s is out of date. Please wait while I update it" course)
    (tq-update-course))

  ;; finally open the syllabus, with current version from server.
  (with-current-directory
   tq-course-directory
   (mygit "git checkout origin/master -- syllabus.org"))
  (find-file (expand-file-name "syllabus.org" tq-course-directory))
  (tq-clean-line-endings)
  (save-buffer)
  (read-only-mode 1)
  
  (techela-mode)
  (setq org-id-extra-files (files-in-below-directory tq-course-directory)))


(defun tq-get-assignment (label)
  "Clone the repo corresponding to LABEL and open the directory."
  (interactive "sLabel: ")
  (let ((student-repo-dir (file-name-as-directory
			   (expand-file-name
			    label
			    tq-root-directory))))
    (if (file-exists-p student-repo-dir)
	;; This means we have a copy. We should check if it is up to date
	(with-current-directory
	 student-repo-dir
	 (if (not (string= "" (shell-command-to-string
			       "git status --porcelain")))
	     ;; There are some local changes. We commit them, and pull
	     (progn
	       (message "Remote changes found. Please wait while I get them.")
	       (shell-command "git commit -am \"my changes\"")
	       (mygit "git pull"))
	   ;; we were clean. Let's pull anyway to get remote changes.
	   (message "Checking for remote changes")
	   (mygit "git pull"))
	    
	 ;; now, open the file
	 (find-file (expand-file-name
		     (concat label ".org")
		     student-repo-dir)))
    ;; The repo does not exist, so we make it by cloning it.
      (let ((default-directory tq-root-directory)
	    (repo (format "assignments/%s" label)))
	;; clone and open label.org
	(tq-clone-repo repo)
	;; we need to reset the remotes now
	(with-current-directory
	 student-repo-dir
	 (mygit "git remote rename origin src")
	 (mygit
	  (format "git remote add origin %s@%s:student-work/%s/%s-%s"
		  tq-current-course
		  tq-git-server
		  label
		  tq-userid
		  label)))
	(find-file (expand-file-name (concat label ".org") label))))))


(defun  tq-turn-it-in ()
  "Save all buffers, add files, create a SYSTEM-INFO file, commit them and push.

Check *techela log* for error messages."
  (interactive)
  (save-some-buffers t t) ; make sure all buffers are saved
  (tq-insert-system-info) ; creates a file
  (mygit "git add *")

  (let ((status (car (mygit "git commit -am \"turning in\""))))
    (unless (or (= 0 status)  ; no problem
		(= 1 status)) ; no change in files
      (switch-to-buffer "*techela log*")
      (error "Problem committing.  Check the logs")))

  (unless (= 0 (car (mygit "git push -u origin master")))
    (switch-to-buffer "*techela log*")
    (error "Problem pushing to server.  Check the logs"))
  
  (message "Woohoo! You turned it in!"))

(defun tq-update-course ()
  "update everything in the current directory."
  (interactive)
  (save-some-buffers t) ;;save all buffers
  (mygit "git add *")
  (mygit "git commit -am \"my changes\"")
  (mygit "git pull origin master")
  (mygit "git commit -am \"accepting merge\""))

;; TODO - this needs better git logic, using functions from techela-git
;; we need to check if the file is tracked, and whether it is dirty
(defun tq-update ()
  "Update current visited file from git.
If local changes have been made, they are commited to the local
repo so a merge can be done."
  (interactive)
  (if (not (string= "" (shell-command-to-string
			  (concat "git status --porcelain "
				  (file-name-nondirectory
				   (buffer-file-name))))))
      ;; the file is dirty. We will commit the results. so we can
      ;; pull. This may result in a conflict later that we have to merge.
      (progn
	(message "It looks like you have made changes to this file. There may be conflicting changes when we merge the update with your changes. These will look like:
<<<<<<<< HEAD
Your changes
========
Changes on the server
>>>>>>>> some-random-git hash characters
These will be committed so that future merges are possible. You should probably keep the server version to avoid future conflicts.")

	;; first, we commit our changes.
	(shell-command (concat "git commit -am \"my changes\""))

	;; now get remotes
	(mygit "git pull origin master")
	;; and now we commit our changes. This may result in
	;; conflicts. We will just accept them and move on.
	(shell-command "git commit -a -m \"accepting merge\""))
  ;; it looks like we were clean
  (mygit "git pull origin master")
  (revert-buffer t t)))


;; This sets up an agenda view of the course assignments
(add-to-list 'org-agenda-custom-commands
      '("c" "Course Agenda"
          (
           ;; deadlines
          (tags-todo "+DEADLINE>=\"<today>\""
                     ((org-agenda-overriding-header "Press q to quit\nUpcoming Deadlines")
		      ))

          ;; now the agenda
	  (agenda ""
		  ((org-agenda-overriding-header "two week agenda")
		   (org-agenda-ndays 14)
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled nil)
		   (org-agenda-todo-ignore-deadlines nil)
		   (org-deadline-warning-days 0)
		   ))

	  ;; and last a global todo list
          (todo "TODO"))) ;; review waiting items ...other commands
			     ;; here
        )


(defun tq-agenda()
  "Show the agenda from the syllabus"
  (interactive)
  (let ((org-agenda-files `(,(expand-file-name "syllabus.org" tq-course-directory))))
    (org-agenda "" "c")))


(defun tq-email-tas ()
    "Construct and send an email to the TAs."
  (interactive)
  ; now create the body of the email
  (let ((email-body
	 (format "Type your note below here, and press C-c C-c when you are done to send it:


======================================================
file: %s
line %s: %s
repo remote origin: %s
======================================================"
		 (buffer-file-name)
		 (what-line)
		 (thing-at-point 'line)
		 (nth 1 (mygit "git config --get remote.origin.url")))))

    (compose-mail-other-frame)
    (message-goto-to)
    (insert "jboes@andrew.cmu.edu,mehakc@andrew.cmu.edu")
    (message-goto-subject)
    (insert (format "[%s] email" tq-current-course))
    (message-goto-body)
    (insert email-body)
    (message-goto-body) ; go back to beginning of email body
    (next-line 2)         ; and down two lines
    (message "Type C-c C-c to send message")))


(defun tq-email ()
  "Construct and send an email to the instructor."
  (interactive)
  ; now create the body of the email
  (let ((email-body
	 (format "Type your note below here, and press C-c C-c when you are done to send it:


======================================================
file: %s
line %s: %s
repo remote origin: %s
======================================================"
		 (buffer-file-name)
		 (what-line)
		 (thing-at-point 'line)
		 (nth 1 (mygit "git config --get remote.origin.url")))))

    (compose-mail-other-frame)
    (message-goto-to)
    (insert "jkitchin@andrew.cmu.edu")
    (message-goto-subject)
    (insert (format "[%s] email" tq-current-course))
    (message-goto-body)
    (insert email-body)
    (message-goto-body) ; go back to beginning of email body
    (next-line 2)         ; and down two lines
    (message "Type C-c C-c to send message")))


(defun tq-send-error-report ()
  "Send an error report to the instructor."
  (interactive)
  (compose-mail-other-frame)
   (message-goto-to)
   (insert "jkitchin@andrew.cmu.edu")
   (message-goto-subject)
   (insert (format "[%s] debug report" tq-current-course))
   (message-goto-body)
   (insert "Tell me what you were doing. Then press C-c C-c to send the message.


Messages\n==========\n")
   (when (get-buffer "*mygit-process*")
     (insert (with-current-buffer "*mygit-process*" (buffer-string)))
     (insert "\n"))
   (when (get-buffer "*techela log*")
     (insert (with-current-buffer "*techela log*" (buffer-string)))
     (insert "\n"))
   (message-goto-body) ; go back to beginning of email body
   
   (next-line 2)         ; and down two lines
   
   (message "Type C-c C-c to send message"))

	   
;;;; links

;; This downloads the assignment repo for the student to work in
(org-add-link-type
 "assignment"
 (lambda (arg)
   (tq-get-assignment arg)))

;; download the solution
(org-add-link-type
 "solution"
 (lambda (label)
   (with-current-directory
    tq-root-directory
    (unless (file-exists-p "solutions")
      (make-directory "solutions"))
    (with-current-directory
     "solutions"
     (if (file-exists-p label)
	 ;; we have the solution
	 (progn
	   (find-file (concat label "/" label ".org"))
	   ;; update just for good measure
	   (tq-update))
       ;; no file
       (mygit (format "git clone %s@%s:solutions/%s"
		      tq-current-course
		      tq-git-server
		      label))
       (find-file (concat label "/" label ".org")))))))
       

;; these will usually be in class or optional exercises. This is a
;; link for clarity of intention for students.
(org-add-link-type
 "exercise"
 (lambda (arg)
   (tq-get-assignment arg)))

;; index link for techela
(org-add-link-type
 "index"
 (lambda (path)
   (tq-index)
   (occur path)))


;; Link to record answers. ans:label::data
;; save in tq-userid-label-data.dat
(org-add-link-type
 "ans"
 (lambda (path)
   (let* ((fields (split-string path "::"))
	  (label (nth 0 fields))
	  (data (nth 1 fields))
	  (data-file (format "%s-%s.dat" tq-userid label)))
     (with-temp-file data-file
       (insert data))
     (mygit (format "git add %s" data-file))
     (mygit (format "git commit -m \"%s\"" data-file))
     (mygit "git push origin master"))))


(defun tq-quit ()
  "Quit techela."
  (interactive)
  (techela-mode -1))


(defun tq-submit-by-email ()
  "Submit contents of current `default-directory' as a zip file attached to an email.
This is normally only done after the deadline, when you cannot push to the git repo, or when there is some issue with the git server. There must be extenuating circumstances for this to be used."
  (interactive)
  (unless (executable-find "zip")
    (error "Could not find a zip executable."))
  
  (let ((zip-name (file-name-sans-extension
		   (file-name-nondirectory
		    (buffer-file-name)))))
    (tq-insert-system-info)
    (shell-command (format "zip -v -r %s *"
			   zip-name))
    (message-mail)
    (mml-attach-file (concat zip-name ".zip"))
    (message-goto-to)
    (insert "jkitchin@andrew.cmu.edu")
    (message-goto-subject)
    (insert (format "[%s email turnin]" tq-current-course))
    (message-send-and-exit)
    ))


(defun tq-get-assigned-assignments ()
  "Return a list of assignments from the syllabus.
Assignments are headings that are tagged with :assignment:.  The assignment is
a link in the heading."
  (interactive)
  (save-current-buffer
    (find-file  (expand-file-name "syllabus.org" tq-course-directory))
    (org-map-entries
     (lambda ()
       (org-entry-get (point) "CUSTOM_ID"))
     "assignment")))


(defun tq-grade-report ()
  "Generate a *grade report* buffer with a summary of the graded assignments."
  (interactive)
  (switch-to-buffer "*grade report*")
  (erase-buffer)
  (org-mode)
  (insert "#+TITLE: Grade report

|label |   score  |             points  |       category|
|--------------------------------------------------------
")
  (dolist (label (tq-get-assigned-assignments))
    (let ((grade)
	  (points)
	  (category)
	  (fname))
      (with-current-buffer (find-file-noselect
			    (expand-file-name "syllabus.org"
					      tq-course-directory))
	(save-restriction
	  (widen)
	  (beginning-of-buffer)
	  ;; This link relies on a CUSTOM_ID
	  (org-open-link-from-string (format "[[#%s]]" label))
	  (setq points (org-entry-get (point) "POINTS"))
	  (setq category (org-entry-get (point) "CATEGORY"))))
      
      ;; check if we need to update
      (if (file-exists-p (expand-file-name label tq-root-directory))
	  (progn
	    (with-current-directory
	     (expand-file-name label tq-root-directory)
	     (when (> (tq-get-num-incoming-changes) 0)
	       (mygit "git pull origin master")))
	  
	    ;; The student assignment will be in root/label/label.org
	    (setq fname (expand-file-name (concat label "/" label ".org") tq-root-directory))
	      
	    (when (file-exists-p fname)
	      (setq grade (gb-get-grade fname)))
	      	    
	    (insert (format "|[[%s][%s]]|  %10s|%20s|%20s|\n" fname label grade points category)))
      ;; no dir found
      (insert (format "|%s|not found|%20s|%20s|\n" label points category)))))
    (previous-line)
    (org-ctrl-c-ctrl-c)
    (goto-char (point-min))
    (switch-to-buffer "*grade report*"))

;;;; menu and minor mode

(require 'easymenu)

(defun tq-open-syllabus ()
  "Open the course syllabus."
  (interactive)
  (find-file (expand-file-name "syllabus.org" tq-course-directory)))

(defvar techela-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") 'tq-email)
    map)
  "Keymap for function `techela-mode'.")

(easy-menu-define techela-menu techela-mode-map "Techela menu"
  '("techela"
    ["Open syllabus" tq-open-syllabus t]
    ["Turn assignment in" tq-turn-it-in t]
    ["Search course files" tq-search t]
    ["Table of contents" tq-toc t]
    ["Course index" tq-index t]
    ["Course agenda" tq-agenda t]
;    ("Assignments")
    ["Get grade report" tq-grade-report t]
    ["Email instructor" tq-email t]
    ["Email TAs" tq-email-tas t]
    ["Update current file" tq-update t]
    ["Send error report" tq-send-error-report t]
    ["Quit techela" tq-quit t]
    ))


(defun tq-get-assignment-menu ()
  "Calculate the list of assignments and their grades for the techela menu."
  ;; add dynamic assignments
  (message "updating techela menu")
  (let ((entries '()))
    (dolist (label (tq-get-assigned-assignments))
      ;; see if we can get the grade
      ;; The student assignment will be in root/label
      (let* ((fname (expand-file-name
		     (concat label "/" label ".org") tq-root-directory))
	     (grade))
	
	(when (file-exists-p fname)
	  (message "getting grade for %s" fname)
	  (setq grade (gb-get-grade fname)))

	(add-to-list 'entries (vector (concat label
					      (when grade (format " (%s)" grade)))
				      `(tq-get-assignment ,label) t))))

    ;; now we update the Assignments menu
    (easy-menu-add-item
     techela-menu
     '()
     (easy-menu-create-menu
      "Assignments"
      entries))
    ))


(define-minor-mode techela-mode
  "Minor mode for techela

\\{techela-mode-map}"
  :lighter " techela"
  :global t
  :keymap techela-mode-map
  
  ;; (if techela-mode
  ;;     (progn
  ;; 	;; this makes it update each time you check the menu
  ;; 	(tq-get-assignment-menu)
  ;; 	(add-hook 'menu-bar-update-hook 'tq-get-assignment-menu))
  ;;   ;;else we are leaving techela mode
  ;;   (remove-hook 'menu-bar-update-hook 'tq-get-assignment-menu))

  )
  


(provide 'techela)

;;; techela.el ends here
