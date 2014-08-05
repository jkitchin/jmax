;;; techela.el --- functions for students to use in techela courses

;;; Commentary:
;; A techela course has a course-name, and is served on a techela server at
;; git clone course-name@techela.cheme.cmu.edu:course
;;
;; Students must send their id_rsa.pub key to the course admin to
;; access the course.
;;
;; TODO: Dynamic assignment menu showing students assignments and
;; grades/due dates
;;
;; TODO: Student grade report

;;; Code:

(require 'net-utils)
(require 'f)
(require 'techela-utils)
(require 'techela-setup)

(defvar tq-git-server "techela.cheme.cmu.edu"
  "The git server where techela courses are served.")

(defvar tq-root-directory nil
  "Location to clone the course and student work to.  This location is not a git repository.")

(defvar tq-course-directory nil
  "Directory where the course content is.  This will be a git repository.  It is inside `tq-root-directory'.")

(defvar tq-current-course nil "Store value of current course.")

(defvar tq-userid nil "Global variable to store a course userid.")

(defvar tq-course-files nil "list of org-files that constitute the course")

(defun techela (course)
  "Open the syllabus for COURSE.

The course should exist at COURSE@techela.cheme.cmu.edu:course

The user id_rsa.pub key must be registered in the course."
  (interactive
   (list
    (ido-completing-read
     "Course name: "
     (tq-config-get-user-courses))))
  
  ;; Set this for the current session
  (setq tq-current-course course)

  ;; initialize to nil, just in case they were previously set
  (setq tq-root-directory nil
	tq-course-directory nil)
  
  ;; load directories to variables if they exist
  (let ((course-hash (tq-config-get-user-course course)))
    (if course-hash
	(setq tq-root-directory (gethash "root-dir" course-hash)
	      tq-course-directory (expand-file-name "course" tq-root-directory)
	      tq-userid (gethash "userid" course-hash))
    ;; else no entry get info and add one.
      (setq tq-root-directory	    
	    (file-name-as-directory
	     (expand-file-name
	      (format "~/techela/%s" course)))
	  tq-course-directory (file-name-as-directory
			       (expand-file-name "course" tq-root-directory))
	  tq-userid (read-from-minibuffer "Enter userid: "))

    (tq-config-set-user-course course tq-userid tq-root-directory))

    (ta-setup-user)
    (ta-setup-ssh)

    ;; make root directory if needed, including parents
    (unless (file-exists-p tq-root-directory)
      (make-directory tq-root-directory t))

    ;; clone course if we need it. This will be in a repo called "course"
    ;; do not clone if the directory exists.
    (unless (and tq-course-directory (file-exists-p tq-course-directory))
      (let ((default-directory (file-name-as-directory tq-root-directory)))
	(mygit (format "git clone git://%s/course" tq-git-server)))))

  ;; finally open the syllabus
  (find-file (expand-file-name "syllabus.org" tq-course-directory))
  (toggle-read-only)
  (techela-mode)

  (setq org-id-extra-files (files-in-below-directory tq-course-directory))

  ;; let user know if an update is needed
  (when (> (tq-get-num-incoming-changes) 0)
    (message "%s is out of date. Please wait while I update it" course)
    (tq-update)))


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
	    (repo (format "a/%s" label)))
	;; clone and open label.org
	(tq-clone-repo repo)
	(find-file (expand-file-name (concat label ".org") label))
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
		  label))))
      (techela-mode 1))))


(defun  tq-turn-it-in ()
  "Save all buffers, add files in current git directory, create a SYSTEM-INFO file, commit them and push.

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
	(shell-command (concat "git commit -m \"my changes\" " (file-name-nondirectory
								(buffer-file-name))))

	(mygit "git pull origin master")
	;; and now we commit our changes. This will have
	(shell-command "git commit -a -m \"accepting merge\""))
  ;; it looks like we were clean
  (mygit "git pull origin master")
  (revert-buffer t t)
  (techela-mode 1)))


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

;; This is a personal assignment
(org-add-link-type
 "assignment"
 (lambda (arg)
   (tq-get-assignment arg)))


;; these will usually be in class or optional exercises. This is a
;; link for clarity of intention for students.
(org-add-link-type
 "exercise"
 (lambda (arg)
   (tq-get-assignment arg)))


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
  "Quit techela"
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
  (with-temp-buffer
    (insert-file-contents (expand-file-name "syllabus.org" tq-course-directory))
    (org-mode)
    (org-map-entries
     (lambda ()
       (org-entry-get (point) "CUSTOM_ID"))
     "assignment")))



;;;; menu and minor mode

(require 'easymenu)

(defvar techela-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") 'tq-email)
    map)
  "Keymap for function `techela-mode'.")

(easy-menu-define techela-menu techela-mode-map "Techela menu"
  '("techela"
    ["Get assignment " tq-get-assignment t]
    ["Turn assignment in" tq-turn-it-in t]
    ["Search course files" tq-search t]
    ["Course index" tq-index t]
    ["Course agenda" tq-agenda t]
    ("Assignments")
;    ["Get grade report" tq-grade-report t]
    ["Email instructor" tq-email t]
    ["Update current file" tq-update t]
    ["Send error report" tq-send-error-report t]
    ["Quit" tq-quit t]
    ))


(define-minor-mode techela-mode
  "Minor mode for techela

\\{techela-mode-map}"
  :lighter " techela"
  :global t
  :keymap techela-mode-map

  ;; add dynamic assignments
  (dolist (label (tq-get-assigned-assignments))
    ;; see if we can get the grade
    ;; The student assignment will be in root/label
    (let* ((fname (expand-file-name (concat label "/" label ".org") tq-root-directory))
	   (grade (when (and (file-exists-p fname) (file-readable-p fname))
		    (with-temp-buffer
		      (insert-file-contents fname)
		      (org-mode)
		      (gb-get-filetag "GRADE")))))
      
      (easy-menu-add-item
       techela-menu '("Assignments")
       (vector (concat label
		       (when grade (format " (%s)" grade))
		       `(tq-get-assignment ,label) t))))))

(provide 'techela)

;;; techela.el ends here
