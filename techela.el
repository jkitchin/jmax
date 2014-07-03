;;; techela.el --- functions for students to use in techela courses

;;; Commentary:
;; A techela course has a course-name, and is served on a techela server at
;; git clone course-name@techela.cheme.cmu.edu:course
;;
;; Students must send their id_rsa.pub key to the course admin to
;; access the course.
;; 

;;; Code:

(defvar tq-git-server "techela.cheme.cmu.edu"
  "The git server where techela courses are served.")

(defvar tq-root-directory nil
  "Location to clone the course and student work to. This location is not a git repository.")

(defvar tq-course-directory nil
  "Directory where the course content is. This will be a git repository. It is inside `tq-root-directory'")

(defvar tq-current-course nil "Store value of current course")

;; options for git
(defvar GIT_SSH (format "GIT_SSH=%s" (expand-file-name "techela_ssh" starter-kit-dir)))

(defvar tq-debug nil "Whether to debug or not. non-nil triggers some debug action")

(defun tq-log (format-string &rest args)
  "log a message to *techela-log*. Same syntax as `message'.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

Note: Use (tq-log \"%s\" VALUE) to print the value of expressions and
variables to avoid accidentally interpreting `%' as format specifiers."
  (with-current-buffer (get-buffer-create "*techela-log*")
    (end-of-buffer)
    (insert "\n")
    (insert (apply 'format format-string args))))


(defmacro with-current-directory (directory &rest body)
  "Runs BODY with the working directory temporarily set to
DIRECTORY. DIRECTORY is expanded"
  `(let ((default-directory ,(file-name-as-directory (expand-file-name (eval directory)))))
     ,@body))


(defun mygit (git-command)
  "Run GIT-COMMAND in custom environment.

For example:
 (mygit \"git clone org-course@techela.cheme.cmu.edu:course\")

You should make
Sets GIT_SSH to `GIT_SSH', and temporarily modifies the process environment before running git. `GIT_SSH' points to a shell script that runs ssh in batch mode.

returns (status output)
"
  (interactive "sgit command: ")
  (let ((process-environment (cons GIT_SSH process-environment))
        (status) (output))
    (when (get-buffer "*mygit-process*") (kill-buffer "*mygit-process*"))
    (tq-log "\nRunning \"%s\"\n  CWD = %s" git-command default-directory)
    (setq status (call-process-shell-command git-command nil "*mygit-process*"))
    (setq output (with-current-buffer "*mygit-process*" (buffer-string)))
    (tq-log "  status = %s" status)
    (tq-log "  output = %s" output)
    (list status output)))

(defun ta-setup-user ()
  "Makes sure these variables are set:

`*andrewid*', `user-full-name'"
  ;; make sure basic variables are defined
  ;; ANDREWID
  (unless (and (boundp '*andrewid*) *andrewid*)
    (customize-save-variable '*andrewid*
			     (downcase (read-from-minibuffer "Enter your andrewid: "))))

  ;; Full name
  (unless (and (boundp 'user-full-name) user-full-name)
    (customize-save-variable 'user-full-name
			     (read-from-minibuffer "Enter your full name: "))))


(defun ta-setup-email ()
  "Saves email setup if needed.

Makes sure these variables are set.
`user-mail-address', `send-mail-function',
`message-send-mail-function', `smtpmail-smtp-server',
`smtpmail-starttls-credentials', `smtpmail-smtp-service'"

  ;; email address
  (unless (and (boundp 'user-mail-address) user-mail-address)
    (customize-save-variable
     'user-mail-address
     (read-from-minibuffer "Enter your email address: ")))
  
  (unless (and (boundp 'send-mail-function) send-mail-function)
    (customize-save-variable 'send-mail-function 'smtpmail-send-it))

  (unless (and (boundp 'message-send-mail-function) message-send-mail-function)
    (customize-save-variable 'message-send-mail-function 'smtpmail-send-it))

  (unless (and (boundp 'smtpmail-smtp-server) smtpmail-smtp-server)
    (customize-save-variable 'smtpmail-smtp-server "smtp.andrew.cmu.edu"))

  (unless (and (boundp 'smtpmail-starttls-credentials) smtpmail-starttls-credentials)
    (customize-save-variable
     'smtpmail-starttls-credentials
     `((,smtpmail-smtp-server 587 nil nil))))

  (unless (and (boundp 'smtpmail-smtp-service) smtpmail-smtp-service)
    (customize-save-variable 'smtpmail-smtp-service 587))

  ;; setup git
  (shell-command (format "git config --global user.name \"%s\"" user-full-name))
  (shell-command (format "git config --global user.email %s" user-mail-address))

  )


(defun techela (tq-course-name)
  "Open the syllabus for TQ-COURSE-NAME.

First we look up the course in `techela-courses'. If it is not there,
we run some setup code to clone the course and save some
variables.

The course should exist at tq-course-name@techela.cheme.cmu.edu:course

The user id_rsa.pub key must be registered in the course.
"
  (interactive
   (list
    (ido-completing-read
     "Course name: "
     (if (boundp 'techela-courses)
	 (mapcar (lambda (x) (car x)) techela-courses)
       '()))))

  (ta-setup-user)
  (ta-setup-email)

  ;; Set this for the current session
  (setq tq-current-course tq-course-name)

  ;; initialize to nil
  (setq tq-root-directory nil
	tq-course-directory nil)

  ;; load directories to variables if they exist
  (when (and (boundp 'techela-courses) techela-courses)
    (let ((vars (cdr (assoc tq-course-name techela-courses))))
      (if vars 
	  (progn  ;; we have some data
	    (setq tq-root-directory (nth 0 vars)
		  tq-course-directory (nth 1 vars)))
	;; no course found, set these to nil so we have to start over
	(setq tq-root-directory nil
	      tq-course-directory nil))))
  
  ;; This is where we will clone the course, and later student work If
  ;; this variable does not exist, it was not saved in custom.el we
  ;; prompt for where to put the course.  if tq-root-directory is
  ;; defined here, it means it was read above, and we skip this step.
  (unless (and (boundp 'tq-root-directory) tq-root-directory)
    (setq tq-root-directory
	  (file-name-as-directory
	   (ido-read-directory-name "Enter directory to download course: " nil
				    (format "~/Desktop/%s" tq-course-name)))
	  tq-course-directory (file-name-as-directory
			       (expand-file-name "course" tq-root-directory))))
  
  ;; make root directory if needed, including parents
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory t))

  
  ;; clone course if we need it. This will be in a repo called "course"
  ;; do not clone if the directory exists.
  (unless (and tq-course-directory (file-exists-p tq-course-directory))
    (let ((default-directory (file-name-as-directory tq-root-directory)))
      (mygit (format "git clone %s@%s:course" tq-course-name tq-git-server))))

  ;; Now, we add the defined course to
  (if (and (boundp 'techela-courses) techela-courses)
      (add-to-list 'techela-courses (cons tq-course-name (list tq-root-directory tq-course-directory)))
    (setq techela-courses (list (cons tq-course-name (list tq-root-directory tq-course-directory)))))
  (customize-save-variable 'techela-courses techela-courses)
  
  ;; finally open the syllabus
  (find-file (expand-file-name "syllabus.org" tq-course-directory))
  (toggle-read-only)
  (techela-mode)

  ;; let user know if an update is needed
  (when (> (tq-get-num-incoming-changes) 0)
    (message "%s is out of date. Please update" tq-course-name)
    (easy-menu-add-item techela-menu nil
			["Update Course needed!" tq-update t])))


(defun tq-in-git-p (&optional debug)
  "Return status for whether `default-directory' is in a git repo.
Optional argument DEBUG switch to output buffer if the command fails."
  (interactive)
  (mygit "git rev-parse --is-inside-work-tree"))


(defun tq-get-num-incoming-changes ()
  "Return number of changes the remote is different than local."
  (interactive)
  (unless (tq-in-git-p)
    (error "You are not in a git repo.  We think you are in %s." default-directory))
  (mygit "git fetch origin")
  (string-to-number (nth 1 (mygit "git rev-list HEAD...origin/master --count"))))


(defun tq-clone-repo (repo)
  "Clone REPO into current directory if needed. If REPO exists,
do not do anything. REPO should not have the extension .git on
it. If you want to clone it somewhere else, temporarily define
default-directory.
"
  (if (file-exists-p repo)
      repo
    (mygit (format "git clone %s@%s:%s.git" tq-current-course tq-git-server repo))
    repo))


(defun tq-clone-and-open (repo)
  "Clone REPO and open it."
  (let ((default-directory tq-root-directory))    
    (tq-clone-repo repo)
    (find-file (expand-file-name (concat repo ".org") repo))))


(defun tq-get-assignment (label)
  "Clone the repo corresponding to LABEL and open the directory."
  (interactive "sLabel: ")
  (let ((default-directory tq-root-directory))
    (let ((repo (format "%s-%s-%s" tq-current-course *andrewid* label)))
      ;; clone and open label.org
      (tq-clone-repo repo)
      (find-file (expand-file-name (concat label ".org") repo)))))


(require 'net-utils)
(defun insert-system-info ()
  "Create a SYSTEM-INFO file containing system info."
  (interactive)
  (with-temp-file "SYSTEM-INFO"
    (insert (format "Name: %s\n" user-full-name))
    (insert (format "Andrewid = %s\n" *andrewid*))
    (insert (format "Email: %s\n" user-mail-address))
    (insert "System name: " (system-name))
    (insert (format "\n%s" system-type))
    (insert (shell-command-to-string ifconfig-program))))


(defun  tq-turn-it-in ()
  "Save all buffers.  add files in current git directory, commit them and push."
  (interactive)
  (save-some-buffers t t)
  (insert-system-info)
  (mygit "git add *")

  (let ((status (car (mygit "git commit -am \"turning in\""))))
    (unless (or (= 0 status)  ; no problem
		(= 1 status)) ; no change in files
      (switch-to-buffer "*techela-log*")
      (error "Problem committing. Check the logs")))

  (unless (= 0 (car (mygit "git push")))
    (switch-to-buffer "*techela-log*")
    (error "Problem pushing to server. Check the logs"))
  
  (message "Woohoo! You turned it in!"))

(defun tq-update ()
  "Run git pull.  Refresh file currently visited."
  (interactive)
  (mygit "git pull")
  (revert-buffer t t)
  (easy-menu-remove-item techela-menu nil "Update Course needed!")
  (techela-mode 1))


(add-to-list 'org-agenda-custom-commands
      '("c" "Course Agenda"
          (
           ;; deadlines
          (tags-todo "+DEADLINE>=\"<today>\""
                     ((org-agenda-overriding-header "Upcoming Deadlines")
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
repo: %s
======================================================"
		 (buffer-file-name)
		 (what-line)
		 (thing-at-point 'line)
		 (mygit "git config --get remote.origin.url" t))))

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
  (interactive)
  (compose-mail-other-frame)
   (message-goto-to)
   (insert "jkitchin@andrew.cmu.edu")
   (message-goto-subject)
   (insert (format "[%s] debug report" tq-current-course))
   (message-goto-body)
   (insert "Tell match-end what you were doing. Then press C-c C-c to send the message.


Messages\n==========\n")
   (when (get-buffer "*mygit-process*")
     (insert (with-current-buffer "*mygit-process*" (buffer-string)))
     (insert "\n"))
   (when (get-buffer "*techela-log*")
     (insert (with-current-buffer "*techela-log*" (buffer-string)))
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


;; these will usually be class directories
(org-add-link-type
 "exercise"
 (lambda (arg)
   (tq-clone-and-open arg)))

;; Link to record answers. ans:label-data
;; parse the path, split by -
;; save in *andrewid*-label.dat
(org-add-link-type
 "ans"
 (lambda (path)
   (let* ((fields (split-string path "-"))
	  (label (nth 0 fields))
	  (data (nth 1 fields))
	  (data-file (format "%s-%s.dat" *andrewid* label)))
     (with-temp-file data-file
       (insert data))
     (mygit (format "git add %s" data-file))
     (mygit (format "git commit -m \"%s\"" data-file))
     (mygit "git push"))))




;;;; menu and minor mode

(require 'easymenu)

(defvar techela-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") 'tq-email)
    map)
  "Keymap for function `techela-mode'.")

(easy-menu-define techela-menu techela-mode-map "My own menu"
  '("techela"
    ["Get assignment " tq-get-assignment t]
    ["Turn assignment in" tq-turn-it-in t]
    ["Course agenda" tq-agenda t]
;    ["Get grade report" tq-grade-report t]
    ["Email" tq-email t]
    ["Send error report" tq-send-error-report t]
))

(define-minor-mode techela-mode
  "Minor mode for techela

\\{techela-mode-map}"
  :lighter " techela"
  :keymap techela-mode-map)

(provide 'techela)

;;; techela.el ends here
