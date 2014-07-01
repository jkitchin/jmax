;;; techela.el --- functions for students to use in techela courses


;;; Commentary:
;; A techela course has a course-name, and is served on a techela server at
;; git clone course-name@techela.cheme.cmu.edu:course
;;
;; Students must send their id_rsa.pub key to the course admin to
;; access the course.
;; 

;;; Code:

(defvar tq-git-server "techela.cheme.cmu.edu" "The git server")
(defvar tq-course-directory nil "location to clone the course to")
(defvar tq-current-course nil "Store value of current course")

(defun tq-syllabus (tq-course-name)
  "Open the syllabus for TQ-COURSE-NAME.

First we look up the course in `tq-courses'. If it is not there,
we run some setup code to clone the course and save some
variables.


The course should exist at tq-course-name@techela.cheme.cmu.edu:course

The user id_rsa.pub key must be registered in the course.
"
  (interactive
   (list
    (ido-completing-read
     "Course name: "
     (if (boundp 'tq-courses)
	 (mapcar (lambda (x) (car x)) tq-courses)
       '()))))

  ;; create this variable if it does not exist
  ;; it is defined in custom.el
  (unless (boundp 'tq-courses)
    (customize-save-variable 'tq-courses '()))

  ;; Set this for the current session
  (setq tq-current-course tq-course-name)


  ;; define directories to variables if they exist
  (when (and (boundp 'tq-courses) tq-courses)
    (let ((vars (cdr (assoc tq-course-name tq-courses))))
      (when vars
	(setq tq-root-directory (nth 0 vars)
	      tq-course-directory (nth 1 vars)))))

  ;; make directory if needed
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory))
  
  ;; make sure basic variables are defined
  ;; ANDREWID
  (unless (and (boundp '*andrewid*) *andrewid*)
    (customize-save-variable '*andrewid*
			     (downcase (read-from-minibuffer "Enter your andrewid: "))))

  ;; Full name
  (unless (and (boundp 'user-full-name) user-full-name)
    (customize-save-variable 'user-full-name
			     (read-from-minibuffer "Enter your full name: ")))

  ;; email address
  (unless (and (boundp 'user-mail-address) user-mail-address)
    (customize-save-variable
     'user-mail-address
     (read-from-minibuffer "Enter your full name: ")))

  ;; This is where we will clone the course, and later student work
  (unless (and (boundp 'tq-root-directory) tq-root-directory)
    (customize-save-variable
     'tq-root-directory
     (file-name-as-directory
      (ido-read-directory-name "Enter directory to download course: " nil
			  (format "~/Desktop/%s" tq-course-name)))))

  ;; clone course if we need it. This will be in a repo called "course"
  (unless (and tq-course-directory (file-exists-p tq-course-directory))
    (let ((default-directory (file-name-as-directory tq-root-directory)))
      (shell-command (format "git clone %s@%s:course" tq-course-name tq-git-server))))

  ;; we use this later
  (setq tq-course-directory (file-name-as-directory
			     (expand-file-name "course" tq-root-directory)))
     
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

  (if tq-courses
      (add-to-list 'tq-courses `(,tq-course-name . (,tq-root-directory ,tq-course-directory)))
    (setq tq-courses `((,tq-course-name . (,tq-root-directory ,tq-course-directory)))))
  (customize-save-variable 'tq-courses tq-courses)
  
  ;; finally open the syllabus
  (find-file (expand-file-name "syllabus.org" tq-course-directory))
  (techela-mode)

  ;; let user know if an update is needed
  (when (> (tq-get-num-incoming-changes) 0)
    (message "%s is out of date. Please update" tq-course-name)
    (easy-menu-add-item my-menu nil
			["Update Course needed!" tq-update t])))

(defalias 'techela 'tq-syllabus)

(defun tq-in-git-p (&optional debug)
  "Return status for whether `default-directory' is in a git repo.
Optional argument DEBUG switch to output buffer if the command fails."
  (interactive)
  (let ((status (= 0 (shell-command "git rev-parse --is-inside-work-tree"))))
    (when debug
      (unless status
	(switch-to-buffer "*Shell Command Output*")))
    status))


(defun tq-get-num-incoming-changes ()
  "Return number of changes the remote is different than local."
  (unless (tq-in-git-p)
    (error "You are not in a git repo.  We think you are in %s" default-directory))
  (shell-command "git fetch origin")
  (string-to-number (shell-command-to-string "git rev-list HEAD...origin/master --count")))


(defun tq-clone-repo (repo)
  "Clone repo if needed. do nothing if directory exists. clone one directory up from the location of course.el.

REPO should not have the extension .git on it"
  (let ((local-repo (expand-file-name repo tq-root-directory)))
    (if (file-exists-p local-repo)
	local-repo ; the repo exists
      ;; else, we clone it
      (let* ((cmd (format "git clone %s@%s:%s.git %s" tq-current-course tq-git-server repo local-repo))
	     (status (= 0 (shell-command cmd))))
	(message "status = %s" status)
	(if status
	    local-repo	  
	  (switch-to-buffer "*Shell Command Output*")
	  (error "Problem cloning %s" repo))))))


(defun tq-clone-and-open (repo)
  "Clone REPO and open it."
  (find-file (tq-clone-repo repo)))


(defun tq-get-assignment (label)
  "Clone the repo corresponding to LABEL and open the directory."
  (interactive "sLabel: ")
  (let ((repo (format "%s-%s-%s" tq-current-course *andrewid* label)))
    (tq-clone-and-open repo)))


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
  (if (and
       (= 0 (shell-command "git add *"))
       (= 0 (shell-command "git commit -am \"turning in\""))
       (= 0 (shell-command "git push")))
      (message "Woohoo! You turned it in!")
    (switch-to-buffer "*Shell Command Output*")))


(defun tq-update ()
  "Run git pull.  Refresh file currently visited."
  (interactive)
  (shell-command "git pull")
  (revert-buffer t t))

(add-to-list 'org-agenda-custom-commands
      '("c" "Course Agenda"
          (
           ;; deadlines
          (tags-todo "+DEADLINE>=\"<today>\""
                     ((org-agenda-overriding-header "Deadlines")
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
		 (shell-command-to-string "git config --get remote.origin.url"))))

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


;;;; menu and minor mode

(require 'easymenu)

(defvar techela-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") 'tq-email)
    map)
  "Keymap for function `techela-mode'.")

(easy-menu-define my-menu techela-mode-map "My own menu"
  '("techela"
    ["Get assignment " tq-get-assignment t]
    ["Turn assignment in" tq-turn-it-in t]
    ["Course agenda" tq-agenda t]
;    ["Get grade report" tq-grade-report t]
    ["Email" tq-email t]
))

(define-minor-mode techela-mode
  "Minor mode for techela

\\{techela-mode-map}"
  :lighter " techela"
  :global t
  :keymap techela-mode-map)

(provide 'techela)

;;; techela.el ends here
