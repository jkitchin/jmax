;;; techela-setup.el --- setup techela courses and admin config file

;; The setup for techela is a ~/.techela file that contains some
;; configuration data for user and admin courses, and a directory
;; ~/techela that is a directory containing the courses. We modify the
;; ssh command in techela_ssh so that it uses a config file
;; ~/.ssh/techela_config which points to a specific id_rsa
;; (~/.ssh/techela_id) key we are going to use.
;;
;; we use the same setup for users and admins (instructors).


;;; Commentary:
;; 

(require 'json)

;;; Code:

(defvar tq-config-file (expand-file-name "~/.techela")
  "Location of the configuration file for techela.")


(defun tq-config-read-data ()
  "Read and return the data in `tq-config-file'.
Data is returned as nested a-lists."
  (if (file-exists-p tq-config-file)
      (let ((json-object-type 'hash-table))
	(json-read-file tq-config-file))
    ;; no file exists, return an empty hash
    (let ((my-hash (make-hash-table :test 'equal)))
      (puthash "user" (make-hash-table :test 'equal) my-hash)
      (puthash "admin" (make-hash-table :test 'equal) my-hash)
      my-hash)))


(defun tq-config-write-data (data)
  "Write DATA to `tq-config-file'.
DATA should be obtained and modified from `tq-config-read-data'."
  (with-temp-file tq-config-file
    (insert (json-encode-hash-table data))))


(defun tq-config-get-user-courses ()
  "Get list of courses the user is registered for."
  (let* ((data (tq-config-read-data))
	 (user-data (gethash "user" data))
	 (results))
    (maphash (lambda (key value) (add-to-list 'results key)) user-data)
    results))


(defun tq-config-get-admin-courses ()
  "Get list of courses the user is registered for."
  (let* ((data (tq-config-read-data))
	 (user-data (gethash "admin" data))
	 (results))
    (maphash (lambda (key value) (add-to-list 'results key)) user-data)
    results))


(defun tq-config-set-user-course (course userid root-dir)
  "Add (COURSE USERID ROOT-DIR) to `tq-config-file'."
  (let* ((data (tq-config-read-data)))
    (if (gethash course (gethash "user" data))
	(progn
	  (puthash "userid" userid (gethash course (gethash "user" data)))
	  (puthash "root-dir" root-dir (gethash course (gethash "user" data))))
      ;; add course.
      (puthash course (make-hash-table :test 'equal) (gethash "user" data))
      (puthash "userid" userid (gethash course (gethash "user" data)))
      (puthash "root-dir" root-dir (gethash course (gethash "user" data))))
    (tq-config-write-data data)))


(defun tq-config-get-user-course (course)
  "Return hash-table for a user COURSE."
  (gethash course (gethash "user" (tq-config-read-data))))


(defun tq-config-get-admin-course (course)
  "Return hash-table for a admin COURSE."
  (gethash course (gethash "admin" (tq-config-read-data))))


(defun tq-config-set-admin-course (course userid root-dir)
  "Add (COURSE USERID ROOT-DIR) to `tq-config-file'."
  (let* ((data (tq-config-read-data)))
    (if (gethash course (gethash "admin" data))
	(progn
	  (puthash "userid" userid (gethash course (gethash "admin" data)))
	  (puthash "root-dir" root-dir (gethash course (gethash "admin" data))))
      ;; add course.
      (puthash course (make-hash-table :test 'equal) (gethash "admin" data))
      (puthash "userid" userid (gethash course (gethash "admin" data)))
      (puthash "root-dir" root-dir (gethash course (gethash "admin" data))))
    (tq-config-write-data data)))


(defun ta-setup-user ()
  "Makes sure these variables are set:  `user-full-name',
`user-mail-address', `send-mail-function',
`message-send-mail-function', `smtpmail-smtp-server',
`smtpmail-starttls-credentials', `smtpmail-smtp-service'."

  ;; Full name
  (unless (and (boundp 'user-full-name) user-full-name (not (string= "" user-full-name)))
    (let ((data (tq-config-read-data)))
      (unless (gethash "user-full-name" data)
	(puthash "user-full-name" (read-from-minibuffer "Enter your full name: ") data)
	(tq-config-write-data data))
      (setq user-full-name (gethash "user-full-name" data))))

  ;; email address
  (unless (and (boundp 'user-mail-address) user-mail-address)
    (let ((data (tq-config-read-data)))
      (unless (gethash "user-mail-address")
	(puthash "user-mail-address" (read-from-minibuffer "Enter your email address: ") data)
	(tq-config-write-data data))
      (setq user-mail-address (gethash "user-mail-address" data))))

  ;; how to send mail
  (unless (and (boundp 'send-mail-function) send-mail-function)
    (setq send-mail-function 'smtpmail-send-it))
  
  ;; the server to send mail from
  (unless (and (boundp 'smtpmail-smtp-server) smtpmail-smtp-server)
    (setq smtpmail-smtp-server "smtp.andrew.cmu.edu"))
  
  ;; credentials we use with authentication.
  (unless (and (boundp 'smtpmail-starttls-credentials) smtpmail-starttls-credentials)
    (setq smtpmail-starttls-credentials '(("smtp.andrew.cmu.edu" 587 nil nil))))

  ;; service port number
  (unless (and (boundp 'smtpmail-smtp-service) smtpmail-smtp-service)
    (setq smtpmail-smtp-service 587))

  (unless (executable-find "python")
    (error "I cannot find python. You cannot use techela."))	  
  
  ;; setup git if it is not. Only set these if they are not already set.
  (unless (executable-find "git")
    (error "I cannot find git.  You cannot use techela"))

  (when (string= "" (shell-command-to-string "git config --global user.name"))
    (shell-command (format "git config --global user.name \"%s\"" user-full-name)))

  (when (string= "" (shell-command-to-string "git config --global user.email"))
    (shell-command (format "git config --global user.email %s" user-mail-address)))

  (when (string= "" (shell-command-to-string "git config --global push.default"))
    (shell-command "git config --global push.default matching")))


(defun ta-setup-ssh ()
  "Setup ssh for use with techela.
Make sure ssh is available. Generate ~/.ssh/techela_id key and
~/.ssh/techela_config. Email key to instructor."
  (interactive)

  (unless (executable-find "ssh")
    (error "I cannot find ssh.  You cannot use techela"))

  ;; check for ~/.ssh
  (unless (file-exists-p
	   (expand-file-name "~/.ssh"))
    (make-directory (expand-file-name "~/.ssh") t))

  ;; now we know the ~/.ssh directory exists, check
  ;; for id_rsa, and make a pair if needed
  (let ((keydir (expand-file-name "~/.ssh")))
    (unless (file-exists-p (expand-file-name "techela_id" keydir))
      ;; we make one with no password
      (shell-command (format "ssh-keygen -t rsa -f %s -N \"\"" (expand-file-name "techela_id" keydir)))

      ;; Now we add this to the config file. first make sure there is a file.
      (shell-command (format "touch %s" (expand-file-name "~/.ssh/techela_config")))

      ;; now append an entry to a config file that techela_ssh uses
      (let ((contents (with-temp-buffer
			(insert-file-contents
			 (expand-file-name "~/.ssh/techela_config"))
			(buffer-string)))
	    (entry (format  "Host %s
  User %s
  IdentityFile ~/.ssh/techela_id
" tq-git-server tq-current-course)))
	(with-temp-file (expand-file-name "~/.ssh/techela_config")
	  (insert contents)
	  (goto-char (point-max))
	  (insert entry)))

      ;; now create an email and send the key to the instructor
      (compose-mail)
      (message-goto-to)
      (insert "jkitchin@andrew.cmu.edu")
      (message-goto-subject)
      (insert (format "[%s] %s pubkey" tq-current-course tq-userid))
      (mml-attach-file (expand-file-name "~/.ssh/techela_id.pub"))
      (message-send-and-exit)
      (message "Your techela key has been sent to the course instructor.  Please wait for a reply with further directions")))
    )

(defun ta-describe ()
  "Open a buffer with information about the setup for techela."
  (interactive)
  (switch-to-buffer (get-buffer-create "*techela describe*"))
  (insert (format "Name: %s\n" user-full-name))
  (insert (format "Userid = %s\n" tq-userid))
  (insert (format "Email: %s\n" user-mail-address))

  (insert (format "System: %s" system-type) "\n")
  (insert (format "Window system: %s" window-system) "\n")
  (insert "~/ located at: " (expand-file-name "~/") "\n")
  (insert "git located at: " (executable-find "git") "\n")
  (insert "ssh located at: " (executable-find "ssh") "\n")
  (insert "python located at: " (executable-find "python") "\n")

  (let ((default-directory starter-kit-dir))
    (insert "\njmax installed at: " starter-kit-dir "\n")
    (insert "jmax current commit: " (shell-command-to-string "git rev-parse HEAD"))
    (insert "jmax tag: " (shell-command-to-string "git tag")))

  
  (insert "\n~/.techela contains:
#+BEGIN====================================================================
")
  (insert-file-contents (expand-file-name "~/.techela"))
  (goto-char (point-max))
  (insert "\n#+END====================================================================

")
  (insert "\n~/.ssh/techela_config contains:
#+BEGIN====================================================================
")
  (insert-file-contents (expand-file-name "~/.ssh/techela_config"))
  (goto-char (point-max))
  (insert "\n#+END====================================================================

")

(insert "\n~/.ssh/techela_id.pub contains:
#+BEGIN====================================================================
")
  (insert-file-contents (expand-file-name "~/.ssh/techela_id.pub"))
  (goto-char (point-max))
  (insert "\n#+END====================================================================

"))


(provide 'techela-setup)

;;; techela-setup.el ends here
