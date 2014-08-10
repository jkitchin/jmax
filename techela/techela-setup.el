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

(defvar tq-config-file nil
  "Location of the configuration file for techela.")


(defun tq-config-read-data ()
  "Read and return the data in `tq-config-file'."
  (if (file-exists-p tq-config-file)
      (let ((json-object-type 'hash-table))
	(json-read-file tq-config-file))
    ;; no file exists, return an empty hash
    (make-hash-table :test 'equal)))


(defun tq-config-write-data (data)
  "Write DATA to `tq-config-file'.
DATA should be obtained and modified from `tq-config-read-data'."
  (with-temp-file tq-config-file
    (insert (json-encode-hash-table data))))


(defun tq-config-get-user-courses ()
  "Get a list of available courses."
  '("org-course" "f14-06625"))

;; (defun tq-config-get-user-courses ()
;;   "Get list of courses the user is registered for."
;;   (let* ((data (tq-config-read-data))
;; 	 (user-data (gethash "user" data))
;; 	 (results))
;;     (maphash (lambda (key value) (add-to-list 'results key)) user-data)
;;     results))

(defun tq-config-get-admin-courses ()
  '("org-course" "f14-06625"))

;; (defun tq-config-get-admin-courses ()
;;   "Get list of courses the user is registered for."
;;   (let* ((data (tq-config-read-data))
;; 	 (user-data (gethash "admin" data))
;; 	 (results))
;;     (maphash (lambda (key value) (add-to-list 'results key)) user-data)
;;     results))


;; (defun tq-config-set-user-course (course userid root-dir)
;;   "Add (COURSE USERID ROOT-DIR) to `tq-config-file'."
;;   (let* ((data (tq-config-read-data)))
;;     (if (gethash course (gethash "user" data))
;; 	(progn
;; 	  (puthash "userid" userid (gethash course (gethash "user" data)))
;; 	  (puthash "root-dir" root-dir (gethash course (gethash "user" data))))
;;       ;; add course.
;;       (puthash course (make-hash-table :test 'equal) (gethash "user" data))
;;       (puthash "userid" userid (gethash course (gethash "user" data)))
;;       (puthash "root-dir" root-dir (gethash course (gethash "user" data))))
;;     (tq-config-write-data data)))


;; (defun tq-config-get-user-course (course)
;;   "Return hash-table for a user COURSE."
;;   (gethash course (gethash "user" (tq-config-read-data))))


;; (defun tq-config-get-admin-course (course)
;;   "Return hash-table for a admin COURSE."
;;   (gethash course (gethash "admin" (tq-config-read-data))))


;; (defun tq-config-set-admin-course (course userid root-dir)
;;   "Add (COURSE USERID ROOT-DIR) to `tq-config-file'."
;;   (let* ((data (tq-config-read-data)))
;;     (if (gethash course (gethash "admin" data))
;; 	(progn
;; 	  (puthash "userid" userid (gethash course (gethash "admin" data)))
;; 	  (puthash "root-dir" root-dir (gethash course (gethash "admin" data))))
;;       ;; add course.
;;       (puthash course (make-hash-table :test 'equal) (gethash "admin" data))
;;       (puthash "userid" userid (gethash course (gethash "admin" data)))
;;       (puthash "root-dir" root-dir (gethash course (gethash "admin" data))))
;;     (tq-config-write-data data)))


(defun ta-setup-user ()
  "Makes sure these variables are set:  `user-full-name',
`user-mail-address', `send-mail-function',
`message-send-mail-function', `smtpmail-smtp-server'. Configure
git with this information where needed."

  ;; Full name
  (let ((data (tq-config-read-data)))
    (unless (gethash "user-full-name" data)
      (puthash "user-full-name" (read-from-minibuffer "Enter your full name: ") data)
      (tq-config-write-data data))
    (setq user-full-name (gethash "user-full-name" data)))

  ;; email address
  (let ((data (tq-config-read-data)))
    (unless (gethash "user-mail-address" data)
      (puthash "user-mail-address" (read-from-minibuffer "Enter your email address: ") data)
      (tq-config-write-data data))
    (setq user-mail-address (gethash "user-mail-address" data)))

  ;; how to send mail
  (when (equal send-mail-function 'sendmail-query-once)
    (setq send-mail-function 'smtpmail-send-it))

  ;; this will clobber any user settings. For new students these are
  ;; not likely to be set. I am not sure how to handle this generally.
  (setq smtpmail-smtp-server "smtp.andrew.cmu.edu"
	smtpmail-smtp-service 587
	smtpmail-starttls-credentials '(("smtp.andrew.cmu.edu" 587 nil nil))
	smtpmail-stream-type nil
	starttls-use-gnutls t
	starttls-gnutls-program "gnutls-cli")

  (unless (and (boundp 'mail-host-address) mail-host-address)
    (setq mail-host-address "andrew.cmu.edu"))

  ;; Append a line to the ~/.authinfo for authentication with mail
  ;; Users will be prompted for their andrew password
  (unless (file-exists-p (expand-file-name "~/.authinfo"))
    (with-temp-file (expand-file-name "~/.authinfo")))
  
  (let ((contents (with-temp-buffer
		    (insert-file-contents
		     (expand-file-name "~/.authinfo"))
		    (buffer-string))))
    (unless (string-match "smtp\.andrew\.cmu\.edu" contents)
      (with-temp-file (expand-file-name "~/.authinfo")
	(when contents (insert contents))
	(goto-char (point-max))
	(insert
	 (format
	  "\nmachine smtp.andrew.cmu.edu port 587 login %s" tq-userid)))))

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
Make sure ssh is available. Generate ssh key, config and wrapper script. Email key to instructor."
  (interactive)

  (unless (executable-find "ssh")
    (error "I cannot find ssh.  You cannot use techela"))

  ;; check for tq-root-directory
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory t))

  ;; now we know the tq-root-directory directory exists, check
  ;; for userid and userid.pub, and make a pair if needed
  
  ;; here we look for the existence of tq-root-directory/userid.pub which is a private ssh key
  (unless (file-exists-p (expand-file-name
			  (concat tq-userid ".pub")
			  tq-root-directory))
    ;; we make one with no password
    (shell-command (format "ssh-keygen -t rsa -f %s -N \"\""
			   (expand-file-name
			    tq-userid
			    tq-root-directory)))

    ;; Now we add this to the config file. first make sure there is a file.
    (let ((ssh-config (expand-file-name
		       "techela-config"
		       tq-root-directory)))
      
      (shell-command (format "touch %s" ssh-config))

      ;; now append an entry to a config file that techela_ssh uses
      (let ((contents (with-temp-buffer
			(insert-file-contents
			 ssh-config)
			(buffer-string)))
	    (entry (format  "Host %s
  User %s
  IdentityFile %s
" tq-git-server tq-current-course 
(expand-file-name tq-userid tq-root-directory))))
	(with-temp-file ssh-config
	  (insert contents)
	  (goto-char (point-max))
	  (insert entry)))

      ;; create the techela_ssh script we need to use
      (with-temp-file (expand-file-name
		       "techela_ssh"
		       tq-root-directory)
	(insert
	 (format "#!/bin/bash

# custom ssh for running git in batch mode for techela with the user-key
exec ssh -F %s -o \"BatchMode yes\" \"$@\"
# end"
		 (expand-file-name "techela-config" tq-root-directory))))

      ;; make the script executable
      (set-file-modes (expand-file-name
		       "techela_ssh"
		       tq-root-directory) #o755)
      

      ;; now create an email and send the key to the instructor
      (compose-mail)
      (message-goto-to)
      (insert "jkitchin@andrew.cmu.edu")
      (message-goto-subject)
      (insert (format "[%s] %s pubkey" tq-current-course tq-userid))
      (mml-attach-file (expand-file-name
			  (concat tq-userid ".pub")
			  tq-root-directory))
      (message-goto-body)
      ;; let us get some user/computer information
      (tq-insert-system-info)
      (insert (with-temp-buffer
		(insert-file-contents "SYSTEM-INFO")
		(buffer-string)))
      (delete-file "SYSTEM-INFO")
      (message-send-and-exit)
      (message "Your techela key has been sent to the course instructor. It is saved in ~/techela/%s/%s and ~/techela/%s/%s.pub. Do not delete these, as they give you access to the class. Please wait for a reply with further directions." tq-current-course tq-userid tq-current-course tq-userid)))
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
  (insert "\n~/.ssh/techela-config contains:
#+BEGIN====================================================================
")
  (insert-file-contents (expand-file-name "~/.ssh/techela-config"))
  (goto-char (point-max))
  (insert "\n#+END====================================================================

")

(insert (format "\n~/.ssh/%s.pub contains:
#+BEGIN====================================================================
" tq-userid))
  (insert-file-contents (expand-file-name (format "~/.ssh/%s.pub" tq-userid)))
  (goto-char (point-max))
  (insert "\n#+END====================================================================

"))

(defun ta-start-clean ()
  (interactive)
  (shell-command "rm ~/.techela")
  (shell-command "rm -fr ~/techela")
  (shell-command "rm ~/.ssh/techela*"))

(provide 'techela-setup)

;;; techela-setup.el ends here
