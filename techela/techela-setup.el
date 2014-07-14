;;; techela-setup.el --- setup techela courses and admin config file


;; The idea is that we will store the data in $HOME/.techela in json
;; so we can avoid the whole custom.el file. The problem with that is
;; that I am concerned about jmax clobbering other people's custom.el
;; files, and this way we have a dedicated file containing our data in
;; a language agnostic format.
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
  (unless (and (boundp 'user-full-name) user-full-name)
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
    (let ((data (tq-config-read-data)))
      (unless (gethash "send-mail-function" data)
	(puthash "send-mail-function" "smtpmail-send-it" data)
	(tq-config-write-data data))
      ;; this variable takes a symbol, so we make one from the string we read.
      (setq send-mail-function (intern (gethash "send-mail-function" data)))))

  ;; the server to send mail from
  (unless (and (boundp 'smtpmail-smtp-server) smtpmail-smtp-server)
    (let ((data (tq-config-read-data)))
      (unless (gethash "smtpmail-smtp-server" data)
	(puthash "smtpmail-smtp-server" "smtp.andrew.cmu.edu" data)
	(tq-config-write-data data))
      (setq smtpmail-smtp-server (gethash "smtpmail-smtp-server" data))))

  ;; credentials we use with authentication.
  (unless (and (boundp 'smtpmail-starttls-credentials) smtpmail-starttls-credentials)
    (let ((data (tq-config-read-data)))
      (unless (gethash "smtpmail-starttls-credentials" data)
	(puthash "smtpmail-starttls-credentials"
		 "((\"smtp.andrew.cmu.edu\" 587 nil nil))" data)
	(tq-config-write-data data))
      (setq smtpmail-starttls-credentials
	    ;; this should be a lisp list. we read it from the string.
	    (read (gethash "smtpmail-starttls-credentials" data)))))
    
  ;; setup git. This probably clobbers existing setting. It would be
  ;; courteous to only run this if these are empty.
  (shell-command (format "git config --global user.name \"%s\"" user-full-name))
  (shell-command (format "git config --global user.email %s" user-mail-address))

  )


(provide 'techela-setup)

;;; techela-setup.el ends here
