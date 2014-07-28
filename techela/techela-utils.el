;;; techela-utils.el --- utilities
;; options for git

;;; Commentary:
;; 

;;; Code:

(defvar *GIT_SSH* (format "GIT_SSH=%s" (expand-file-name "techela/techela_ssh" starter-kit-dir)))

(defvar tq-debug nil "Whether to debug or not.  non-nil triggers some debug action.")


(defun tq-log (format-string &rest args)
  "Log a message to *techela log*.  Same syntax as `message'.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

Note: Use (tq-log \"%s\" VALUE) to print the value of expressions and
variables to avoid accidentally interpreting `%' as format specifiers.
Argument FORMAT-STRING format string.
Optional argument ARGS extra arguments."
  (with-current-buffer (get-buffer-create "*techela log*")
    (goto-char (point-max))
    (insert "\n")
    (insert (apply 'format format-string args))))


;(defmacro with-current-directory (directory &rest body)
;  "Set the working directory temporarily set to DIRECTORY and run BODY.
;DIRECTORY is expanded"
;  `(let ((default-directory ,(file-name-as-directory (expand-file-name (eval directory)))))
;     ,@body))


(defun with-current-directory (directory &rest body)
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    body))

(defun mygit (git-command)
  "Run GIT-COMMAND in custom environment.

For example:
 (mygit \"git clone org-course@techela.cheme.cmu.edu:course\")

Sets GIT_SSH to `*GIT_SSH*', and temporarily modifies the process
environment before running git. `*GIT_SSH*' points to a shell
script that runs ssh in batch mode.

returns (status output)"
  (interactive "sgit command: ")
  (let ((process-environment (cons *GIT_SSH* process-environment))
        (status) (output))
    (when (get-buffer "*mygit-process*") (kill-buffer "*mygit-process*"))
    (tq-log "\nRunning \"%s\"\n  CWD = %s" git-command default-directory)
    (setq status (call-process-shell-command git-command nil "*mygit-process*"))
    (setq output (with-current-buffer "*mygit-process*" (buffer-string)))
    (tq-log "  status = %s" status)
    (tq-log "  output = %s" output)
    (list status output)))


(defun tq-in-git-p (&optional debug)
  "Return status for whether `default-directory' is in a git repo.
Optional argument DEBUG switch to output buffer if the command fails."
  (interactive)
  (mygit "git rev-parse --is-inside-work-tree"))


(defun tq-get-num-incoming-changes ()
  "Return number of commits the remote is different than local."
  (interactive)
  (unless (tq-in-git-p)
    (error "You are not in a git repo.  We think you are in %s" default-directory))
  (mygit "git fetch origin")
  (string-to-number (nth 1 (mygit "git rev-list HEAD...origin/master --count"))))


(defun tq-clone-repo (repo)
  "Clone REPO into current directory if needed.
If REPO exists, do not do anything.  REPO should not have the extension .git on
it.  If you want to clone it somewhere else, temporarily define
`default-directory'."
  (if (file-exists-p (f-filename repo))
      repo
    (when (not (= 0 (car (mygit (format "git clone %s@%s:%s.git" tq-current-course tq-git-server repo)))))
      (switch-to-buffer "*techela log*")
      (error "Problem cloning %s" repo))
    repo))


(defun tq-clone-and-open (repo)
  "Clone REPO and open it."
  (let ((default-directory tq-root-directory))
    (tq-clone-repo repo)
    (find-file (expand-file-name (concat repo ".org") repo))))

(defun tq-insert-system-info ()
  "Create a SYSTEM-INFO file containing system info."
  (interactive)
  (with-temp-file "SYSTEM-INFO"
    (insert (format "Name: %s\n" user-full-name))
    (insert (format "Userid = %s\n" tq-userid))
    (insert (format "Email: %s\n" user-mail-address))
    (insert "System name: " (system-name))
    (insert (format "\n%s" system-type))
    (insert (shell-command-to-string ifconfig-program))))

;; http://www.gnu.org/software/emacs/manual/html_node/eintr/Files-List.html
(defun files-in-below-directory (directory)
  "List the .org files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "DDirectory name: ")
  (let (org-files-list
	(current-directory-list
	 (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".org" (substring (car (car current-directory-list)) -4))
	(setq org-files-list
	      (cons (car (car current-directory-list)) org-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
	;; decide whether to skip or recurse
	(if
	    (equal "."
		   (substring (car (car current-directory-list)) -1))
	    ;; then do nothing since filename is that of
	    ;;   current directory or parent, "." or ".."
	    ()
	  ;; else descend into the directory and repeat the process
	  (setq org-files-list
		(append
		 (files-in-below-directory
		  (car (car current-directory-list)))
		 org-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    org-files-list))'


(defun tq-search (regexp)
  "Search all of the course files using `multi-occur-in-matching-buffers' for REGEXP.

Opens all course files, then does the search."
  (interactive (list (read-regexp "Regexp: ")))

  (let ((org-files (files-in-below-directory tq-course-directory)))
    (message "%s" org-files)
  
    (dolist (f org-files)
      (message "opening %s" f)
      (find-file-noselect f))

    (multi-occur-in-matching-buffers ".*.org" regexp)))
  

(defun tq-index ()
  "Generate a temporary index buffer from the course files."
  (interactive)
  (let ((*index-links*)
	(*initial-letters*)
	(org-files (files-in-below-directory tq-course-directory)))
    ;; get links
    (dolist (f org-files)
      (find-file f)

      (org-element-map (org-element-parse-buffer) 'link
	(lambda (link)       
	  (let ((type (nth 0 link))
		(plist (nth 1 link)))
	    
	    (when (equal (plist-get plist ':type) "index")
	      (add-to-list '*index-links* 
			   (cons (plist-get plist :path)
				 (format "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s))][%s]] (%s)"
								 
					 (current-buffer)
					 (plist-get plist :begin)
					 (save-excursion
					   (goto-char (plist-get plist :begin))
					   (replace-regexp-in-string "\n" "" (thing-at-point 'sentence)))
					 (file-name-nondirectory (buffer-file-name))))))))))				
    (setq *index-links*  (cl-sort *index-links* 'string-lessp :key 'car))

    ;; now first letters
    (dolist (link *index-links*)
      (add-to-list '*initial-letters* (substring (car link) 0 1) t))


    (switch-to-buffer (get-buffer-create "*index*"))
    (org-mode)
    (erase-buffer)
    (insert "#+TITLE: Index\n\n")
    (dolist (letter *initial-letters*)
      (insert (format "* %s\n" (upcase letter)))
      ;; now process the links
      (while (and *index-links* (string= letter (substring (car (car *index-links*)) 0 1)))
	(let ((link (pop *index-links*)))
	  (insert (format "%s %s\n\n" (car link) (cdr link)))))))) 
    
    

(provide 'techela-utils)

;;; techela-utils.el ends here
