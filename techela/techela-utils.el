;;; techela-utils.el --- utilities
;; options for git

;;; Commentary:
;; 

;;; Code:

(defvar GIT_SSH (format "GIT_SSH=%s" (expand-file-name "techela/techela_ssh" starter-kit-dir)))

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
    (end-of-buffer)
    (insert "\n")
    (insert (apply 'format format-string args))))


(defmacro with-current-directory (directory &rest body)
  "Set the working directory temporarily set to DIRECTORY and run BODY.
DIRECTORY is expanded"
  `(let ((default-directory ,(file-name-as-directory (expand-file-name (eval directory)))))
     ,@body))


(defun mygit (git-command)
  "Run GIT-COMMAND in custom environment.

For example:
 (mygit \"git clone org-course@techela.cheme.cmu.edu:course\")

Sets GIT_SSH to `GIT_SSH', and temporarily modifies the process
environment before running git. `GIT_SSH' points to a shell
script that runs ssh in batch mode.

returns (status output)"
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


(provide 'techela-utils)

;;; techela-utils.el ends here
