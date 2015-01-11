;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; remember this directory
(defconst starter-kit-dir (file-name-directory (or load-file-name (buffer-file-name)))
    "directory where the starterkit is installed")

(defvar user-dir (expand-file-name "user" starter-kit-dir)
  "user directory for personal code")

(add-to-list 'load-path starter-kit-dir)
(add-to-list 'load-path user-dir)

;; check status of jmax, and update if needed.  This is not super
;; robust if the user has changed jmax, since we do not check if your
;; repo is clean.
(let ((default-directory starter-kit-dir))
  (shell-command "git fetch")
  (shell-command "git submodule init")
  (shell-command "git submodule update")
  (let ((output (shell-command-to-string
		 "git rev-list --count --left-right HEAD...origin/master"))
	(local-changes) (remote-changes))
    (setq local-changes (string-to-number (nth 0 (split-string output))))
    (setq remote-changes (string-to-number (nth 1 (split-string output))))
    (when (> remote-changes 0)
      (when (let ((last-nonmenu-event nil))
	      (y-or-n-p "jmax is not up to date. Update now?"))
	(message "updating jmax now")
	(shell-command "git pull")
	(shell-command "git submodule init")
	(shell-command "git submodule update")))))

(require 'packages)
(require 'jmax)
;;; end init
