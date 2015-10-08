;;; init.el --- Where all the magic begins
;;

;;; Commentary:
;;
;; This is a starter kit for jmax. This package provides a customized setup for
;; emacs that we use daily for scientific programming and publication.
;;

;;; Code:

(when (version< emacs-version "24.4")
  (warn "You probably need Emacs 24.4. You should upgrade. You may need to install leuven-theme manually."))

;; remember this directory
(defconst starter-kit-dir (file-name-directory (or load-file-name (buffer-file-name)))
    "Directory where the starterkit is installed.")

(defvar user-dir (expand-file-name "user" starter-kit-dir)
  "User directory for personal code.")

(add-to-list 'load-path starter-kit-dir)
(add-to-list 'load-path user-dir)

(defvar jmax-auto-update t
  "Determines if jmax will automatically update itself.")

;; we load the user/preload.el file if it exists. This lets users define
;; variables that might affect packages when they are loaded, e.g. key-bindings,
;; etc... In particular, this is needed for setting some key-bindings in
;; jmax-bibtex. Also, you can turn off jmax-auto-update here.

(let ((preload (expand-file-name "user/preload.el" starter-kit-dir)))
  (when (file-exists-p preload)
    (load preload)))

;; check status of jmax, and update if needed.  This is not super
;; robust if the user has changed jmax, since we do not check if your
;; repo is clean.
(if jmax-auto-update
    (let ((default-directory starter-kit-dir))
      (shell-command "git fetch origin master")
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
		(shell-command "git pull origin master")
		(shell-command "git submodule init")
		(shell-command "git submodule update"))))))

(require 'packages)
(require 'jmax)


(provide 'init)

;;; init.el ends here
