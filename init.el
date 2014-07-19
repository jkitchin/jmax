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

;; check status of jmax, and update if needed. We use a timeout of 10
;; seconds which defaults to no update. We could use a variable
;; setting here but where would it get set?
(let ((default-directory starter-kit-dir))
  (shell-command "git fetch")
  (unless (= 0 (string-to-number
		(shell-command-to-string
		 "git rev-list HEAD...origin/master --count")))
    (when (let ((last-nonmenu-event nil))
	    (y-or-n-p "jmax is not up to date. Update now?"))
      (message "updating jmax now")
      (shell-command "git pull"))))

(require 'packages)
(require 'jmax)
;;; end init

