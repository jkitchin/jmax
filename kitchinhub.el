;;; kitchinhub.el --- Interface to KitchinHUB repos

;;; Commentary:
;; One important command: `kitchinhub' which prompts you with a helm buffer of
;; possible repos, and then clones and opens it.

(require 'json)

;;; Code:

(defvar kitchinhub-root "~/Dropbox/KitchinHUB/"
  "Root directory to clone repos in.")

(defvar kitchinhub-userpass nil
  "User:password. Set this in a private place!")

(defun kitchinhub-repos ()
  "Get list of helm candidates of KitchinHUB repos."
  (let ((json (json-read-from-string
	       (shell-command-to-string
		(format
		 "curl -s https://api.github.com/orgs/kitchinhub/repos -u %s"
		 kitchinhub-userpass)))))
    (loop for repo across json
	  collect (cons (format "%30s | %s"
				(cdr (assoc 'name repo))
				(cdr (assoc 'description repo)))
			(cdr (assoc 'name repo))))))


(defun kitchinhub (repo)
  "Select a KitchinHUB REPO and open it.
Selection is done with helm."
  (interactive (list (helm :sources `((name . "KitchinHUB Repos")
				      (candidates . ,(kitchinhub-repos))
				      (action . identity)))))

  ;; make sure we have it
  (unless (file-directory-p (expand-file-name repo kitchinhub-root))
    (let ((default-directory (file-name-as-directory kitchinhub-root)))
      (shell-command (format  "git clone git@github.com:KitchinHUB/%s.git" repo))))

  ;; Now open it
  (find-file (expand-file-name repo kitchinhub-root))
  (magit-status))

(provide 'kitchinhub)

;;; kitchinhub.el ends here
