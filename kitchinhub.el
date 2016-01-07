;;; kitchinhub.el --- Interface to KitchinHUB repos

;;; Commentary:
;; One important command: `kitchinhub' which prompts you with a helm buffer of
;; possible repos, and then clones and opens it.

(require 'json)

;;; Code:

(defvar kitchinhub-userpass nil
  "User:password. Set this in a private place!")

(defvar kitchinhub-root "~/Dropbox/KitchinHUB/"
  "Root directory to clone KitchinHUB repos in.")

(defvar jkitchin-github-root "~/Dropbox/jkitchin-github/"
  "Root directory to clone jkitchin repos in.")

(defvar github-root "~/Dropbox/github/"
  "Root directory to clone other github repos in.")

(loop for dir in (list kitchinhub-root jkitchin-github-root github-root)
      do
      (unless (file-directory-p dir)
	(make-directory dir t)))


(defun kitchinhub-repo-candidates ()
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


(defun jkitchin-repo-candidates ()
  "Get a list of helm candidates of repos from
http://github.com/jkitchin."
  (let ((json (json-read-from-string
	       (shell-command-to-string
		"curl -s https://api.github.com/users/jkitchin/repos"))))
    (loop for repo across json
	  collect (cons (format "%30s | %s"
				(cdr (assoc 'name repo))
				(cdr (assoc 'description repo)))
			(cdr (assoc 'name repo))))))


(defun kitchinhub-open-repo (repo)
  "Open REPO from KitchinHUB."
  (unless (file-directory-p (expand-file-name repo kitchinhub-root))
    (let ((default-directory (file-name-as-directory kitchinhub-root)))
      (shell-command
       (format
	"git clone git@github.com:KitchinHUB/%s.git" repo))))

  ;; Now open it
  (find-file (expand-file-name repo kitchinhub-root))
  (magit-status))


(defun jkitchin-open-repo (repo)
  "Open REPO from jkitchin@github."
  (unless (file-directory-p (expand-file-name repo jkitchin-github-root))
    (let ((default-directory (file-name-as-directory jkitchin-github-root)))
      (shell-command
       (format  "git clone git@github.com:jkitchin/%s.git" repo))))

  ;; Now open it
  (find-file (expand-file-name repo jkitchin-github-root))
  (magit-status))


(defun github-open-repo (repo)
  "Open REPO from github."
  ;; assume repo is in form username/repo-dir
  (let ((repo-dir (file-name-nondirectory repo)))
    (unless (file-directory-p (expand-file-name repo-dir github-root))
      (let ((default-directory (file-name-as-directory github-root)))
	(shell-command (format  "git clone git@github.com:%s.git" repo))))

    ;; Now open it
    (find-file (expand-file-name repo-dir github-root))
    (magit-status)))


(defun kitchinhub (repo)
  "Select a KitchinHUB REPO and open it.
Selection is done with helm."
  (interactive (list (helm :sources `(((name . "KitchinHUB Repos")
				       (candidates . ,(kitchinhub-repo-candidates))
				       (action . kitchinhub-open-repo))
				      ((name . "jkitchin@github")
				       (candidates . ,(jkitchin-repo-candidates))
				       (action . jkitchin-open-repo)))))))

;; * Different approach with asynchronous sources (I think)

(defvar kitchinhub-source (helm-build-async-source
			      "KitchinHUB Repos async"
			    :candidates-process 'kitchinhub-repo-candidates
			    :action '(("Open" . kitchinhub-open-repo))))


(defvar jkitchin-github-source (helm-build-async-source
				   "jkitchin@github async"
			    :candidates-process 'jkitchin-repo-candidates
			    :action '(("Open" . jkitchin-open-repo))))


(defun github-cloned-repos ()
  "List installed repos in `github-root'."
  (f-entries github-root))


(defvar github-source (helm-build-sync-source
			  "GitHUB cloned repos"
			:candidates 'github-cloned-repos
			:action 'find-file))

(defvar github-url-source (helm-build-sync-source
			      "GitHUB urls"
			    :candidates '("https://github.com/"
					  "https://github.com/jkitchin/"
					  "https://github.com/KitchinHUB")
			    :action 'browse-url))


;; I don't know how to use the helm-build-sync-source command to get a fallback.
(defvar github-fallback-source '((name . "Clone github repo")
				(dummy)
				(action . github-open-repo)))


(defun github ()
  "Run Helm to select a git repo from jkitchin or KitchinHUB."
  (interactive)
  (helm :sources '(jkitchin-github-source
		   kitchinhub-source
		   github-source
		   github-url-source
		   github-fallback-source)))


(defun kitchingroup ()
  "Open the Kitchin Group README.org file."
  (interactive)

  ;; clone if we don't have it
  (unless (file-directory-p (expand-file-name "kitchingroup" kitchinhub-root))
    (let ((default-directory kitchinhub-root))
      (shell-command "git clone git@github.com:KitchinHUB/kitchingroup.git")))

  ;; Make sure we are up to date. Stash any changes we made, pull, and reapply them
  (let ((default-directory (file-name-as-directory
			    (expand-file-name "kitchingroup" kitchinhub-root))))
    (shell-command "git stash")
    (shell-command "git pull")
    (shell-command "git stash pop"))

  ;; Now open the README
  (find-file (expand-file-name
	      "README.org"
	      (expand-file-name "kitchingroup" kitchinhub-root))))

;; Some new bindings to add to vc-prefix-map
(define-key 'vc-prefix-map "t" 'magit-status)
(define-key 'vc-prefix-map "p" 'magit-push)
(define-key 'vc-prefix-map "P" 'magit-pull)

(provide 'kitchinhub)

;;; kitchinhub.el ends here
