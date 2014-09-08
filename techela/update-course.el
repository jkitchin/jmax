#!/usr/bin/env emacs --script
;; clone or pull all repos 


(dolist (entry (cddr
		(split-string
		 (shell-command-to-string
		  "ssh -x f14-06625@techela.cheme.cmu.edu info")
		 "\n")))
 
  (when (and (not (string-match "\*" entry)) ; wildcard pattern
	     (not (string= "" entry)))
    (let* (;; full-repo is returned from the ssh command
	   (full-repo (substring entry 5 (length entry)))
	   ;; this is the directory the repo is in
	   (repo-base (file-name-directory full-repo))
	   ;; name of the repo
	   (repo (file-name-nondirectory full-repo)))
      ;; now, if the full repo does not exist
      (if (file-exists-p full-repo)
	  (progn
	    (print (format "Updating %s" full-repo))
	    (let ((default-directory (file-name-as-directory (expand-file-name full-repo))))
	      (shell-command "git pull")))
	;; we do not have it, so we get it.
	(print (format "we need to get %s" full-repo))
	  
	(when (and repo-base (not (file-exists-p repo-base)))
	  (make-directory repo-base t))
	
	(let ((default-directory (file-name-as-directory (expand-file-name (or repo-base "")))))
	  (print (format "cloning %s" full-repo))
	  (shell-command
	   (format "git clone f14-06625@techela.cheme.cmu.edu:%s" full-repo)))))))
	      
 
