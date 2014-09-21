;;; git-modeline.el --- Provide git repo summary in modeline



;;; Commentary:
;; 

;;; Code:

(defun in-git-p ()
  "Return if in a git repo or not."
  (not (string-match "^fatal" (shell-command-to-string "git rev-parse --git-dir"))))


(defun git-parse-status ()
  "Get git status and return a propertized string."
  (interactive)
  (let ((U 0)   ; untracked files
	(M 0)   ; modified files
	(O 0)   ; other files
	(U-files "")
	(M-files "")
	(O-files ""))
    (dolist (line (split-string
		   (shell-command-to-string "git status --porcelain")
		   "\n"))
      (cond

       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
	(setq U (+ 1 U))
	(setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
	(setq M (+ 1 M))
	(setq M-files (concat M-files "\n" line))
	)

       (t
        (message "detected other in %s" line)
	(setq O (+ 1 O))
	(setq O-files (concat O-files "\n" line)))))
      
    ;; construct propertized string
    (concat
     "("
     (propertize
      (format "M:%d" M)
      'face (list ':foreground (if (> M 0)
				   "red"
				 "forest green"))
      'help-echo M-files)
     "|"
     (propertize
      (format "U:%d" U)
      'face (list ':foreground (if (> U 0)
				   "red"
				 "forest green"))
      'help-echo U-files)
     "|"
     (propertize
      (format "O:%d" O)
      'face (list ':foreground (if (> O 0)
				   "red"
				 "forest green"))
      'help-echo O-files)
     ") ")))


(defun git-remote-status ()
  (interactive)
  (let* (;; get the branch we are on.
	 (branch (s-trim
		  (shell-command-to-string
		   "git rev-parse --abbrev-ref HEAD")))
	 ;; get the remote the branch points to.
	 (remote (s-trim
		  (shell-command-to-string
		   (format "git config branch.%s.remote" branch))))
         (remote-branch (s-trim
			 (shell-command-to-string
			  "git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD)")))
	 (commits (split-string
		   (s-trim
		    (shell-command-to-string
		     (format
		      "git rev-list --count --left-right HEAD...%s"
		      remote-branch)))))
	 (local (nth 0 commits))
	 (remotes (nth 1 commits)))
    (concat
     "["
     (propertize
      (format "%s" branch)
      'face (list :foreground "magenta"))
     "|"
     (format "↑%s|↓%s" local remotes)
     "]")))


(defvar git-modeline-last-update (float-time) "Last time we updated.")
(defvar git-modeline-update-interval 15 "Minimum time between update in seconds.")
(defvar git-modeline "" "Last value of the modeline.")


(define-minor-mode git-mode
  "minor mode to put git repo status in modeline"
  nil nil nil
  (let ((git-modeline '(:eval (if
				  ;; check if enough time has elapsed for an update!
				  (> (- (float-time) git-modeline-last-update)
				     git-modeline-update-interval)
				  ;; we are updating
				  (setq git-modeline
					(if (not (in-git-p))
					    ""
					  (setq  git-modeline-last-update (float-time))
					  (concat
					   (git-remote-status)
					   (git-parse-status))))
				
			      ;; use last value of the modeline
			      git-modeline))))
    (if git-mode
	;; put in modeline
	(push git-modeline mode-line-format)
	
      ;; remove from modeline
      (setq mode-line-format
	    (-remove (lambda (x)
		       (equal x git-modeline))
		     mode-line-format)))))
     

(provide 'git-modeline)

(provide 'git-modeline)

;;; git-modeline.el ends here
