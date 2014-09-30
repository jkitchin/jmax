;;; techela-git.el --- Scripting git in emacs for techela


;;; Commentary:
;; 

(require 'git-modeline)



;;; Code:

(defun ta-git-n-commits ()
  "Return how many commits differ locally and remotely.

\(0 0) means you are up to date as far as commits go
\(n 0) means you are ahead of the remote and need to push
\(0 n) means you are behind the remote and need to pull
\(m n) means there is a divergence, and a pull and push will be required."
  (interactive)
  ;; see what is on remote
  (mygit "git fetch")
  (let ((result
	 (mygit "git rev-list --count --left-right HEAD...origin/master"))
	(status) (output)
	(local-changes) (remote-changes))
    (setq status (nth 0 result))
    (setq output (nth 1 result))
    (setq local-changes (string-to-number (nth 0 (split-string output))))
    (setq remote-changes (string-to-number (nth 1 (split-string output))))

    (list local-changes remote-changes)))


(defun ta-git-n-untracked-files ()
  "Return number of untracked files.
These are files with ?? in them from git status --porcelain"
  (interactive)
  (let ((n 0))
    (dolist (line (split-string
		   (shell-command-to-string "git status --porcelain")
		   "\n"))
      (when (string-match "^\\?\\?" line)
	(setq n (+ 1 n))))
    n))


(defun ta-git-n-modified-files ()
  "Return number of modified, but uncommitted files.
These are files with M in them from git status --porcelain"
    (let ((n 0))
      (dolist (line (split-string
		     (shell-command-to-string "git status --porcelain")
		     "\n"))
	(when (string-match "^ M" line)
	  (setq n (+ 1 n))))
      n))


(defun ta-git-buffer-tracked-p ()
  "Return if the file the buffer is visiting is tracked by git.

git ls-files filename returns an empty string if filename is not under git control"
  (interactive)
  (not (string=
	""
	(shell-command-to-string
	 (format "git ls-files %s"
		 (file-name-nondirectory
		  (buffer-file-name)))))))


(defun ta-git-buffer-modified-p ()
  "Return if the file the buffer is visiting has been modified.

Save the buffer first.
git status --porcelain filename
returns \" M filename
\" when the file is modified."
  (interactive)
  (when (ta-git-buffer-tracked-p)
    (save-buffer)
    (string-match
     "^ M"
     (shell-command-to-string
      (format "git status --porcelain %s"
	      (file-name-nondirectory
	       (buffer-file-name)))))))


(defvar ta-git-unmerged-states
  '(("D" "D")
    ("A" "U")
    ("U" "D")
    ("U" "A")
    ("D" "U")
    ("A" "A")
    ("U" "U"))
  "List of XY states a file can be in that indicate an unmerged state.")

(defun ta-git-make-repo-clean ()
  "Handle every line in git status --porcelain and leave repo in clean state.

This may happen before or after a merge.  Before a merge, we handle each line.  After a merge, we add everything at once, and then commit the merge."
  (interactive)

  (let* ((merge-p nil)
	 (lines (loop for line in
		      (split-string
		       (shell-command-to-string "git status --porcelain") "\n")
		      when (not (string= "" line))
		      collect
		      (let* ((X (substring line 0 1))
			     (Y (substring line 1 2))
			     (PATHS (split-string (substring line 3)))
			     (FROM (nth 0 PATHS))
			     ;; for a rename there will be PATH1 -> PATH2
			     (TO (if (= 3 (length PATHS)) (nth 2 PATHS) nil)))
			(when (-contains? ta-git-unmerged-states (list X Y))
			  (setq merge-p t))
			(message "%s" (list X Y FROM TO line))
			(list X Y FROM TO line)))))

    (if merge-p
	;; we need to add everything, and commit it.
	(progn
	  (message "Processing an unmerged state")
	  (mygit (format "git add *"))
	  (mygit (format "git commit -am \"committing a merge\"")))

      ;; we are not in a merge, so we just handle each line.  now we
      ;; loop through the lines and handle them. We try to handle
      ;; individual lines so we can create more meaningful, single
      ;; file commits.
      (loop for (X Y FROM TO LINE) in lines
	    do
	    (message "handling %s" LINE)
	    (cond
	     ;; untracked files get added and committed.
	     ((equal (list X Y) '("?" "?"))
	      (mygit (format "git add %s" FROM))
	      (mygit
	       (format "git commit %s -m \"adding untracked file: %s.\""
		       FROM FROM)))
	 
	     ;; user rename
	     ((equal (list X Y) '("R" " "))
	      (mygit (format "git commit %s -m \"rename %s to %s\"" FROM FROM TO))
	      (mygit (format "git add %s" TO))
	      (mygit (format "git commit %s -m \"adding %s\"" TO TO)))
	 
	     ;; rename and modify
	     ((equal (list X Y) '("R" "M"))
	      ;; commit the rename
	      (mygit (format "git commit %s -m \"rename %s to %s\"" FROM FROM TO))
	      (mygit (format "git commit %s -m \"changes in %s\"" TO TO)))
	 
	     ;; added file
	     ((equal (list X Y) '("A" " "))
	      (mygit (format "git commit %s -m  \"Adding %s\"" FROM FROM)))
	 
	     ;; deleted file
	     ((equal (list X Y) '(" " "D"))
	      (mygit (format "git commit %s -m \"Deleting %s\"" FROM FROM)))
	 
	     ;; modified file
	     ((or (string= X "M")
		  (string= Y "M"))
	      (mygit (format "git add %s" FROM))
	      (mygit (format "git commit %s -m \"changes in %s\"" FROM FROM)))
      
	     ;; catch everything else
	     (t
	      (mygit (format "git add %s" FROM))
	      (mygit (format "git commit %s -m \"%s\"" FROM LINE))))))))

  
(defun ta-git-update-file ()
  "Update the current file.

This is tricky because we cannot just pull a file. We have to save all files, commit them, then pull. It may be the case that this causes a merge conflict, which we then need to address.

The strategy is to check git status --porcelain first, and get the repo into a clean state. Then we do the pull. Then, we check again and get back to a clean state if needed."
  (interactive)
  
  (save-some-buffers t t) ; make sure all files are saved.
  (with-current-directory
   ;; this switches us to the top git level which is where all the
   ;; paths are relative to.
   (s-trim
    (shell-command-to-string "git rev-parse --show-toplevel"))
   ;; first clean the repo
   (ta-git-make-repo-clean)
   
   ;; Next we are going to fetch
   (mygit "git fetch")

   ;; then merge, we assume with origin/master
   (mygit "git merge origin/master -m \"merging origin/master in\"")

   ;; there may be merge conflicts. we take them, and make the repo
   ;; clean again.
   (ta-git-make-repo-clean)
   (revert-buffer t t) ;; update this buffer
   ))

(provide 'techela-git)

;;; techela-git.el ends here
