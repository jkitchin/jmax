(defun ta-git-n-commits ()
  "returns how many commits differ locally and remotely.

(0 0) means you are up to date as far as commits go
(n 0) means you are ahead of the remote and need to push
(0 n) means you are behind the remote and need to pull
(m n) means there is a divergence, and a pull and push will be required.
"
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
  "return number of untracked files.
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
  "return number of modified, but uncommitted files.
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

git ls-files filename returns an empty string if filename is not under git control
"
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
\" when the file is modified.
"
  (interactive)
  (when (ta-git-buffer-tracked-p)
    (save-buffer)
    (string-match
     "^ M"
     (shell-command-to-string
      (format "git status --porcelain %s"
	      (file-name-nondirectory
	       (buffer-file-name)))))))

(provide 'techela-git)
