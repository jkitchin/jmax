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
  (length
   (loop for line in (split-string
		      (shell-command-to-string "git status --porcelain")
		      "\n")
	 when (string-match "^\?\?" line) collect line)))
  

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

; (ta-git-n-commits)
; (ta-git-n-untracked-files)
; (ta-git-n-modified-files)

(provide 'techela-git)
