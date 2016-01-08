;;; org-wdiff.el --- Word diff for org-mode files


;;; Commentary:
;; `org-wdiff' for two files
;; `org-wdiff-git' for git revisions

;;; Code:

(defcustom org-wdiff-cmd
  "wdiff -w [-- -x --] -y {++ -z ++} "
  "Command to run wdiff with.")


(defface git-diff-deletion-face
  `((t :foreground "red"))
  "Deleted text.")

(defface git-diff-insertion-face
  `((t :foreground "blue"))
  "Inserted text.")


(defun org-wdiff-fontify ()
  "Fontify a wdiff buffer."
  (font-lock-add-keywords
   nil
   '(("\\[--\\(\\([[:ascii:]]\\)+?\\)--\\]"
      0 '(face git-diff-deletion-face help-echo "deleted"))
     ("{\\+\\+\\(\\([[:ascii:]]\\)+?\\)\\+\\+}"
      0 '(face git-diff-insertion-face help-echo "inserted"))))
  (font-lock-fontify-buffer))


(defun org-wdiff (oldfile newfile)
  "Perform a word diff on OLDFILE and NEWFILE."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*org-wdiff*"))
  (erase-buffer)
  (org-mode)

  (insert
   (shell-command-to-string
    (format "%s %s %s" org-wdiff-cmd oldfile newfile)))
  (org-wdiff-fontify))


(defun org-wdiff-git (commit)
  "Perform a wdiff on the current version to the one in COMMIT."
  (interactive
   (list
    (helm :sources `((name . "commits")
		     (candidates . ,(mapcar (lambda (s)
					      (let ((commit (nth 1 (split-string s)))
						    msg)
						(string-match "|.*$" s)
						(cons (concat commit " " (match-string 0 s)) commit)))
					    (split-string (shell-command-to-string "git hist") "\n")))
		     (action . (lambda (commit)
				 (let* ((fname (buffer-file-name))
					(cmd (format "%s <(git show %s:%s) %s"
						     org-wdiff-cmd
						     commit fname
						     fname)))
				   (switch-to-buffer-other-window (get-buffer-create "*org-wdiff*"))
				   (erase-buffer)
				   (insert (shell-command-to-string cmd))
				   (org-wdiff-fontify)))))))))



(provide 'org-wdiff)

;;; org-wdiff.el ends here
