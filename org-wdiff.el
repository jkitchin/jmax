;;; org-wdiff.el --- Word diff for org-mode files


;;; Commentary:
;; `org-wdiff' for two files
;; `org-wdiff-git' for git revisions

;; There is a minor mode that enables something like track changes for
;; accepting/rejecting revisions reported by wdiff.

;; This is not perfect, but it seems to work well on relatively simple changes
;; in org-files.

;;; Code:

(defcustom org-wdiff-cmd
  "wdiff -w [-- -x --] -y {++ -z ++} "
  "Command to run wdiff with.")


(defcustom org-wdiff-deleted-regexp
  "\\[--\\(\\([[:ascii:]]\\)+?\\)--\\]"
  "Regexp for deleted text.")

(defcustom org-wdiff-inserted-regexp
  "{\\+\\+\\(\\([[:ascii:]]\\)+?\\)\\+\\+}"
  "Regexp for inserted text.")


(defface git-diff-deletion-face
  `((t :foreground "red" :weight bold))
  "Deleted text.")


(defface git-diff-insertion-face
  `((t :foreground "blue" :weight bold))
  "Inserted text.")


(defvar org-wdiff-original-filename nil
  "Global variable for saving file.")


(defun org-wdiff-fontify ()
  "Fontify a wdiff buffer."
  (font-lock-add-keywords
   nil
   `((,org-wdiff-deleted-regexp
     0 '(face git-diff-deletion-face deleted t help-echo "deleted"))
    (,org-wdiff-inserted-regexp
     0 '(face git-diff-insertion-face inserted t help-echo "inserted"))))
  (add-to-list 'font-lock-extra-managed-props 'deleted)
  (add-to-list 'font-lock-extra-managed-props 'inserted)
  (font-lock-fontify-buffer))


(defun org-wdiff (oldfile newfile)
  "Perform a word diff on OLDFILE and NEWFILE."
  (interactive "fOld file: \nfNew file:")
  (switch-to-buffer-other-window (get-buffer-create "*org-wdiff*"))
  (erase-buffer)
  (org-mode)

  (insert
   (shell-command-to-string
    (format "%s %s %s" org-wdiff-cmd oldfile newfile)))
  (org-wdiff-fontify)
  (track-change-mode))



(defun org-wdiff-git (commit)
  "Perform a wdiff on the current version to the one in a git COMMIT."
  (interactive
   (list
    (helm :sources `((name . "commits")
		     (candidates . ,(mapcar (lambda (s)
					      (let ((commit (nth 1 (split-string s)))
						    msg)
						(string-match "|.*$" s)
						(cons (concat commit " " (match-string 0 s)) commit)))
					    (split-string
					     (shell-command-to-string "git hist") "\n")))
		     (action . (lambda (commit)
				 (let* ((fname
					 (file-relative-name
					  (buffer-file-name)
					  (vc-git-root (buffer-file-name))))
					(git-root (vc-git-root (buffer-file-name)))
					(mmode major-mode)
					(cmd (format "%s <(git show %s:%s) %s"
						     org-wdiff-cmd
						     commit fname
						     fname))
					(buf (get-buffer-create "*org-wdiff-git*")))
				   (setq org-wdiff-original-filename fname)
				   (switch-to-buffer-other-window buf)
				   (funcall mmode)
				   (erase-buffer)
				   (let ((default-directory git-root))
				     (insert (shell-command-to-string cmd)))
				   (org-wdiff-fontify)
				   (track-changes-mode)
				   (goto-char (point-min))))))))))


(defun next-revision ()
  "Move point to beginning of next revision."
  (interactive)
  (goto-char
   (min (or (next-single-property-change (point) 'deleted) most-positive-fixnum)
	(or (next-single-property-change (point) 'inserted) most-positive-fixnum)
	(point-max)))

  ;; when at the beginning of a revision, the next property change is usually a
  ;; space after it, not the beginning of the next revision. Here we make sure
  ;; to get to the next one.
  (when (not (= (point) (point-max)))
    (unless (or (get-text-property (point) 'deleted)
		(get-text-property (point) 'inserted))
      (next-revision))))

(defun previous-revision ()
  "Move point to beginning of the previous revision."
  (interactive)
  (when (not (bobp))
    (goto-char
     (max (or (previous-single-property-change
	       (point) 'deleted)
	      0)
	  (or (previous-single-property-change
	       (point) 'inserted)
	      0)))

    (when (not (bobp))
      (unless (or (get-text-property (point) 'deleted)
		  (get-text-property (point) 'inserted))
	(previous-revision)))))


(defun accept-revision ()
  "Accept the revision at point and go to next revision."
  (interactive)
  (cond
   ;; delete the whole marked text
   ((get-text-property (point) 'deleted)
    (save-excursion
      (let (start end)
	(re-search-forward "--\\]")
	(setq end (point))
	(re-search-backward "\\[--")
	(setq start (point))
	(setf (buffer-substring start end) ""))))
   ;; Just delete the markers
   ((get-text-property (point) 'inserted)
    (save-excursion
      (let (start end)
	(re-search-forward "\\+\\+}")
	(setq end (point))
	(replace-match "")
	(re-search-backward "{\\+\\+")
	(setq start (point))
	(replace-match "")))))
  (next-revision))


(defun reject-revision ()
  "Reject the revision at point."
  (interactive)
  (cond
   ;; delete the markers, keep the text
   ((get-text-property (point) 'deleted)
    (save-excursion
      (let (start end)
	(re-search-forward "--\\]")
	(setq end (point))
	(replace-match "")
	(re-search-backward "\\[--")
	(setq start (point))
	(replace-match ""))))
   ;; delete the whole marked text
   ((get-text-property (point) 'inserted)
    (save-excursion
      (let (start end)
	(re-search-forward "\\+\\+}")
	(setq end (point))
	(re-search-backward "{\\+\\+")
	(setq start (point))
	(setf (buffer-substring start end) "")))))
  (next-revision))


(defun org-wdiff-save ()
  "Save the *org-wdiff-git* buffer in the file it was made from."
  (interactive)
  (write-file org-wdiff-original-filename))


(defvar track-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-revision)
    (define-key map "p" 'previous-revision)
    (define-key map "a" 'accept-revision)
    (define-key map "r" 'reject-revision)
    (define-key map "q" 'quit-window)
    (define-key map "s" 'org-wdiff-save)
    map)
  "Keymap for track-changes-mode.")


(define-minor-mode track-changes-mode
  "Minor mode for track-changes.

\\{track-changes-mode-map}"
  :init-value nil
  :lighter "Track Changes"
  :keymap track-changes-mode-map)


(provide 'org-wdiff)

;;; org-wdiff.el ends here
