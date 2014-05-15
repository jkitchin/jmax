
;; this provides gnus-dired-attach which allows you to mark files and
;; attach them to an email

(require 'gnus-dired)

(defun email-region (start end)
  "Send region as the body of an email"
  (interactive "r")
  (let ((content (buffer-substring start end)) content-with-citations)
    (setq content-with-citations (with-temp-buffer
				   (org-mode)
				   (insert content)
				   ;(org-ref-extract-bibtex-entries)
				   (buffer-string)))
    (compose-mail-other-frame)
    (message-goto-body)
    (insert content-with-citations)
    (message-goto-to)))

(defun email-region-as-attachment (start end)
  "Send the region as an attachment in an email"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (let ((content (buffer-substring start end))
	  (cb (buffer-name)))
      (set-buffer (get-buffer-create "*org-email-region*"))
      (org-mode)
      (insert content)
      ;(org-ref-extract-bibtex-entries)
      (compose-mail-other-frame)
      (mml-attach-buffer "*org-email-region*")
      (message-goto-to))))

(defun email-heading ()
  "Send the current org-mode heading as the body of an email"
  (interactive)
  (org-mark-subtree)
  (email-region (point) (mark)))
