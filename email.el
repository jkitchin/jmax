(defun email-region (start end)
  "Send region as the body of an email"
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (compose-mail-other-frame)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

(defun email-region-as-attachment (start end)
  "Send the region as an attachment in an email"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (let ((cb (buffer-name)))
      (compose-mail-other-frame)
      (mml-attach-buffer cb)
      (message-goto-to))))

(defun email-heading ()
  "Send the current org-mode heading as the body of an email"
  (interactive)
  (org-mark-subtree)
  (email-region (point) (mark)))
