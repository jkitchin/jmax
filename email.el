
;; this provides gnus-dired-attach which allows you to mark files and
;; attach them to an email
(require 'gnus-dired)

(defun email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

(defun email-buffer ()
  "Send region as the body of an email."
  (interactive)
  (let ((content (buffer-string)))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

(defvar *email-heading-point* nil
  "global variable to store point in for returning")

(defvar *email-to-addresses* nil
  "global variable to store to address in email")

(defun email-heading-return ()
  "after returning from compose do this"
  (switch-to-buffer (marker-buffer  *email-heading-point*))
  (goto-char (marker-position  *email-heading-point*))
  (setq *email-heading-point* nil)
  (org-set-property "SENT-ON" (current-time-string))
  ;; reset this incase you added new ones
  (org-set-property "TO" *email-to-addresses*)
  )

(defun email-send-action ()
  "send action for compose-mail"
  (setq *email-to-addresses* (mail-fetch-field "To")))

(defun email-heading ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

save when it was sent as s SENT property. this is overwritten on
subsequent sends. could save them all in a logbook?
"
  (interactive)
  ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (org-mark-subtree)
    (let ((content (buffer-substring (point) (mark)))
	  (TO (org-entry-get (point) "TO" t))
	  (CC (org-entry-get (point) "CC" t))
	  (BCC (org-entry-get (point) "BCC" t))
	  (SUBJECT (nth 4 (org-heading-components)))
	  (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
	  (continue nil)
	  (switch-function nil)
	  (yank-action nil)
	  (send-actions '((email-send-action . nil)))
	  (return-action '(email-heading-return)))
      
      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
      (message-goto-body)
      (insert content)
      (when CC
	(message-goto-cc)
	(insert CC))
      (when BCC
	(message-goto-bcc)
	(insert BCC))
      (if TO
	  (message-goto-body)
	(message-goto-to))       
      )))


(defun email-region-as-attachment (start end)
  "Send the region as an attachment in an email"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (let ((content (buffer-substring start end))
	  (cb (buffer-name))
	  )
      (set-buffer (get-buffer-create "*org-email-region*"))
      (org-mode)
      (insert content)
      ;(org-ref-extract-bibtex-entries)
      (compose-mail-other-frame TO SUBJECT OTHER-HEADERS)
      (mml-attach-buffer "*org-email-region*")
      (message-goto-to))))



(defun email-bibtex-entry ()
  "Email current bibtex entry and pdf if it exists."
  (interactive)
  
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((key (reftex-get-bib-field "=key=" (bibtex-parse-entry t)))
	   (pdf (expand-file-name
		 (concat key ".pdf")
		 org-ref-pdf-directory)))
      (bibtex-copy-entry-as-kill)
      (compose-mail)
      (message-goto-body)
      (insert (pop bibtex-entry-kill-ring))
      (message-goto-subject)
      (insert key)
      (message "%s exists %s" pdf (file-exists-p pdf))
      (when (file-exists-p pdf)
	(mml-attach-file pdf))
      (message-goto-to))))
