;;; email.el --- Email functions

;; this provides gnus-dired-attach which allows you to mark files and
;; attach them to an email

;;; Commentary:
;;

(require 'gnus-dired)

;;; Code:

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
  "Global variable to store point in for returning.")

(defvar *email-to-addresses* nil
  "Global variable to store to address in email.")

(defvar *email-mu4e-link-to-message* nil
  "Global var to store mu4e link to Message-ID of last email.")


(defun email-heading-return ()
  "After returning from compose do this."
  (switch-to-buffer (marker-buffer  *email-heading-point*))
  (goto-char (marker-position  *email-heading-point*))
  (setq *email-heading-point* nil)
  (org-set-property "SENT-ON" (current-time-string))
  ;; reset this incase you added new ones
  (org-set-property "TO" *email-to-addresses*)
  (org-set-property "Message-ID" *email-mu4e-link-to-message*)

  (dolist (email *email-to-addresses*)
    (let ((contact (org-contacts-filter nil nil `("EMAIL" . ,email))))
      (when (= 1 (length contact))
	(message-box "%s %s" msgid contact)
	(setq contact (car contact))
	(org-entry-put
	 (nth 1 contact) "LAST_SENT_EMAIL"
	 *email-mu4e-link-to-message*)))))


(defun email-send-action ()
  "Send action for `compose-mail'."
  (setq *email-to-addresses* (mail-fetch-field "To"))
  (setq *email-mu4e-link-to-message*
	(format "[[mu4e:msgid:%s][%s (%s)]]"
		;; borrowed from https://github.com/girzel/gnorb/blob/master/gnorb-utils.el#L137
		(replace-regexp-in-string
		 "\\(\\`<\\|>\\'\\)" "" (mail-fetch-field "Message-ID"))
		(mail-fetch-field "Subject")
		(current-time-string))))

(defun email-heading ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
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
	(message-goto-to)))))

(defun email-heading-body ()
  "Send the current org-mode heading content as the body of an email.

use these properties to create the email.
TO
CC
BCC
SUBJECT (or use the headline)
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. This is overwritten on
subsequent sends."
  (interactive)
					; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
                       (buffer-substring
			(org-end-of-meta-data-and-drawers)
                        (org-element-property :contents-end headline)))))
          (TO (org-entry-get (point) "TO" t))
          (CC (org-entry-get (point) "CC" t))
          (BCC (org-entry-get (point) "BCC" t))
          (SUBJECT (or (org-entry-get (point) "SUBJECT" t)
		       (nth 4 (org-heading-components))))
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
        (message-goto-to)))))


(defun email-region-as-attachment (start end)
  "Send the region as an attachment in an email.
Argument START start of region.
Argument END end of region."
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

(provide 'email)

;;; email.el ends here
