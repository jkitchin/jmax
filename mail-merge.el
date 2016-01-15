;;; mail-merge.el --- mail-merge

;;; Commentary:
;; mail-merge library for using org-mode, mu4e and email.el to send mail
;; merges. The idea is to write a mail-template that can be formatted by
;; `s-format', use emacs-lisp to generate a data-source that will populate each
;; template and generate an org-mode heading for each message using
;; `mail-merge-make-headings'. Then, you can review the messages, edit as
;; needed, and finally send them via `mail-merge'.

;;; Code:

(defun mail-merge-make-headings (s-template data-source)
  "Create the mail headings.
S-TEMPLATE is an `s-format' string.  DATA-SOURCE is an alist of
entries that will be used to expand the S-TEMPLATE and generate
the headings.

Each entry in DATA-SOURCE must contain \"TO\" which is the email
address(es) to send the message to.  Also a \"SUBJECT\" must be
included, as well as a \"HEADLINE\" which will be used in the
headline instead of the subject.

The function will make a headline called Messages as a subheading
of the current heading, and each message will be a subheading of
the Messages heading.

an org-id will be created for each message.  you can use ${ID} in the S-TEMPLATE.

Each message will be tagged :unsent:

This function does not send the messages.

Example usage:

 (mail-merge-make-headings
 \"Dear ${name},

  Please check this file: ${file}.

  -----------------------
  Please do not delete this.
  [[id:${ID}]]

  \"
 '(((\"TO\" . \" some@person.com \")
    (\"name\" . \"Someone\")
    (\"file\" . \" tees.org \")
    (\"SUBJECT\" . \" [J] Person \"))))"
  ;; create Messages heading if needed
  (save-excursion
    (unless (and (outline-next-heading)
		 (string= "Messages" (nth 4 (org-heading-components))))
      (org-insert-heading)
      (insert "Messages")
      (org-do-demote)))

  ;; create Message entries
  (loop for data in data-source
	do (save-excursion
	     (save-restriction
	       (org-narrow-to-subtree)
	       (org-open-link-from-string "[[*Messages]]")
	       (org-insert-heading-after-current)
	       (org-do-demote)
	       (setq data (add-to-list 'data (cons "ID" (org-id-get-create))))
	       (outline-previous-heading)
	       (end-of-line)
	       (insert (or (cdr (assoc "HEADLINE" data))
			   (cdr (assoc "SUBJECT" data))))
	       (org-end-of-meta-data-and-drawers)
	       (insert (s-format s-template 'aget data))
	       ;; refill now that it is expanded
	       (save-restriction
		 (org-narrow-to-subtree)
		 (goto-char (point-min))
		 (fill-region (point-min) (or (re-search-forward "^--" nil t)
					      (point-max))))
	       (org-entry-put (point) "TO" (cdr (assoc "TO" data)))
	       (when (cdr (assoc "SUBJECT" data))
		 (org-entry-put (point) "SUBJECT" (cdr (assoc "SUBJECT" data))))
	       (org-set-tags-to (append '("unsent") (org-get-tags-at)))))))


(defun mail-merge-send-heading (&optional just-send)
  "Create message with org-heading body at point using heading properties.
With prefix arg, also send the message and move to the next one."
  (interactive "P")
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
	  (SUBJECT (replace-regexp-in-string
		    "{{.*}} "
		    ""
		    (or (org-entry-get (point) "SUBJECT" t)
			(nth 4 (org-heading-components)))))
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
      ;; move point back to the top
      (message-goto-to)
      (when just-send
	(message-send-and-exit))))
  (org-todo "DONE")

  (let ((tags (-remove
	       (lambda (x) (string= x "unsent"))
	       (org-get-tags-at))))
    (add-to-list 'tags "sent")
    (org-set-tags-to tags))
  (message  (format "sent to %s" (org-entry-get (point) "TO")))
  (outline-hide-entry)
  (outline-next-heading)
  (outline-show-entry))



(defun mail-merge ()
  "Run a mail-merge in the current heading.
This will map over entries tagged unsent with a TO property, and
mail the body of each heading using
`email-heading-body'. Headings tagged ignore will be ignored."
  (interactive)
  (org-map-entries
   (lambda ()
     (mail-merge-send-heading t)
     (sleep-for 0.2))
   ;; on headings that are tagged unsent
   "unsent-ignore+TO={.}"))

(provide 'mail-merge)

;;; mail-merge.el ends here
