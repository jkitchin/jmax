;;; techela-roster.el --- functions for handling the roster.
;; techela-roster.el - update course-admin with roster

;; we need to get new students and dropped students,


;;; Commentary:
;;

;;; Code:


(defun ta-roster ()
  "Return a data structure of userids and names.

The data structure is (userid :name Full name :email userid@somewhere.edu

Use it like this:
 (plist-get (cdr (assoc \"jboes\" (ta-roster))) :name)"
  (with-temp-buffer
    (insert-file-contents ta-roster)
    (let ((contents (cdr (csv-parse-buffer nil))))  ;; first line is header
      (mapcar (lambda (x)
		      (list (nth 8 x)                     ; userid
			    :name (format "%s %s"
					  (nth 6 x)       ; first name
					  (nth 5 x))      ; last name
			    :email (nth 9 x)))
		    contents))))


(defun ta-have-user-pubkey-p (userid)
  "Return whether we have the ssh.pub key for USERID."
  (interactive
   (list
    (ido-completing-read "Userid: " (ta-get-userids) nil t)))

  (with-current-directory
   (expand-file-name
    "keydir"
    ta-gitolite-admin-dir)
   (file-exists-p (format "%s.pub" userid))))

(defun ta-open-roster-to-user (userid)
  "Open roster.org to USERID."
  (interactive "sUserid: ")
  (find-file (expand-file-name "roster.org" ta-gitolite-admin-dir))
  (goto-char (point-min))
  (re-search-forward userid)
  (show-subtree))


(defun ta-check-pub-keys ()
  "Generate a buffer showing which students we are missing ssh.pub keys."
  (interactive)
  (switch-to-buffer "*ta pub keys*")
  (erase-buffer)
  (insert "#+TITLE: Check if we have ssh.pub keys for users\n\n")
  (dolist (userid (sort (ta-get-userids) 'string-lessp))
    (unless (ta-have-user-pubkey-p userid)
    (insert
     (format
      "%15s missing  %s\n"
      (format "[[elisp:(ta-open-roster-to-user \"%s\")][%s]]"
	      userid
	      userid)
      (format "[[elisp:(progn (ta-email \"%s@andrew.cmu.edu\")(message-goto-subject)(insert \" Missing ssh pub key\")(message-goto-body)(insert \"Dear %s,\\n\\nI need you to run M-x techela-register to generate and send my your ssh.pub key. Please see me or the TAs if you need help.\\n\\nThanks,\\nProfessor Kitchin\\n\"))][Email %s]]"
	      userid
	      (plist-get (cdr (assoc userid (ta-roster))) :name) userid)))))
  (org-mode))


(defun ta-update-roster ()
  "Update the list of students in conf/students.conf group.

You run this after you have modified the roster.dat file.  This
function figures out which students are new, which students are
dropped."
  (interactive)
  (let*
      ((roster (ta-roster))
       ;; instructor is just test account that acts like a student
       (new-roster-userids (mapcar (lambda (x) (car x)) roster))
       (roster-org-file (expand-file-name "roster.org" ta-gitolite-admin-dir))
       (userids-in-roster-org (with-temp-buffer
				(org-mode)
				(if (file-exists-p roster-org-file)
				    (progn
				      (insert-file-contents roster-org-file)
				      (org-map-entries
				       (lambda () (org-entry-get (point) "CUSTOM_ID"))))
					; else
				  '())))

       (student-conf-file (expand-file-name
			   "conf/students.conf"
			   ta-gitolite-admin-dir))
       (userids-in-conf (with-temp-buffer
			   (insert-file-contents student-conf-file)
			   (split-string
			    (nth 1 (split-string (buffer-string) "=")) " " t)))
       (key-dir (file-name-as-directory
		 (expand-file-name "keydir"
				   ta-gitolite-admin-dir)))
       (new-students (-difference new-roster-userids userids-in-conf))
       (dropped-students (-difference userids-in-conf new-roster-userids)))

    ;; Now we update the roster.org
    (switch-to-buffer (get-buffer-create "*Roster update*"))
    (erase-buffer)

    (when new-students
      (insert "New students added\n" "==================\n")
      (dolist (userid new-students nil)
	(insert (format "%20s %30s (pub-key exists:%s)\n"
			userid
			(plist-get
			 (cdr (assoc userid (ta-roster)))
			 :name)
			(file-exists-p (expand-file-name
					(format "%s.pub" userid)
					key-dir))))

	;; now add entry to roster.org
	(with-current-buffer (find-file-noselect roster-org-file)
	  (end-of-buffer)
	  (insert "\n* " (plist-get
			  (cdr (assoc userid roster))
			  :name)"\n")
	  (forward-line)
	  (org-set-tags-to (replace-regexp-in-string "-" "_" ta-course-name))
	  (org-entry-put (point) "CUSTOM_ID" userid)
	  (org-entry-put (point) "EMAIL" (if (string-match "@" userid)
					     userid
					   (concat userid "@" ta-email-host)))
	  (org-entry-put (point) "ADDED" (format-time-string "[%Y-%m-%d %a]" (current-time)))
	  (save-buffer))))

    ;; we need to remove the userid.pub file
    (when dropped-students
      (dolist (userid dropped-students nil)
	(let ((user-pub-key (format "%s.pub" userid)))
	  (with-current-directory
	   key-dir
	   (if (file-exists-p user-pub-key)
	       (progn
		 (mygit (format "git rm %s" user-pub-key))
		 (mygit (format "git commit %s -m \"deleted %s. %s dropped the class\""
				user-pub-key user-pub-key userid)))
	     (warn "%s not found for %s" user-pub-key userid))))
	;; tag students in roster.org with dropped
	(with-current-buffer (find-file-noselect roster-org-file)
	  (org-open-link-from-string (format "[[#%s]]" userid))
	  (org-entry-put (point) "DROPPED" (format-time-string "[%Y-%m-%d %a]" (current-time)))
	  (org-set-tags-to "dropped")
	  (save-buffer))
	)
      (insert "\nDropped students\n" "==================\n"
	      (mapconcat 'identity dropped-students "\n")))

    (with-current-directory
     ta-gitolite-admin-dir
     (mygit "git add roster.org")
     (mygit "git commit roster.org -m \"Updated roster\"")
     (mygit "git push"))

    ;; write out new conf file
    (with-temp-file student-conf-file
      (insert "@students = " (mapconcat 'identity new-roster-userids " ")))

    ;; Now we should commit that change, and push it
    (with-current-directory
     (file-name-directory student-conf-file)
     (mygit "git commit students.conf -m \"updated the students.conf\"")
     (mygit "git push")
     )))


(defun ta-add-roster-note (note userid)
  "Add a NOTE to the USERID entry in roster.org."
  (interactive
   (list
    (ido-completing-read
     "Userid: "
     (with-current-buffer (find-file-noselect (expand-file-name
					       "roster.org"
					       ta-gitolite-admin-dir))
       (org-map-entries (lambda () (org-entry-get (point) "CUSTOM_ID")))))
    (read-from-minibuffer "Note: ")))

  (with-current-buffer (find-file-noselect (expand-file-name
					    "roster.org"
					    ta-gitolite-admin-dir))
    (save-restriction
      (widen)
      (org-open-link-from-string (format "[[#%s]]" userid))
      (org-narrow-to-subtree)
      (goto-char (point-max))
      (insert (format "\n%s %s\n"
		      (format-time-string "[%Y-%m-%d %a]" (current-time))
		      note))
      (save-buffer)))

  (with-current-directory
     ta-gitolite-admin-dir
     (mygit "git add roster.org")
     (mygit "git commit roster.org -m \"Updated roster with a note.\"")
     (mygit "git push"))
   )



(provide 'techela-roster)

;;; techela-roster.el ends here
