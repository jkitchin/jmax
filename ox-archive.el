;; library to create an org-zip archive

(defun ox-archive-create-zip (&optional tag)
  "create a zip archive from the current org-file. All references to files will be copied to the archive."
  (interactive)
  (let* (
	 (org-file (buffer-name))
	 (base-name (file-name-sans-extension org-file))
	 ;; directory to save all exports in, using the current date
	 (org-archive (concat base-name
			      (or  
			       tag 
			       (format-time-string "%Y-%m-%d" (current-time)))))
	 ;; Name of the zip file we will use.
	 (org-archive-zip (concat org-archive ".zip"))
	 link-list
	 )
    
    ;; delete zip file if it exists
    (when (file-exists-p org-archive-zip)
      (delete-file org-archive-zip))

    ;; delete the directory if it exists
    (when (file-exists-p org-archive) 
      (delete-directory org-archive t))

    ;; make directory
    (make-directory org-archive t)

    ;; get list of links, copy files and save names
    (setq link-list 
	  (let ((parsetree (org-element-parse-buffer))
		(counter 0))
	    (org-element-map parsetree 'link
	      (lambda (link) 
		(let* ((type (nth 0 link))
		       (plist (nth 1 link))
		       (content (nth 2 link))
		       (path (plist-get plist :path))
		       (type (plist-get plist ':type))
		       (fname (car (last (split-string path "/"))))
		       (temporary-file-directory org-archive)
		       (new-file)
		       )     
		  (cond
		   ;; regular file with content
		   ((and (string= type "file")  content)
		    (setq new-file  (make-temp-file (file-name-sans-extension fname) nil 
						    (concat "." (file-name-extension fname))))
		    (with-temp-file new-file
		      (insert-file-contents path))
		    (format "[[./%s][%s]] " (file-name-nondirectory new-file) content))
		   ;; regular file with no content
		   ((and (string= type "file"))
		    (setq new-file  (make-temp-file (file-name-sans-extension fname) nil 
						    (concat "." (file-name-extension fname))))
		    (with-temp-file new-file
		      (insert-file-contents path))
		    (format "[[./%s]] " (file-name-nondirectory new-file)))
		   (t nil)))))))
    
    ;; create filter for links and export org buffer
    (let ((counter 0))
      (defun ox-mrkup-filter-link (text back-end info)
	(let ((link (nth counter link-list)))
	  (if (not (string= link "nil")) (setq output   (format "%s" link))
	    (setq output (format "%s" text)))
	  (setq counter (+ counter 1))
	  output))
      
      (let ((org-export-filter-link-functions '(ox-mrkup-filter-link)))
	(org-org-export-as-org)))
    
    (switch-to-buffer "*Org ORG Export*")
    (write-file (expand-file-name org-file org-archive))
    (shell-command (concat "zip -R " org-archive-zip " *"))
    (rename-file org-archive-zip (concat "../"org-archive ".zip"))
    (kill-buffer)

    (switch-to-buffer org-file)
    ;; get rid of temp-dir
    (delete-directory org-archive t)
    org-archive-zip))  

(defun ox-archive-create-and-mail (&optional tag)
  "create a zip file from current org-file and attach it to an email"
  (interactive)
  (let ((zip (ox-archive-create-zip tag)))
    (message-mail)
    (mml-attach-file zip)
    (message-goto-to)))

(provide 'ox-archive)
