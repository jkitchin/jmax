(require 'json)

(defun expand-template (s)
  "expand a template containing %{} with the eval of its contents"
  (replace-regexp-in-string "%{\\([^}]+\\)}"
                            (lambda (arg)
                              (let ((sexp (substring arg 2 -1)))
                                (format "%s" (eval (read sexp))))) s))

(defun doi-to-bibtex-article (doi)
 "insert a bibtex entry for doi at point"
 (interactive "sDOI: ")
 (let ((url-request-method "GET") 
       (url-mime-accept-string "application/citeproc+json")
       (json-object-type 'plist)
       type
       results
       author
       title
       journal
       year
       volume
       number
       pages
       month
       url json-data)
   
   (setq results 
	 (with-current-buffer
	     (url-retrieve-synchronously
	      (concat "http://dx.doi.org/" doi))
	 (json-read-from-string (buffer-substring url-http-end-of-headers (point-max))))
         type (plist-get results :type)
	 author (mapconcat (lambda (x) (concat (plist-get x :given) " " (plist-get x :family)))
		     (plist-get results :author) " and ")
	 title (plist-get results :title)
	 journal (plist-get results :container-title)
	 volume (plist-get results :volume)
	 issue (plist-get results :issue)
	 year (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 0)
	 month (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 1)
	 pages (plist-get results :page)
	 doi (plist-get results :DOI)
	 url (plist-get results :URL)
	 json-data (format "%s" results))

   (when (string= type "journal-article")

     (expand-template "@article{,
  author = 	 {%{author}},
  title = 	 {%{title}},
  journal = 	 {%{journal}},
  year = 	 {%{year}},
  volume = 	 {%{volume}},
  number = 	 {%{issue}},
  pages = 	 {%{pages}},
  doi =          {%{doi}},
  url =          {%{url}},
  month = 	 {%{month}},
  json = 	 {%{json-data}}
}"))))

(defun insert-bibtex-entry-from-doi (doi)
  "insert bibtex entry from a doi"
  (interactive "sDOI: ")
  (insert (doi-to-bibtex-article doi)))

(defun bibtex-set-field (field value)
  "set field to value in bibtex file. create field if it does not exist"
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
	;; we found a field
	(progn
	  (goto-char (car (cdr found)))
	  (when value
	    (bibtex-kill-field)
	    (bibtex-make-field field)
	    (backward-char)
	    (insert value)))
      ;; make a new field
      (message "new field being made")
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field)
      (backward-char)
      (insert value))))

(defun plist-get-keys (plist)
   "return keys in a plist"
  (loop
   for key in results by #'cddr collect key))

(defun update-bibtex-entry-from-doi (doi)
  "update fields in a bibtex entry from the doi."
  (interactive (list (or (bibtex-autokey-get-field "doi") (read-string "DOI: "))))
  (let* ((url-request-method "GET") 
	(url-mime-accept-string "application/citeproc+json")
	(json-object-type 'plist)
	(results (with-current-buffer
		    (url-retrieve-synchronously
		     (concat "http://dx.doi.org/" doi))
		  (json-read-from-string
		   (buffer-substring url-http-end-of-headers (point-max)))))
	(type (plist-get results :type))
	(author (mapconcat
		(lambda (x) (concat (plist-get x :given)
				    " " (plist-get x :family)))
		(plist-get results :author) " and "))
	(title (plist-get results :title))
	(journal (plist-get results :container-title))
	(year (format "%s"
		       (elt
			(elt
			 (plist-get
			  (plist-get results :issued) :date-parts) 0) 0)))
	
	(volume (plist-get results :volume))
	(number (or (plist-get results :issue) ""))
	(pages (or (plist-get results :page) ""))
	(month (format "%s"
			(elt
			 (elt
			  (plist-get
			   (plist-get results :issued) :date-parts) 0) 1)))
	(url (or (plist-get results :URL) ""))
	(doi (plist-get results :DOI))
	(annote (format "%s" results)))
    
    ;; map the json fields to bibtex fields
    (setq mapping '((:author . (bibtex-set-field "author" author))
		    (:title . (bibtex-set-field "title" title))
		    (:container-title . (bibtex-set-field "journal" journal))
		    (:issued . (progn
				 (bibtex-set-field "year" year)
				 (bibtex-set-field "month" month)))
		    (:volume . (bibtex-set-field "volume" volume))
		    (:issue . (bibtex-set-field "issue" issue))
		    (:page . (bibtex-set-field "pages" pages))
		    (:DOI . (bibtex-set-field "doi" doi))
		    (:URL . (bibtex-set-field "url" url))))

    ;; now we have code to run for each entry. we map over them and evaluate the code
    (mapcar
     (lambda (key)
       (eval (cdr (assoc key mapping))))
     (plist-get-keys results)))
  
  ; reclean entry, but keep existing key. check if it exists
  (if (bibtex-key-in-head)
      (org-ref-clean-bibtex-entry t)
    (org-ref-clean-bibtex-entry)))
