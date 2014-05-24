(require 'json)

(defun expand-template (s)
  "expand a template containing %{} with the eval of its contents"
  (replace-regexp-in-string "%{\\([^}]+\\)}"
                            (lambda (arg)
                              (let ((sexp (substring arg 2 -1)))
                                (format "%s" (eval (read sexp))))) s))

(defun doi-to-bibtex-article (doi)
 "return a bibtex entry for doi at point"
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
}"))))

(defun insert-bibtex-entry-from-doi (doi)
  "insert bibtex entry from a doi"
  (interactive "sDOI: ")
  (insert (doi-to-bibtex-article doi))
  (backward-char)
  (if (bibtex-key-in-head nil)
       (org-ref-clean-bibtex-entry t)
     (org-ref-clean-bibtex-entry))
   ;; try to get pdf
   (doi-utils-get-bibtex-entry-pdf))

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
  (interactive (list
		(or (replace-regexp-in-string "http://dx.doi.org/" "" (bibtex-autokey-get-field "doi"))
		    (read-string "DOI: "))))
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
		    (:issue . (bibtex-set-field "number" number))
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

(defvar *doi-utils-waiting* t
  "stores redirect url from a callback function")

(defvar *doi-utils-redirect* nil
  "stores redirect url from a callback function")

(defun doi-utils-redirect-callback (&optional status)
  "callback for url-retrieve to set the redirect"
  (when (plist-get status :error)
    (signal (car (plist-get status :error)) (cdr(plist-get status :error))))
  (when (plist-get status :redirect) ;  is nil if there none
    (message "*doi-utils-redirect* set to %s"
	     (setq *doi-utils-redirect* (plist-get status :redirect))))
  ;; we have done our job, so we are not waiting any more.
  (setq *doi-utils-waiting* nil))


(defun doi-utils-get-redirect (doi)
  "get redirect url from dx.doi.org/doi"
  (setq *doi-utils-waiting* t)
  (url-retrieve 
   (format "http://dx.doi.org/%s" doi)
   'doi-utils-redirect-callback)
  ; I suspect we need to wait here for the asynchronous process to finish.
  (while *doi-utils-waiting* (sleep-for   0.1)))

(defvar *doi-utils-pdf-url* nil)

(defun doi-utils-get-science-direct-pdf-url (redirect-url)
  "science direct hides the pdf url in html. we get it out here"
  (url-retrieve redirect-url
		(lambda (status)
		  (beginning-of-buffer)
		  (re-search-forward "pdfurl=\"\\([^\"]*\\)\"")
		  (setq *doi-utils-pdf-url* (match-string 1))))
  *doi-utils-pdf-url*)


(defun doi-utils-get-pdf-url (doi)
  "returns a url to a pdf for the doi if one can be calculated"
  (doi-utils-get-redirect doi)
  
  (unless *doi-utils-redirect*
    (error "No redirect found for %s" doi))
  
  (cond
   ;; APS journals
   ((string-match "^http://journals.aps.org" *doi-utils-redirect*)
    (message "APS journal at %s found" *doi-utils-redirect*)
    (replace-regexp-in-string "/abstract/" "/pdf/" *doi-utils-redirect*))

   ;; Science
   ((string-match "^http://www.sciencemag.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf"))

   ;; Nature
   ((string-match "^http://www.nature.com/nature" *doi-utils-redirect*)
    (let ((result *doi-utils-redirect*))
      (setq result (replace-regexp-in-string "/full/" "/pdf/" result))
      (replace-regexp-in-string "\.html$" "\.pdf" result)))

   ;; Nature Materials
   ((string-match "^http://www.nature.com/nmat" *doi-utils-redirect*)
    (let ((result *doi-utils-redirect*))
      (setq result (replace-regexp-in-string "/abs/" "/pdf/" result))
      (replace-regexp-in-string "\.html$" "\.pdf" result)))

   ;; Wiley - it is aguess this works for all of them
   ((string-match "^http://onlinelibrary.wiley.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/abstract" "/pdf" *doi-utils-redirect*))

   ;; Springer
   ((string-match "^http://link.springer.com" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".pdf"))

   ;; ACS journals
   ((string-match "^http://pubs.acs.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/" "/pdf/" *doi-utils-redirect*))

   ;; iop
   ;; http://iopscience.iop.org/0953-8984/21/39/395502/
   ;; http://iopscience.iop.org/0953-8984/21/39/395502/pdf/0953-8984_21_39_395502.pdf
   ((string-match "^http://iopscience.iop.org" *doi-utils-redirect*)
    (let ((tail (replace-regexp-in-string "^http://iopscience.iop.org" "" *doi-utils-redirect*)))
      (concat "http://iopscience.iop.org" tail "/pdf" (replace-regexp-in-string "/" "_" tail) ".pdf")))

   ;; jstor
   ;; http://www.jstor.org/stable/2245477
   ;; http://www.jstor.org/stable/pdfplus/2245477.pdf
   ((string-match "^http://www.jstor.org" *doi-utils-redirect*)
    (concat (replace-regexp-in-string "/stable/" "/stable/pdfplus/" *doi-utils-redirect*) ".pdf"))

   ;; AIP - this feels like major hackery but, these urls are complicated
   ((string-match "^http://scitation.aip.org" *doi-utils-redirect*)
    ;; get stuff after content
    (let (p1 p2 s p3)
      (setq p2 (replace-regexp-in-string "^http://scitation.aip.org/" "" *doi-utils-redirect*))
      (setq s (split-string p2 "/"))
      (setq p1 (mapconcat 'identity (-remove-at-indices '(0 6) s) "/"))
      (setq p3 (concat "/" (nth 0 s) (nth 1 s) "/" (nth 2 s) "/" (nth 3 s)))
      (format "http://scitation.aip.org/deliver/fulltext/%s.pdf?itemId=/%s&mimeType=pdf&containerItemId=%s"
	      p1 p2 p3)))

   ;; Science direct - must retrieve from the html page
   ((string-match "^http://www.sciencedirect.com" *doi-utils-redirect*)
    (doi-utils-get-science-direct-pdf-url *doi-utils-redirect*)
    *doi-utils-pdf-url*)

   ;; tandfonline
   ;; http://www.tandfonline.com/doi/full/10.1080/08927022.2013.844898
   ;; http://www.tandfonline.com/doi/pdf/10.1080/08927022.2013.844898
   ((string-match "^http://www.tandfonline.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/\\|/full/" "/pdf/" *doi-utils-redirect*))
  
   (t
    (message "redirect %s is unknown" *doi-utils-redirect*)
    nil)))
   
  
(defun doi-utils-get-bibtex-entry-pdf ()
  "get pdf for entry at point. The entry must have a doi. The pdf will be saved to `org-ref-pdf-directory', by the name %s.pdf where %s is the bibtex label. Files will not be overwritten."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry) 
    (let ((doi (replace-regexp-in-string "http://dx.doi.org/" "" (bibtex-autokey-get-field "doi")))	       
	  (key)
	  (pdf-url)
	  (pdf-file))
      (re-search-forward bibtex-entry-maybe-empty-head)
      (setq key (match-string bibtex-key-in-head))
      (setq pdf-file (concat org-ref-pdf-directory key ".pdf"))
      (when doi
	(setq pdf-url (doi-utils-get-pdf-url doi))
	(if pdf-url
	    (if (file-exists-p pdf-file)
		(message "%s already exists" pdf-file)
	      (url-copy-file pdf-url pdf-file nil)
	      (message "%s saved" pdf-file))
	  (message "no url for pdf found"))))))

      
