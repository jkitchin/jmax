;;; doi-utils.el --- get bibtex entries and pdfs from a DOI

;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Lisp code to generate and update bibtex entries from a DOI, and to
;; download pdfs from publisher websites from a DOI.
;;
;; Package-Requires: ((org-ref))

(require 'json)

(defun expand-template (s)
  "expand a template containing %{} with the eval of its contents"
  (replace-regexp-in-string "%{\\([^}]+\\)}"
                            (lambda (arg)
                              (let ((sexp (substring arg 2 -1)))
                                (format "%s" (eval (read sexp))))) s))

(defun doi-to-bibtex-entry (doi)
 "return a bibtex entry as a string for the doi. Only articles are currently supported"
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
   
   (setq results (with-current-buffer
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
	 ;; month (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 1)
	 pages (plist-get results :page)
	 doi (plist-get results :DOI)
	 url (plist-get results :URL)
	 json-data (format "%s" results))

   (cond
    ((string= type "journal-article")
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
}"))
    (t (message-box "%s not supported yet." type)))))
 
(defun insert-bibtex-entry-from-doi (doi)
  "insert bibtex entry from a doi. Also cleans entry using
org-ref, and tries to download the corresponding pdf."
  (interactive "sDOI: ")
  (insert (doi-to-bibtex-entry doi))
  (backward-char)
  (if (bibtex-key-in-head nil)
       (org-ref-clean-bibtex-entry t)
     (org-ref-clean-bibtex-entry))
   ;; try to get pdf
   (doi-utils-get-bibtex-entry-pdf)
   (save-selected-window
     (org-ref-open-bibtex-notes)))

(defun add-bibtex-entry-from-doi (doi)
  "add entry to end of first entry in org-ref-default-bibliography."
  (interactive "sDOI: ")
  (find-file (car org-ref-default-bibliography))
  (end-of-buffer)
  (insert "\n\n")
  (insert-bibtex-entry-from-doi doi))

(defun add-bibtex-entry-from-region (start end)
  "add entry assuming region is a doi to end of first entry in org-ref-default-bibliography."
  (interactive "r")
  (let ((doi (buffer-substring start end)))
    (find-file (car org-ref-default-bibliography))
    (end-of-buffer)
    (insert "\n")
    (insert-bibtex-entry-from-doi doi)))

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
  "update fields in a bibtex entry from the doi. Every field will be updated, so previous changes will be lost."
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
    
    ;; map the json fields to bibtex fields. The code each field is mapped to is evaluated.
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
  "stores waiting state for url retrieval.")

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
  ;; we are going to wait until the url-retrieve is done
  (setq *doi-utils-waiting* t)
  ;; start with no redirect. it will be set in the callback.
  (setq *doi-utils-redirect* nil) 
  (url-retrieve 
   (format "http://dx.doi.org/%s" doi)
   'doi-utils-redirect-callback)
  ; I suspect we need to wait here for the asynchronous process to
  ; finish. we loop and sleep until the callback says it is done via
  ; `*doi-utils-waiting*'. this works as far as i can tell. Before I
  ; had to run this a few times to get it to work, which i suspect
  ; just gave the first one enough time to finish.
  (while *doi-utils-waiting* (sleep-for 0.1)))

(defvar *doi-utils-pdf-url* nil
  "stores url to pdf download from a callback function")

(defun doi-utils-get-science-direct-pdf-url (redirect-url)
  "science direct hides the pdf url in html. we get it out here"
  (setq *doi-utils-waiting* t)
  (url-retrieve redirect-url
		(lambda (status)
		  (beginning-of-buffer)
		  (re-search-forward "pdfurl=\"\\([^\"]*\\)\"")
		  (setq *doi-utils-pdf-url* (match-string 1))))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)

(defvar pdf-url-functions nil
  "list of functions that return a url to a pdf from a redirect url. Each function takes one argument, the redirect url. The function must return a pdf-url, or nil.")

(defun aps-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://journals.aps.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/abstract/" "/pdf/" *doi-utils-redirect*)))

(add-to-list 'pdf-url-functions 'aps-pdf-url)

(defun science-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.sciencemag.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))

(add-to-list 'pdf-url-functions 'science-pdf-url)

(defun nature-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.nature.com" *doi-utils-redirect*)
    (let ((result *doi-utils-redirect*))
      (setq result (replace-regexp-in-string "/full/" "/pdf/" result))
      (replace-regexp-in-string "\.html$" "\.pdf" result))))

(add-to-list 'pdf-url-functions 'nature-pdf-url)

(defun wiley-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://onlinelibrary.wiley.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/abstract" "/pdf" *doi-utils-redirect*)))

(add-to-list 'pdf-url-functions 'wiley-pdf-url)

(defun springer-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://link.springer.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/article/" "/content/pdf/" (concat *doi-utils-redirect* ".pdf"))))

(add-to-list 'pdf-url-functions 'springer-pdf-url)

(defun acs-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://pubs.acs.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/" "/pdf/" *doi-utils-redirect*)))

(add-to-list 'pdf-url-functions 'acs-pdf-url)

(defun iop-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://iopscience.iop.org" *doi-utils-redirect*)
    (let ((tail (replace-regexp-in-string "^http://iopscience.iop.org" "" *doi-utils-redirect*)))
      (concat "http://iopscience.iop.org" tail "/pdf" (replace-regexp-in-string "/" "_" tail) ".pdf"))))

(add-to-list 'pdf-url-functions 'iop-pdf-url)

(defun jstor-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.jstor.org" *doi-utils-redirect*)
    (concat (replace-regexp-in-string "/stable/" "/stable/pdfplus/" *doi-utils-redirect*) ".pdf")))

(add-to-list 'pdf-url-functions 'jstor-pdf-url)

(defun aip-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://scitation.aip.org" *doi-utils-redirect*)
    ;; get stuff after content
    (let (p1 p2 s p3)
      (setq p2 (replace-regexp-in-string "^http://scitation.aip.org/" "" *doi-utils-redirect*))
      (setq s (split-string p2 "/"))
      (setq p1 (mapconcat 'identity (-remove-at-indices '(0 6) s) "/"))
      (setq p3 (concat "/" (nth 0 s) (nth 1 s) "/" (nth 2 s) "/" (nth 3 s)))
      (format "http://scitation.aip.org/deliver/fulltext/%s.pdf?itemId=/%s&mimeType=pdf&containerItemId=%s"
	      p1 p2 p3))))

(add-to-list 'pdf-url-functions 'aip-pdf-url)

(defun science-direct-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.sciencedirect.com" *doi-utils-redirect*)
    (doi-utils-get-science-direct-pdf-url *doi-utils-redirect*)
    *doi-utils-pdf-url*))

(add-to-list 'pdf-url-functions 'science-direct-pdf-url)

(defun tandfonline-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.tandfonline.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/\\|/full/" "/pdf/" *doi-utils-redirect*)))

(add-to-list 'pdf-url-functions 'tandfonline-pdf-url)

(defun ecs-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://jes.ecsdl.org" *doi-utils-redirect*)
    (replace-regexp-in-string "\.abstract$" ".full.pdf" *doi-utils-redirect*)))

(add-to-list 'pdf-url-functions 'ecs-pdf-url)

(defun rsc-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://pubs.rsc.org" *doi-utils-redirect*)
    (let ((url (downcase *doi-utils-redirect*)))
      (setq url (replace-regexp-in-string "articlelanding" "articlepdf" url))
      url)))

(add-to-list 'pdf-url-functions 'rsc-pdf-url)

(defun doi-utils-get-pdf-url (doi)
  "returns a url to a pdf for the doi if one can be
calculated. Loops through the functions in `pdf-url-functions'
until one is found"
  (interactive "sDOI: ")
  (doi-utils-get-redirect doi)
  
  (unless *doi-utils-redirect*
    (error "No redirect found for %s" doi))
  
  (catch 'pdf-url
    (dolist (func pdf-url-functions pdf-url)
      (let ((pdf-url (funcall func *doi-utils-redirect*)))
	(when pdf-url
	  (throw 'pdf-url pdf-url))))))
	   
;; (defun doi-utils-get-pdf-url-ori (doi)
;;   "returns a url to a pdf for the doi if one can be
;; calculated. The calculated urls are basically worked out by hand
;; for each url base. These tend to be publisher specific, and
;; sometimes are journal specific."
;;   (doi-utils-get-redirect doi)
  
;;   (unless *doi-utils-redirect*
;;     (error "No redirect found for %s" doi))
  
;;   (cond
;;    ;; APS journals
;;    ((string-match "^http://journals.aps.org" *doi-utils-redirect*)
;;     (replace-regexp-in-string "/abstract/" "/pdf/" *doi-utils-redirect*))

;;    ;; Science
;;    ((string-match "^http://www.sciencemag.org" *doi-utils-redirect*)
;;     (concat *doi-utils-redirect* ".full.pdf"))

;;    ;; Nature and related journals
;;    ((string-match "^http://www.nature.com" *doi-utils-redirect*)
;;     (let ((result *doi-utils-redirect*))
;;       (setq result (replace-regexp-in-string "/full/" "/pdf/" result))
;;       (replace-regexp-in-string "\.html$" "\.pdf" result)))
   
;;    ;; Wiley - it is aguess this works for all of them
;;    ((string-match "^http://onlinelibrary.wiley.com" *doi-utils-redirect*)
;;     (replace-regexp-in-string "/abstract" "/pdf" *doi-utils-redirect*))

;;    ;; Springer
;;    ;; http://link.springer.com/article/10.1007%2Fs11244-012-9808-0
;;    ;; http://link.springer.com/content/pdf/10.1007%2Fs11244-012-9808-0.pdf
;;    ((string-match "^http://link.springer.com" *doi-utils-redirect*)
;;     (replace-regexp-in-string "/article/" "/content/pdf/" (concat *doi-utils-redirect* ".pdf")))

;;    ;; ACS journals
;;    ((string-match "^http://pubs.acs.org" *doi-utils-redirect*)
;;     (replace-regexp-in-string "/abs/" "/pdf/" *doi-utils-redirect*))

;;    ;; iop
;;    ;; http://iopscience.iop.org/0953-8984/21/39/395502/
;;    ;; http://iopscience.iop.org/0953-8984/21/39/395502/pdf/0953-8984_21_39_395502.pdf
;;    ((string-match "^http://iopscience.iop.org" *doi-utils-redirect*)
;;     (let ((tail (replace-regexp-in-string "^http://iopscience.iop.org" "" *doi-utils-redirect*)))
;;       (concat "http://iopscience.iop.org" tail "/pdf" (replace-regexp-in-string "/" "_" tail) ".pdf")))

;;    ;; jstor
;;    ;; http://www.jstor.org/stable/2245477
;;    ;; http://www.jstor.org/stable/pdfplus/2245477.pdf
;;    ((string-match "^http://www.jstor.org" *doi-utils-redirect*)
;;     (concat (replace-regexp-in-string "/stable/" "/stable/pdfplus/" *doi-utils-redirect*) ".pdf"))

;;    ;; AIP - this feels like major hackery but, these urls are complicated
;;    ((string-match "^http://scitation.aip.org" *doi-utils-redirect*)
;;     ;; get stuff after content
;;     (let (p1 p2 s p3)
;;       (setq p2 (replace-regexp-in-string "^http://scitation.aip.org/" "" *doi-utils-redirect*))
;;       (setq s (split-string p2 "/"))
;;       (setq p1 (mapconcat 'identity (-remove-at-indices '(0 6) s) "/"))
;;       (setq p3 (concat "/" (nth 0 s) (nth 1 s) "/" (nth 2 s) "/" (nth 3 s)))
;;       (format "http://scitation.aip.org/deliver/fulltext/%s.pdf?itemId=/%s&mimeType=pdf&containerItemId=%s"
;; 	      p1 p2 p3)))

;;    ;; Science direct - must retrieve from the html page
;;    ((string-match "^http://www.sciencedirect.com" *doi-utils-redirect*)
;;     (doi-utils-get-science-direct-pdf-url *doi-utils-redirect*)
;;     *doi-utils-pdf-url*)

;;    ;; tandfonline
;;    ;; http://www.tandfonline.com/doi/abs/10.1080/08927022.2013.844898
;;    ;; http://www.tandfonline.com/doi/full/10.1080/08927022.2013.844898
;;    ;; http://www.tandfonline.com/doi/pdf/10.1080/08927022.2013.844898
;;    ((string-match "^http://www.tandfonline.com" *doi-utils-redirect*)
;;     (replace-regexp-in-string "/abs/\\|/full/" "/pdf/" *doi-utils-redirect*))

;;    ;; ECS
;;    ;; http://jes.ecsdl.org/content/158/10/A1079.abstract
;;    ;; http://jes.ecsdl.org/content/158/10/A1079.full.pdf+html
;;    ((string-match "^http://jes.ecsdl.org" *doi-utils-redirect*)
;;     (replace-regexp-in-string "\.abstract$" ".full.pdf" *doi-utils-redirect*))

;;    ;; RSC
;;    ;; http://pubs.rsc.org/en/Content/ArticleLanding/2014/RA/c3ra47097k
;;    ;; http://pubs.rsc.org/en/content/articlepdf/2014/ra/c3ra47097k
;;    ((string-match "^http://pubs.rsc.org" *doi-utils-redirect*)
;;     (let ((url (downcase *doi-utils-redirect*)))
;;       (setq url (replace-regexp-in-string "articlelanding" "articlepdf" url))
;;       url))
   
;;    (t
;;     (message-box "redirect %s is unknown. Please report this message to jkitchin@andrew.cmu.edu." *doi-utils-redirect*)
;;     nil)))
   

(defun doi-utils-get-bibtex-entry-pdf ()
  "download pdf for entry at point if the pdf does not already
exist locally. The entry must have a doi. The pdf will be saved
to `org-ref-pdf-directory', by the name %s.pdf where %s is the
bibtex label. Files will not be overwritten. The pdf will be
checked to make sure it is a pdf, and not some html failure
page. you must have permission to access the pdf. We open the pdf
at the end."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry) 
    (let (;; get doi, removing http://dx.doi.org/ if it is there.
	  (doi (replace-regexp-in-string
		"http://dx.doi.org/" ""
		(bibtex-autokey-get-field "doi")))	       
	  (key)
	  (pdf-url)
	  (pdf-file)
	  (content))
      ;; get the key and build pdf filename.
      (re-search-forward bibtex-entry-maybe-empty-head)
      (setq key (match-string bibtex-key-in-head))
      (setq pdf-file (concat org-ref-pdf-directory key ".pdf"))

      ;; now get file if needed.
      (when (and doi (not (file-exists-p pdf-file)))
	(setq pdf-url (doi-utils-get-pdf-url doi))
	(url-copy-file pdf-url pdf-file)
	;; now check if we got a pdf
	(with-temp-buffer
	  (insert-file-contents pdf-file)
	  ;; PDFS start with %PDF-1.x as the first few characters.
	  (if (not (string= (buffer-substring 1 6) "%PDF-"))
	      (progn
		(message "%s" (buffer-string))
		(delete-file pdf-file))
	    (message "%s saved" pdf-file)))
	
      (when (file-exists-p pdf-file)
	(org-open-file pdf-file))
      pdf-file))))
