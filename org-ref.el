
;;; org-ref.el --- setup bibliography, cite, ref and label org-mode links.

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
;; Lisp code to setup bibliography cite, ref and label org-mode links.
;; also sets up reftex for org-mode. The links are clickable and do
;; things that are useful. You should really read org-ref.org for details.

(require 'reftex-cite)

(defgroup org-ref nil
  "customization group for org-ref")

(defcustom org-ref-bibliography-notes
  nil
  "filename to where you will put all your notes about an entry in
  the default bibliography."
  :type 'list
  :group 'org-ref)

(defcustom org-ref-default-bibliography
  nil
  "list of bibtex files to search for. You should use full-paths for each file."
  :group 'org-ref)

(defcustom org-ref-pdf-directory
  nil
  "directory where pdfs are stored by key. put a trailing / in"
  :group 'org-ref)

(defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
	 (global-auto-revert-mode t)
         (reftex-parse-all))
    (make-local-variable 'reftex-cite-format)
    (setq reftex-cite-format 'org)
    (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-cite-link))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)


;; We make two formats. The default inserts a cite link. the "a"
;; format is for (a)ppending citations on an existing citation
(eval-after-load 'reftex-vars
  '(progn
      (add-to-list 'reftex-cite-format-builtin
                   '(org "Org-mode citation"
                         ((?\C-m . "cite:%l")
			  (?a . ",%l"))))))

(defun org-ref-strip-string (string)
  "strip leading and trailing whitespace from the string"
  (interactive)
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" string)))

(defun org-ref-split-and-strip-string (string)
  "split key-string and strip keys. Assumes the key-string is comma delimited"
  (mapcar 'org-ref-strip-string (split-string string ",")))

(org-add-link-type "bibliography"
		   ;; this code is run on clicking. The bibliography
		   ;; may contain multiple files. this code finds the
		   ;; one you clicked on and opens it.
		   (lambda (link-string)	
		       ;; get link-string boundaries
		       ;; we have to go to the beginning of the line, and then search forward
		       
		     (let* ((bibfile)
			    ;; object is the link you clicked on
			    (object (org-element-context))
 
			    (link-string-beginning) 
			    (link-string-end))

		     (save-excursion
		       (goto-char (org-element-property :begin object))
		       (search-forward link-string nil nil 1)
		       (setq link-string-beginning (match-beginning 0))
		       (setq link-string-end (match-end 0)))

		       ;; We set the reftex-default-bibliography
		       ;; here. it should be a local variable only in
		       ;; the current buffer. We need this for using
		       ;; reftex to do citations.
		       (set (make-local-variable 'reftex-default-bibliography) 
			    (split-string (org-element-property :path object) ","))

		       ;; now if we have comma separated bibliographies
		       ;; we find the one clicked on. we want to
		       ;; search forward to next comma from point
		       (save-excursion
			 (if (search-forward "," link-string-end 1 1)
			     (setq key-end (- (match-end 0) 1)) ; we found a match
			   (setq key-end (point)))) ; no comma found so take the point
		       ;; and backward to previous comma from point
		       (save-excursion
			 (if (search-backward "," link-string-beginning 1 1)
			     (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
			   (setq key-beginning (point)))) ; no match found
		       ;; save the key we clicked on.
		       (setq bibfile (org-ref-strip-string (buffer-substring key-beginning key-end)))
		       (message "found %s for bibliography" bibfile)
		       (find-file bibfile))) ; open file on click

		     ;; formatting code
		   (lambda (keyword desc format)
		     (cond
		      ((eq format 'html) (format "")); no output for html
		      ((eq format 'latex)
			 ;; write out the latex bibliography command
		       (format "\\bibliography{%s}" (replace-regexp-in-string  "\\.bib" "" keyword))))))

(org-add-link-type "bibliographystyle"
		   (lambda (arg) (message "Nothing implemented for clicking here."))
		   (lambda (keyword desc format)
		     (cond
		      ((eq format 'latex)
		       ;; write out the latex bibliography command
		       (format "\\bibliographystyle{%s}" keyword)))))

(defun org-ref-list-of-figures (&optional arg)
  "Generate buffer with list of figures in them"
  (interactive)
  (let* ((c-b (buffer-name))
	 (counter 0)
	 (list-of-figures 
	  (org-element-map (org-element-parse-buffer) 'link
	    (lambda (link) 
	      "create a link for to the figure"
	      (when 
		  (and (string= (org-element-property :type link) "file")
		       (string-match-p  
			"[^.]*\\.\\(png\\|jpg\\|eps\\|pdf\\)$"
			(org-element-property :path link)))                   
		(incf counter)
		
		(let* ((start (org-element-property :begin link))
		       (parent (car (cdr (org-element-property :parent link))))
		       (caption (caaar (plist-get parent :caption)))
		       (name (plist-get parent :name)))
		  (if caption 
		      (format 
		       "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s))][figure %s: %s]] %s\n" 
		       c-b start counter (or name "") caption)
		    (format 
		     "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s))][figure %s: %s]]\n" 
		     c-b start counter (or name "")))))))))
    (switch-to-buffer "*List of Figures*")
    (org-mode)
    (erase-buffer)
    (insert (mapconcat 'identity list-of-figures ""))
    (setq buffer-read-only t)
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))))

(org-add-link-type 
 "list-of-figures"
 'org-ref-list-of-figures ; on click
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\listoffigures")))))

(defun org-ref-list-of-tables (&optional arg)
  "Generate a buffer with a list of tables"
  (interactive)
  (let* ((c-b (buffer-name))
	 (counter 0)
	 (list-of-tables 
	  (org-element-map (org-element-parse-buffer 'element) 'table
	    (lambda (table) 
	      "create a link for to the table"
	      (incf counter)
	      (let ((start (org-element-property :begin table))
		    (name  (org-element-property :name table))
		    (caption (caaar (org-element-property :caption table))))
		(if caption 
		    (format 
		     "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s))][table %s: %s]] %s\n" 
		     c-b start counter (or name "") caption)
		  (format 
		   "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s))][table %s: %s]]\n" 
		   c-b start counter (or name ""))))))))
    (switch-to-buffer "*List of Tables*")
    (org-mode)
    (erase-buffer)
    (insert (mapconcat 'identity list-of-tables ""))
    (setq buffer-read-only t)
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))))

(org-add-link-type 
 "list-of-tables"
 'org-ref-list-of-tables
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\listoftables")))))

(org-add-link-type
 "label"
 (lambda (label)
   "on clicking count the number of label tags used in the buffer. A number greater than one means multiple labels!"
   (message (format "%s occurences"
		    (+ (count-matches (format "label:%s\\b" label) (point-min) (point-max) t)
		       (count-matches (format "\\label{%s}\\b" label) (point-min) (point-max) t)
                       ;; this is the org-format #+label:
		       (count-matches (format "#\\+label:%s\\b" label) (point-min) (point-max) t)))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<label>%s</label>)" path))
    ((eq format 'latex)
     (format "\\label{%s}" keyword)))))

(defun org-label-store-link ()
  "store a link to a label. The output will be a ref to that label"
  ;; First we have to make sure we are on a label link. 
  (let* ((object (org-element-context)))
    (when (and (equal (org-element-type object) 'link) 
               (equal (org-element-property :type object) "label"))
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" (org-element-property :path object))))

    ;; Store link on table
    (when (equal (org-element-type object) 'table)
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" (org-element-property :name object))))

;; it turns out this does not work. you can already store a link to a heading with a CUSTOM_ID
    ;; store link on heading with custom_id
;    (when (and (equal (org-element-type object) 'headline)
;	       (org-entry-get (point) "CUSTOM_ID"))
;      (org-store-link-props
;       :type "ref"
;       :link (concat "ref:" (org-entry-get (point) "CUSTOM_ID"))))

    ;; and to #+label: lines
    (when (and (equal (org-element-type object) 'paragraph)
	       (org-element-property :name object))
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" (org-element-property :name object))))
))

(add-hook 'org-store-link-functions 'org-label-store-link)

(org-add-link-type
 "ref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (org-mark-ring-push)
   ;; next search from beginning of the buffer

   (unless
       (or
	;; our label links
	(progn 
	  (goto-char (point-min))
	  (re-search-forward (format "label:%s" label) nil t))

	;; a latex label
	(progn
	  (goto-char (point-min))
	  (re-search-forward (format "\\label{%s}" label) nil t))

	;; #+label: name  org-definition
	(progn
	  (goto-char (point-min))
	  (re-search-forward (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t))
	
	;; org tblname
	(progn
	  (goto-char (point-min))
	  (re-search-forward (format "^#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t))

;; Commented out because these ref links do not actually translate correctly in LaTeX.
;; you need [[#label]] links.
	;; CUSTOM_ID
;	(progn
;	  (goto-char (point-min))
;	  (re-search-forward (format ":CUSTOM_ID:\s-*\\(%s\\)" label) nil t))
	)
     ;; we did not find anything, so go back to where we came
     (org-mark-ring-goto)
     (error "%s not found" label))
   (message "go back with (org-mark-ring-goto) `C-c &`"))
 ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<ref>%s</ref>)" path))
    ((eq format 'latex)
     (format "\\ref{%s}" keyword)))))

(defun org-ref-get-custom-ids ()
 "return a list of custom_id properties in the buffer"
 (interactive)
 (let ((results '()) custom_id)
   (org-map-entries 
    (lambda () 
      (let ((custom_id (org-entry-get (point) "CUSTOM_ID")))
	(when (not (null custom_id))
	  (setq results (append results (list custom_id)))))))
results))

(defun org-ref-get-latex-labels ()
(interactive) 
(save-excursion
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "\\\\label{\\([a-zA-z0-9:-]*\\)}" (point-max) t)
	(add-to-list 'matches (match-string-no-properties 1) t))
matches)))

(defun org-ref-get-tblnames ()
  (interactive)
  (org-element-map (org-element-parse-buffer 'element) 'table
    (lambda (table) 
      (org-element-property :name table))))

(defun org-ref-get-labels ()
  "returns a list of labels in the buffer that you can make a ref link to. this is used to auto-complete ref links."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "label:\\([a-zA-z0-9:-]*\\)" (point-max) t)
	(add-to-list 'matches (match-string-no-properties 1) t))
      (append matches (org-ref-get-latex-labels) (org-ref-get-tblnames) (org-ref-get-custom-ids)))))

(defun org-ref-complete-link (&optional arg)
  "Completion function for ref links"
  (let ((label))
    (setq label (completing-read "label: " (org-ref-get-labels)))
    (format "ref:%s" label)))

(defun org-ref-insert-ref-link ()
 (interactive)
 (insert (org-ref-complete-link)))

(org-add-link-type
 "eqref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (org-mark-ring-push)
   ;; next search from beginning of the buffer
   (goto-char (point-min))
   (unless
       (or
	;; search forward for the first match
	;; our label links
	(re-search-forward (format "label:%s" label) nil t)
	;; a latex label
	(re-search-forward (format "\\label{%s}" label) nil t)
	;; #+label: name  org-definition
	(re-search-forward (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t))
     (org-mark-ring-goto)
     (error "%s not found" label))
   (message "go back with (org-mark-ring-goto) `C-c &`"))
 ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<eqref>%s</eqref>)" path))
    ((eq format 'latex)
     (format "\\eqref{%s}" keyword)))))

(defun org-ref-get-bibtex-key-under-cursor ()
  "returns key under the bibtex cursor. We search forward from
point to get a comma, or the end of the link, and then backwards
to get a comma, or the beginning of the link. that delimits the
keyword we clicked on. We also strip the text properties."
  (interactive)
  (let* ((object (org-element-context))	 
	 (link-string (org-element-property :path object)))    
    (when (and (equal (org-element-type object) 'link) 
               (equal (org-element-property :type object) "cite"))
      ;; we need the link path start and end
      (save-excursion
	(goto-char (org-element-property :begin object))
	(search-forward link-string nil nil 1)
	(setq link-string-beginning (match-beginning 0))
	(setq link-string-end (match-end 0)))

      ;; The key is the text between commas, or the link boundaries
      (save-excursion
	(if (search-forward "," link-string-end t 1)
	    (setq key-end (- (match-end 0) 1)) ; we found a match
	  (setq key-end link-string-end))) ; no comma found so take the end
      ;; and backward to previous comma from point which defines the start character
      (save-excursion
	(if (search-backward "," link-string-beginning 1 1)
	    (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
	  (setq key-beginning link-string-beginning))) ; no match found
      ;; save the key we clicked on.
      (setq bibtex-key (org-ref-strip-string (buffer-substring key-beginning key-end)))
      (set-text-properties 0 (length bibtex-key) nil bibtex-key)
      (message "you selected %s" bibtex-key)
      bibtex-key
      )))

(defun org-ref-find-bibliography ()
  "find the bibliography in the buffer.
This function sets and returns cite-bibliography-files, which is a list of files
either from bibliography:f1.bib,f2.bib
\bibliography{f1,f2}
internal bibliographies

falling back to what the user has set in org-ref-default-bibliography
"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;;  look for a bibliography link
    (re-search-forward "bibliography:\\([^\]\|\n]+\\)" nil t)
    (if (match-string 1) ; we found a link
	(progn
	  (setq org-ref-bibliography-files
		(mapcar 'org-ref-strip-string (split-string (match-string 1) ",")))
	  (message "org-ref-bibliography-files = %s from %s" org-ref-bibliography-files (match-string 1)))
      (progn ;we did not find a bibliography link. now look for \bibliography
	(message "no bibliography link found")
	(goto-char (point-min))
	(re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
	(if (match-string 1) ; we found a link
	    ;; split, and add .bib to each file
	    (setq org-ref-bibliography-files
		  (mapcar (lambda (x) (concat x ".bib"))
			  (mapcar 'org-ref-strip-string 
				  (split-string (match-string 1) ","))))
	 ; we did not find a raw latex bibliography. use defaults
	  (setq org-ref-bibliography-files org-ref-default-bibliography)
	  (message "org-ref-bibliography-files = %s" org-ref-bibliography-files)))))
  (message "org-ref-bibliography-files = %s" org-ref-bibliography-files)
  ;; set reftex-default-bibliography so we can search
  (set (make-local-variable 'reftex-default-bibliography) org-ref-bibliography-files)
  org-ref-bibliography-files)

(defun org-ref-key-in-file-p (key filename)
  "determine if the key is in the file"
  (with-temp-buffer
    (insert-file-contents filename)
    (bibtex-search-entry key)))

(defun org-ref-get-bibtex-key-and-file ()
  "returns the bibtex key and file that it is in under point"
 (interactive)

 (let ((org-ref-bibliography-files (org-ref-find-bibliography))
       key file)
   (setq key (org-ref-get-bibtex-key-under-cursor))
   (setq file     (loop for file in org-ref-bibliography-files do
			    (if (org-ref-key-in-file-p key file) 
				(return file))))
   (message  "you found %s in %s" key file)
   (cons key file)))

(defun org-ref-get-menu-options ()
  "returns a dynamically determined string of options for the citation under point.

we check to see if there is pdf, and if the key actually exists in the bibliography"
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (cb (current-buffer))
         (pdf-file (format (concat org-ref-pdf-directory "%s.pdf") key))
         (bibfile (cdr results))
	 m1 m2 m3 m4 m5 menu-string)
    (setq m1 (if bibfile		 
		 "(o)pen"
	       "(No key found)"))

    (setq m3 (if (file-exists-p pdf-file)
		 "(p)df"
		     "(No pdf found)"))

    (setq m4 "(u)rl")
    (setq m5 "(n)otes")
    (setq m2 (if bibfile
		 (progn
		   (let ((cb (current-buffer)) citation)
		     (setq citation (progn
				      (set-buffer (find-file-noselect bibfile))
				      (bibtex-search-entry key)  
				      (org-ref-bib-citation)))
		     (set-buffer cb)
		     citation))
	       "no key found"))

    (setq menu-string (mapconcat 'identity (list m2 "\n" m1 m3 m4 m5 "(q)uit") "  "))
    (message "%s" menu-string)
    menu-string))

(defun org-ref-open-pdf-at-point ()
  "open the pdf for bibtex key under point if it exists"
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
         (pdf-file (format (concat org-ref-pdf-directory "%s.pdf") key)))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
(message "no pdf found for %s" key))))


(defun org-ref-open-url-at-point ()
  "open the url for bibtex key under point."
  (interactive)
  (let* ((cb (current-buffer))
	 (results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (bibfile (cdr results)))
    (save-excursion
      (set-buffer (find-file-noselect bibfile))
      (bibtex-search-entry key)
      ;; I like this better than bibtex-url which does not always find
      ;; the urls
      (catch 'done
	(let ((url (bibtex-autokey-get-field "url")))
	  (when  url
	    (browse-url url)
	    (throw 'done nil)))

	(let ((doi (bibtex-autokey-get-field "doi")))
	  (when doi
	    (if (string-match "^http" doi)
		(browse-url doi)
	      (browse-url (format "http://dx.doi.org/%s" doi)))
	    (throw 'done nil)))))
    (set-buffer cb)))


(defun org-ref-open-notes-at-point ()
  "open the notes for bibtex key under point."
  (interactive)
  (let* ((cb (current-buffer))
	 (results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (bibfile (cdr results)))
    (save-excursion
	   (find-file bibfile)
	   (bibtex-search-entry key)
	   (org-ref-open-bibtex-notes))))

(defun org-ref-cite-onclick-minibuffer-menu (&optional link-string)
  "use a minibuffer to select options for the citation under point.

you select your option with a single key press."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (cb (current-buffer))
         (pdf-file (format (concat org-ref-pdf-directory "%s.pdf") key))
         (bibfile (cdr results))
	 (choice (read-char (org-ref-get-menu-options) )))

    (cond
     ;; open
     ((= choice ?o)
      (find-file bibfile)
       (bibtex-search-entry key))

     ;; cite
     ((= choice ?c)
      (let ((cb (current-buffer)))	
	(message "%s" (progn
			(set-buffer (find-file-noselect bibfile))
			(bibtex-search-entry key)  
			(org-ref-bib-citation)))
	(set-buffer cb)))

     ;; quit
     ((or 
      (= choice ?q) ; q
      (= choice ?\ )) ; space
      ;; this clears the minibuffer
      (message ""))

     ;; pdf
     ((= choice ?p)
      (org-ref-open-pdf-at-point))

     ;; notes
     ((= choice ?n)
      (org-ref-open-notes-at-point))

     ;; url
     ((= choice ?u)
      (org-ref-open-url-at-point))

     ;; anything else we just quit.
     (t (message ""))))
    )

(org-add-link-type
 "cite"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (concat "\\cite{"
	     (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",")
	     "}")))))

(org-add-link-type
 "citealp"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<citealp>%s</citealp>)" path))
    ((eq format 'latex)
     (concat "\\citealp{"
	     (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",")
	     "}")))))

(org-add-link-type
 "citet"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citet{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "citet*"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citet*{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

;; TODO these links do not support options [see][]
(org-add-link-type
 "citep"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citep{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "citep*"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citep*{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "citeauthor"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeauthor{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "citeauthor*"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeauthor*{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "citeyear"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeyear{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "nocite"
 'org-ref-cite-onclick-minibuffer-menu
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\nocite{" (mapconcat (lambda (key) key) (org-ref-split-and-strip-string keyword) ",") "}")))))

(org-add-link-type
 "citetext"
 nil ;; clicking does not make sense
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citetext{" path "}")))))

(defun org-ref-insert-cite-link ()
  "Insert a citation link using reftex. If you are on a link, it
appends to the end of the link, otherwise, a new link is
inserted"
  (interactive)
  (let* ((object (org-element-context))
	 (link-string-beginning (org-element-property :begin object))
	 (link-string-end (org-element-property :end object))
	 (path (org-element-property :path object)))    
    
    (cond
     ;; case where we are in a link
     ((and (equal (org-element-type object) 'link) 
	   (equal (org-element-property :type object) "cite"))
      (goto-char link-string-end)
      ;; sometimes there are spaces at the end of the link
      ;; this code moves point pack until no spaces are there
      (while (looking-back " ") (backward-char))  
      (insert (concat "," (mapconcat 'identity (reftex-citation t ?a) ","))))

     ;; We are next to a link, and we want to append
     ((save-excursion 
	(backward-char)
	(and (equal (org-element-type (org-element-context)) 'link) 
	     (equal (org-element-property :type (org-element-context)) "cite")))
      (while (looking-back " ") (backward-char))  
      (insert (concat "," (mapconcat 'identity (reftex-citation t ?a) ","))))

     ;; insert fresh link
     (t (insert (concat "cite:" (mapconcat 'identity (reftex-citation t) ",")))))))

(defun org-cite-complete-link (&optional arg)
  "Completion function for cite links"
  (format "cite:%s" 
	  (completing-read 
	   "bibtex key: " 
	   (let ((bibtex-files (org-ref-find-bibliography)))
	     (bibtex-global-key-alist)))))

(defun org-ref-insert-cite-with-completion ()
  "Insert a cite link with completion"
  (interactive)
  (insert (org-cite-complete-link)))

(defun org-ref-bib-citation ()
  "from a bibtex entry, create and return a simple citation string."
  (interactive)
  (if (eq major-mode 'bibtex-mode)
      (progn
        (bibtex-beginning-of-entry)
        (let* ((cb (current-buffer))
               (bibtex-expand-strings t)
               (entry (bibtex-parse-entry t))
               (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
               (year  (reftex-get-bib-field "year" entry))
               (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
               (key (reftex-get-bib-field "=key=" entry))
               (journal (reftex-get-bib-field "journal" entry))
               (volume (reftex-get-bib-field "volume" entry))
               (pages (reftex-get-bib-field "pages" entry))
               (doi (reftex-get-bib-field "doi" entry))
               (url (reftex-get-bib-field "url" entry))
               )
	  ;;authors, "title", Journal, vol(iss):pages (year).
            (format "%s, \"%s\", %s, %s:%s (%s)"
		    author title journal  volume pages year)))))

(defun org-ref-bib-open-bibtex-pdf ()
  "open pdf for a bibtex entry, if it exists. assumes point is in
the entry of interest in the bibfile. but does not check that."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "=key=" entry))
           (pdf (format (concat org-ref-pdf-directory "%s.pdf") key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
          (org-open-link-from-string (format "[[file:%s]]" pdf))
        (ding)))))

(defun org-ref-open-bibtex-notes ()
  "from a bibtex entry, open the notes if they exist, and create a heading if they do not.

I never did figure out how to use reftex to make this happen
non-interactively. the reftex-format-citation function did not
work perfectly; there were carriage returns in the strings, and
it did not put the key where it needed to be. so, below I replace
the carriage returns and extra spaces with a single space and
construct the heading by hand."
  (interactive)
  (if (eq major-mode 'bibtex-mode)
      (progn
        (bibtex-beginning-of-entry)
        (let* ((cb (current-buffer))
               (bibtex-expand-strings t)
               (entry (bibtex-parse-entry t))
               (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
               (year  (reftex-get-bib-field "year" entry))
               (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
               (key (reftex-get-bib-field "=key=" entry))
               (journal (reftex-get-bib-field "journal" entry))
               (volume (reftex-get-bib-field "volume" entry))
               (pages (reftex-get-bib-field "pages" entry))
               (doi (reftex-get-bib-field "doi" entry))
               (url (reftex-get-bib-field "url" entry))
               )
	  (save-buffer)

	  ;; save key to clipboard to make saving pdf later easier by pasting.
	  (with-temp-buffer
	    (insert key)
	    (kill-ring-save (point-min) (point-max)))

          ;; now look for entry in the notes file
          (if  org-ref-bibliography-notes
	      (find-file org-ref-bibliography-notes)
	    (error "org-ref-bib-bibliography-notes is not set to anything"))

          (goto-char (point-min))
          ;; put new entry in notes if we don't find it.
          (unless (re-search-forward (format ":Custom_ID: %s$" key) nil 'end)
            (insert (format "\n** TODO %s - %s" year title))
            (insert (format"
 :PROPERTIES:
  :Custom_ID: %s
  :AUTHOR: %s
  :JOURNAL: %s
  :YEAR: %s
  :VOLUME: %s
  :PAGES: %s
  :DOI: %s
  :URL: %s
 :END:
[[cite:%s]] [[file:%s/%s.pdf][pdf]]\n\n"
key author journal year volume pages doi url key org-ref-pdf-directory key))
(save-buffer))))))

(defun org-ref-open-in-browser ()
  "Open the bibtex entry at point in a browser using the url field or doi field"
(interactive)
(save-excursion
  (bibtex-beginning-of-entry)
  (catch 'done
    (let ((url (bibtex-autokey-get-field "url")))
      (when  url
        (browse-url url)
        (throw 'done nil)))

    (let ((doi (bibtex-autokey-get-field "doi")))
      (when doi
        (if (string-match "^http" doi)
            (browse-url doi)
          (browse-url (format "http://dx.doi.org/%s" doi)))
        (throw 'done nil)))
    (message "No url or doi found"))))

(defun org-ref-upload-bibtex-entry-to-citeulike ()
  "with point in  a bibtex entry get bibtex string and submit to citeulike.

Relies on the python script /upload_bibtex_citeulike.py being in the user directory."
  (interactive)
  (message "uploading to citeulike")
  (save-restriction
    (bibtex-narrow-to-entry)
    (let ((startpos (point-min))
          (endpos (point-max))
          (bibtex-string (buffer-string))
          (script (concat "python " starter-kit-dir "/upload_bibtex_citeulike.py&")))
      (with-temp-buffer (insert bibtex-string)
                        (shell-command-on-region (point-min) (point-max) script t nil nil t)))))

(global-set-key [f10] 'org-ref-open-bibtex-notes)
(global-set-key [f11] 'org-ref-open-bibtex-pdf)
(global-set-key [f12] 'org-ref-open-in-browser)

(defun org-ref-build-full-bibliography ()
  "build pdf of all bibtex entries, and open it."
  (interactive)
  (let* ((bibfile (file-name-nondirectory (buffer-file-name)))
	(bib-base (file-name-sans-extension bibfile))
	(texfile (concat bib-base ".tex"))
	(pdffile (concat bib-base ".pdf")))
    (find-file texfile)
    (erase-buffer)
    (insert (format "\\documentclass[12pt]{article}
\\usepackage[version=3]{mhchem}
\\usepackage{url}
\\usepackage[numbers]{natbib}
\\usepackage[colorlinks=true, linkcolor=blue, urlcolor=blue, pdfstartview=FitH]{hyperref}
\\usepackage{doi}
\\begin{document}
\\nocite{*}
\\bibliographystyle{unsrtnat}
\\bibliography{%s}
\\end{document}" bib-base))
    (save-buffer)
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "bibtex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (kill-buffer texfile)
    (org-open-file pdffile)
    )) 

(defun org-ref-extract-bibtex-entries ()
  "extract the bibtex entries referred to by cite links in the current buffer into a src block at the bottom of the current buffer.

If no bibliography is in the buffer the `reftex-default-bibliography' is used."
  (interactive)
  (let* ((tempname (make-temp-file "extract-bib"))
         (contents (buffer-string))
         (cb (current-buffer))
	 basename texfile bibfile results)
    
    ;; open tempfile and insert org-buffer contents
    (find-file tempname)
    (insert contents)
    (setq basename (file-name-sans-extension 
		    (file-name-nondirectory buffer-file-name))
	  texfile (concat tempname ".tex")
	  bibfile (concat tempname ".bib"))
    
    ;; see if we have a bibliography, and insert the default one if not.
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^bibliography:" (point-max) 'end)
	(insert (format "\nbibliography:%s" 
			(mapconcat 'identity reftex-default-bibliography ",")))))
    (save-buffer)

    ;; get a latex file and extract the references
    (org-latex-export-to-latex)
    (find-file texfile)
    (reftex-parse-all)
    (reftex-create-bibtex-file bibfile)
    (save-buffer)
    ;; save results of the references
    (setq results (buffer-string))

    ;; kill buffers. these are named by basename, not full path
    (kill-buffer (concat basename ".bib"))
    (kill-buffer (concat basename ".tex"))
    (kill-buffer basename)

    (delete-file bibfile)
    (delete-file texfile)
    (delete-file tempname)

    ;; Now back to the original org buffer and insert the results
    (switch-to-buffer cb)
    (save-excursion
      (goto-char (point-max))
      (insert (format "

** Bibtex entries

#+BEGIN_SRC: :tangle %s
,%s
#+END_SRC" bibfile results)))))

(require 'cl)

(defun index (substring list)
  "return the index of string in a list of strings"
  (let ((i 0)
	(found nil))
    (dolist (arg list i)
      (if (string-match substring arg)
	  (progn 
	    (setq found t)
	    (return i)))
      (setq i (+ i 1)))
    ;; return counter if found, otherwise return nil
    (if found i nil)))


(defun org-ref-bib-find-bad-citations ()
  "Create a list of citation keys in an org-file that do not have a bibtex entry in the known bibtex files.

Makes a new buffer with clickable links."
  (interactive)
  ;; generate the list of bibtex-keys and cited keys
  (let* ((bibtex-files (org-ref-find-bibliography))
	 (bibtex-keys (mapcar (lambda (x) (car x)) (bibtex-global-key-alist)))
	 (bad-citations '()))

    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)       
	(let ((plist (nth 1 link)))			     
	  (when (equal (plist-get plist ':type) "cite")
	    (dolist (key (org-ref-split-and-strip-string (plist-get plist ':path)) )
	      (when (not (index key bibtex-keys))
		(setq bad-citations (append bad-citations
					    `(,(format "%s [[elisp:(progn (switch-to-buffer-other-frame \"%s\")(goto-char %s))][not found here]]\n"
						       key (buffer-name)(plist-get plist ':begin)))))
		))))))

    (if identity bad-citations
      (progn
	(switch-to-buffer-other-window "*Missing citations*")
	(org-mode)
	(erase-buffer)
	(insert "* List of bad cite links\n")
	(insert (mapconcat 'identity bad-citations ""))
					;(setq buffer-read-only t)
	(use-local-map (copy-keymap org-mode-map))
	(local-set-key "q" #'(lambda () (interactive) (kill-buffer))))
      (message "No bad cite links found"))))

(defun org-ref-find-non-ascii-characters ()
  "finds non-ascii characters in the buffer. Useful for cleaning up bibtex files"
  (interactive)
  (occur "[^[:ascii:]]"))

(provide 'org-ref)
