;; Lisp code to setup bibliography  cite, ref and label org-mode links.
;; also sets up reftex for org-mode

;; [[bibliography]]
;; [[ref link]]
;; [[label link]]
;; [[cite links]]

(require 'reftex-cite)

(defgroup jorg-bib nil
  "customization group for jorg-bib")

(defcustom jorg-bib-bibliography-notes
  nil
  "filename to where you will put all your notes about an entry in
  the default bibliography."
  :type 'list
  :group 'jorg-bib)

(defcustom jorg-bib-default-bibliography
  nil
  "list of bibtex files to search for. You should use full-paths for each file."
  :group 'jorg-bib)

(defcustom jorg-bib-pdf-directory
  nil
  "directory where pdfs are stored by key. put a trailing / in"
  :group 'jorg-bib)


;; variables that control bibtex key format for auto-generation
;; I want firstauthor-year-title-words
;; this usually makes a legitimate filename to store pdfs under.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
	 (global-auto-revert-mode t)
         (reftex-parse-all))
    (make-local-variable 'reftex-cite-format)
    (setq reftex-cite-format 'org)
    (define-key org-mode-map (kbd "C-c ]") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(eval-after-load 'reftex-vars
  '(progn
      (add-to-list 'reftex-cite-format-builtin
                   '(org "Org-mode citation"
                         ((?\C-m . "cite:%l"))))))

(defun jorg-bib/upload-bibtex-entry-to-citeulike ()
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

(defun jorg-bib-open-bibtex-pdf ()
  "open pdf for a bibtex entry, if it exists. assumes point is in
the entry of interest in the bibfile. but does not check that."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "=key=" entry))
           (pdf (format (concat jorg-bib-pdf-directory "%s.pdf") key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
          (org-open-link-from-string (format "[[file:%s]]" pdf))
        (ding)))))

(defun jorg-bib-open-bibtex-notes ()
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
          (find-file jorg-bib-bibliography-notes)

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
key author journal year volume pages doi url key jorg-bib-pdf-directory key))
(save-buffer))))))


(defun jorg-bib-open-in-browser ()
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

(global-set-key [f10] 'jorg-bib-open-bibtex-notes)
(global-set-key [f11] 'jorg-bib-open-bibtex-pdf)
(global-set-key [f12] 'jorg-bib-open-in-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; bibliography and bibliography style code <<bibliography>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; link to hold a bibliography bibtex file(s). Mostly so I can click on
;; the link and open the file. It also sets the default bibliography
(org-add-link-type "bibliography"
		   ;; this code is run on clicking. The bibliography
		   ;; may contain multiple files. this code finds the
		   ;; one you clicked on and opens it.
		   (lambda (link-string)
		     (message (format "link-string = %s" link-string))
		     (save-excursion
		       ;; get link-string boundaries
		       ;; we have to go to the beginning of the line, and then search forward
		       (beginning-of-visual-line)
		       (search-forward link-string nil nil 1)
		       (setq link-string-beginning (match-beginning 0))
		       (setq link-string-end (match-end 0)))

		     ;; We set the reftex-default-bibliography
		     ;; here. it should be a local variable only in
		     ;; the current buffer. We need this for using
		     ;; reftex to do citations.
		     (set (make-local-variable 'reftex-default-bibliography) 
			  (split-string
			   (buffer-substring link-string-beginning link-string-end)
			   ","))
		     (message (format "link found %s "(buffer-substring link-string-beginning link-string-end)))
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
		     (setq bibfile (cite-strip-key (buffer-substring key-beginning key-end)))
		     (message (format "bibfile = %s" bibfile))		     
		     (find-file bibfile)) ; open file on click
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ref and label links <<ref link>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(org-add-link-type
 "ref"
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
	(re-search-forward (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t)
	;; org tblname
	(re-search-forward (format "^#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t))
     (org-mark-ring-goto)
     (error "%s not found" label))
   (message "go back with (org-mark-ring-goto) `C-c &`"))
 ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<ref>%s</ref>)" path))
    ((eq format 'latex)
     (format "\\ref{%s}" keyword)))))


(defun jorg-get-labels ()
  "returns a list of labels in the buffer. We only count label links. this is used to auto-complete ref links."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "label:\\([a-zA-z0-9:-]*\\)" (point-max) t)
	(add-to-list 'matches (match-string-no-properties 1) t))
      matches)))


(defun org-insert-ref-link (&optional arg)
  (interactive (list (completing-read "label: " (jorg-get-labels))))
  (insert (format "ref:%s" arg)))


;; <<label link>>

(org-add-link-type
 "label"
 (lambda (label)
   "on clicking count the number of label tags used in the buffer. A number greater than one means multiple labels!"
   (message (format "%s occurences"
		    (+ (count-matches (format "label:%s\\b" label) (point-min) (point-max) t)
		       (count-matches (format "\\label{%s}\\b" label) (point-min) (point-max) t)
		       (count-matches (format "#\\+label:%s\\b" label) (point-min) (point-max) t)))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<label>%s</label>)" path))
    ((eq format 'latex)
     (format "\\label{%s}" keyword)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; <<cite links>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of cite:  to make bibtex citations that are also clickable.
(defun cite-find-bibliography ()
  "find the bibliography in the buffer.
This function sets and returns cite-bibliography-files, which is a list of files
either from bibliography:f1.bib,f2.bib
\bibliography{f1,f2}
internal bibliographies

falling back to what the user has set in jorg-bib-default-bibliography
"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;;  look for a bibliography link
    (re-search-forward "bibliography:\\([^\]\|\n]+\\)" nil t)
    (if (match-string 1) ; we found a link
	(progn
	  (setq cite-bibliography-files
		(mapcar 'cite-strip-key (split-string (match-string 1) ",")))
	  (message "cite-bibliography-files = %s from %s" cite-bibliography-files (match-string 1)))
      (progn ;we did not find a bibliography link. now look for \bibliography
	(message "no bibliography link found")
	(goto-char (point-min))
	(re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
	(if (match-string 1) ; we found a link
	    ;; split, and add .bib to each file
	    (setq cite-bibliography-files
		  (mapcar (lambda (x) (concat x ".bib"))
			  (mapcar 'cite-strip-key 
				  (split-string (match-string 1) ","))))
	 ; we did not find a raw latex bibliography. look for bibitems
	 (progn
	   (message "no \\bibliography found")
	   (goto-char (point-min))
	   (re-search-forward "\\(bibitem\\)" nil t)
	   (if (match-string 1) (setq cite-bibliography-files "internal")
             ; no internal bibliography found. now we use the default setting
             ; from the user
             (progn
               (message "no bibitems found")
               (setq cite-bibliography-files jorg-bib-default-bibliography)
               (message "cite-bibliography-files = %s" cite-bibliography-files))))))))
  (message "cite-bibliography-files = %s" cite-bibliography-files)
  cite-bibliography-files)

(defun cite-goto-bibentry (bibfile key)
  "goto the key, in another window if needed."
  (interactive)
  (message "cite-goto-bibentry key=%s | bibfile=%s" key bibfile)
  (if (and bibfile (not (equal bibfile "internal")))
      (find-file-other-window bibfile))
  (org-mark-ring-push)
  (goto-char (point-min)) ; always start search from beginning.
  (message (format "searching for {%s}" key))
  (set-text-properties 0 (length key) nil key)
  (search-forward (format "{%s," key) nil nil 1)
  (message "go back with `C-c &`"))

(defun cite-strip-key (key)
  "strip leading and trailing whitespace from the key"
  (interactive)
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" key)))

(defun cite-split-keys (key-string)
  "split key-string and strip keys. Assumes the key-string is comma delimited"
  (mapcar 'cite-strip-key (split-string key-string ",")))

(defun cite-key-in-file-p (key filename)
  "determine if the key is in the file"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (search-forward key nil t 1)))


(defun get-bibtex-key-under-cursor ()
  "returns key under the bibtex cursor. We search forward from
point to get a comma, or the end of the link, and then backwards
to get a comma, or the beginning of the link. that delimits the
keyword we clicked on. We also strip the text properties.

This code duplicates some code in cite-onclick. one day I will
redesign all of this and clean it up. I wrote this to enable a
tooltip."
  (interactive)
  (let* ((object (org-element-context))
	 (link-string-beginning (org-element-property :begin object))
	 (link-string-end (org-element-property :end object))
	 (path (org-element-property :path object)))    
    (when (and (equal (org-element-type object) 'link) 
               (equal (org-element-property :type object) "cite"))
      ;; The key is the text between commas, or the link boundaries
      (save-excursion
	(if (search-forward "," link-string-end t 1)
	    (setq key-end (- (match-end 0) 1)) ; we found a match
	  (setq key-end link-string-end))) ; no comma found so take the end
      ;; and backward to previous comma from point which defines the start character
      (save-excursion
	(if (search-backward "," link-string-beginning 1 1)
	    (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
	  (setq key-beginning (+ link-string-beginning 5)))) ; no match found
      ;; save the key we clicked on.
      (setq bibtex-key (cite-strip-key (buffer-substring key-beginning key-end)))
      (set-text-properties 0 (length bibtex-key) nil bibtex-key)
      (message "you selected %s" bibtex-key)
      bibtex-key
      )))

(defun get-bibtex-entry (key)
  "Returns the bibtex entry associated with key"
  (interactive)
  (setq cite-bibliography-files (cite-find-bibliography))

  ;; now find the first bib file containing the key if it is a file
  (when (not (equal cite-bibliography-files "internal"))      
      (setq bib-file 
	    (loop for file in cite-bibliography-files do
		  (if (cite-key-in-file-p bibtex-key file) 
		      (return file)))))
  ;; and finally, open the file at the key
  (let ((bibkey))
    (setq bibkey (with-temp-buffer 
	       (insert-file-contents bib-file)
	       (goto-char (point-min))
	       (re-search-forward key)
	       (bibtex-narrow-to-entry)
	       (buffer-string)))
    bibkey))

(defun jorg-bib-tooltip ()
  "display bibtex entry tooltip"
  (interactive)
  (popup-tip (get-bibtex-entry (cite-strip-key (get-bibtex-key-under-cursor)))))

;; variable for the timer object
(defvar idle-timer-bibtex-timer nil)

;; start functions
(defun idle-timer-bibtex-start ()
  (interactive)
  (when (timerp idle-timer-bibtex-timer)
    (cancel-timer idle-timer-bibtex-timer))
  (setq idle-timer-bibtex-timer
          (run-with-timer 1 1 #'jorg-bib-tooltip)))

;; stop function
(defun idle-timer-bibtex-stop ()
  (interactive)
  (when (timerp idle-timer-bibtex-timer)
    (cancel-timer idle-timer-bibtex-timer))
  (setq idle-timer-bibtex-timer nil))

;; (idle-timer-bibtex-start)

(defun cite-onclick (link-string)
  "this function executes when you click on cite link. It identifies the key you clicked on and opens the first bibliography file it finds containing the key.

A regular left-mouse click opens the entry in the bibtex file.
A right-click opens the pdf associated with the entry, if it exists."
  ;; First we find the boundaries of the link you clicked on, then
  ;; identify the key you clicked on. First get boundaries of the link-string
  (message "\n\nyou clicked on %s" link-string)
  (message "%s" last-input-event)
  (save-excursion
    (search-backward "cite:")
    (search-forward link-string nil t 1)
    (setq link-string-beginning (match-beginning 0))
    (setq link-string-end (match-end 0)))
  ;; now we want to search forward to next comma from point, which
  ;; defines the end character of the key
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
  (setq bibtex-key (cite-strip-key (buffer-substring key-beginning key-end)))
  (message (format "found bibtex key: %s" bibtex-key))
  (message "%s" last-input-event)
  (let ((button (car last-input-event)))
    (cond ((eq button 'mouse-2)
           ;; now we get the bibliography files
           (setq cite-bibliography-files (cite-find-bibliography))
           (message "cite-bibliography-files = %s" cite-bibliography-files)
           ;; now find the first bib file containing the key if it is a file
           (if (not (equal cite-bibliography-files "internal"))
               (progn
                 (message "checking files")
                 (setq bib-file (loop for file in cite-bibliography-files do
                                      (if (cite-key-in-file-p bibtex-key file) (return file))))))
           ;; and finally, open the file at the key
           (cite-goto-bibentry bib-file bibtex-key)
           (recenter-top-bottom 1))

          ;; if you right clicked, open the pdf. point is in the bibtex entry.
          ((eq button 'mouse-3)
               (let ((pdf (format (concat jorg-bib-pdf-directory "%s.pdf") bibtex-key)))
                 (if (file-exists-p pdf)
                     (org-open-link-from-string (format "[[file:%s]]" pdf))
                   (message "%s not found" pdf)
                   (ding)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; cite links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-add-link-type
 "cite"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (concat "\\cite{"
	     (mapconcat (lambda (key) key) (cite-split-keys keyword) ",")
	     "}")))))

(org-add-link-type
 "citealp"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<citealp>%s</citealp>)" path))
    ((eq format 'latex)
     (concat "\\citealp{"
	     (mapconcat (lambda (key) key) (cite-split-keys keyword) ",")
	     "}")))))

(org-add-link-type
 "citet"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citet{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citet*"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citet*{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

;; TODO these links do not support options [see][]
(org-add-link-type
 "citep"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citep{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citep*"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citep*{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citeauthor"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeauthor{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citeauthor*"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeauthor*{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citeyear"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeyear{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "nocite"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\nocite{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; index links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-add-link-type
 "index"
 () ; clicks do nothing.
 (lambda (keyword desc format)
   (cond
((eq format 'html)
     (format ""))
    ((eq format 'latex)
     (format "\\index{%s}" keyword)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-full-bibliography ()
  "build pdf of all bibtex entries"
  (interactive)
  (let* ((bibfile (file-name-nondirectory (buffer-file-name)))
	(bib-base (file-name-sans-extension bibfile))
	(texfile (concat bib-base ".tex")))
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
\\bibliographystyle{plainnat}
\\bibliography{%s}
\\end{document}" bib-base))
    (save-buffer)
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "bibtex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
     (kill-buffer texfile)
     ) 
)
    

(defun jb-extract-bibtex ()
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
%s
#+END_SRC" bibfile results)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/ElispCookbook#toc4
(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
	 (let ((elength (length ending)))
	   (string= (substring s (- 0 elength)) ending)))
	(t nil)))

(defun jorg-list-of-figures (&optional arg)
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
			"[^.]*\\.\\(png\\|jpg\\)$"
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
 'jorg-list-of-figures ; on click
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\listoffigures")))))


(defun jorg-list-of-tables (&optional arg)
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
 'jorg-list-of-tables
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\listoftables")))))

(provide 'jorg-bib)
;;; jorg-bib.el ends here
