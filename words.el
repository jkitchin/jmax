;;; words.el --- Functions to operate on word at point or region

;;; Commentary:
;; 

;;; Code:

(defun words-dictionary ()
  "Look up word at point in an online dictionary."
  (interactive)
  (browse-url
   (format
    "http://dictionary.reference.com/browse/%s?s=t"
    (thing-at-point 'word))))


(defun words-thesaurus ()
  "Look up word at point in an online thesaurus."
  (interactive)
  (browse-url
   (format
    "http://www.thesaurus.com/browse/%s"
    (thing-at-point 'word))))


(defun words-google ()
  "Google the word at point or selection."
  (interactive)
  (browse-url
   (format
    "http://www.google.com/search?q=%s"
    (if (region-active-p)
	(url-hexify-string (buffer-substring (region-beginning)
					     (region-end)))
      (thing-at-point 'word)))))


(defun words-google-scholar ()
  "Google scholar the word at point or selection."
  (interactive)
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s"
    (if (region-active-p)
	(url-hexify-string (buffer-substring (region-beginning)
					     (region-end)))
      (thing-at-point 'word)))))


(defun words-twitter ()
  "Search twitter for word at point or selection."
  (interactive)
  (browse-url
   (format
    "https://twitter.com/search?q=%s"
    (if (region-active-p)
	(url-hexify-string (buffer-substring (region-beginning)
					     (region-end)))
      (thing-at-point 'word)))))


(defun words-atd ()
  "Send paragraph at point to After the deadline for spell and grammar checking."
  (interactive)
  
  (let* ((url-request-method "POST")
	 (url-request-data (format
			    "key=some-random-text-&data=%s"
			    (url-hexify-string
			     (thing-at-point 'paragraph))))
	 (xml  (with-current-buffer
		   (url-retrieve-synchronously
		    "http://service.afterthedeadline.com/checkDocument")
		 (xml-parse-region url-http-end-of-headers (point-max))))
	 (results (car xml))
	 (errors (xml-get-children results 'error)))
    
    (switch-to-buffer-other-frame "*ATD*")
    (erase-buffer)
    (dolist (err errors)
      (let* ((children (xml-node-children err))
	     ;; for some reason I could not get the string out, and had to do this.
	     (s (car (last (nth 1 children))))
	     ;; the last/car stuff doesn't seem right. there is probably
	     ;; a more idiomatic way to get this
	     (desc (last (car (xml-get-children children 'description))))
	     (type (last (car (xml-get-children children 'type))))
	     (suggestions (xml-get-children children 'suggestions))
	     (options (xml-get-children (xml-node-name suggestions) 'option))
	     (opt-string  (mapconcat
			   (lambda (el)
			     (when (listp el)
			       (car (last el))))
			   options
			   " ")))

	(insert (format "** %s ** %s
Description: %s
Suggestions: %s

" s type desc opt-string))))))


(defun words-crossref ()
  "Search region in CrossRef."
  (interactive)
  (browse-url
   (format
    "http://search.crossref.org/?q=%s"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (read-string "string: ")))))


(defun words-bibtex ()
  "Find selected region or word at point in variable `reftex-default-bibliography'."
  (interactive)
  (multi-occur
   (mapcar (lambda (f) (find-file-noselect f))
	   reftex-default-bibliography)
   (if (use-region-p)
       (buffer-substring
	(region-beginning)
	(region-end))
     (thing-at-point 'word))))


(defun words-mdfind ()
  "Search for file names matching word or selection at point using mdfind.
Opens an org-buffer with links to results."
  (interactive)
  (let ((query (if (use-region-p)
		   (buffer-substring
		    (region-beginning)
		    (region-end))
		 (thing-at-point 'word))))
    (switch-to-buffer-other-window "*mdfind*")
    (erase-buffer)
    (insert
     (mapconcat
      (lambda (x)
	(format "[[%s]]" x))
      (split-string
       (shell-command-to-string
	(format "mdfind -name %s"
		(shell-quote-argument query)))
       "\n")
      "\n"))
    (org-mode)))


(defun words-finder ()
  "Open Mac Finder with search of word at point or selection."
  (interactive)
  (let* ((query (if (use-region-p)
		    (buffer-substring
		     (region-beginning)
		     (region-end))
		  (thing-at-point 'word)))
	 (applescript (concat
		       "tell application \"Finder\" to activate
tell application \"System Events\"
	tell process \"Finder\"
		click menu item \"Find\" of menu \"File\" of menu bar 1
		keystroke " (format "\"%s\"" query )
		"
	end tell
end tell")))
    (message "%s" applescript)

    ;; from org-mac-link
    (do-applescript applescript)))


(defvar words-funcs '()
 "Functions to run in `words'.  Each entry is a list of (char menu-name function).")

(setq words-funcs
  '(("d" "ictionary" words-dictionary)
    ("t" "hesaurus" words-thesaurus)
    ("g" "oogle" words-google)
    ("r" "CrossRef" words-crossref)
    ("b" "ibtex" words-bibtex)
    ("f" "inder" words-finder)
    ("m" "dfind" words-mdfind)
    ("c" "google-scholar" words-google-scholar)
    ("s" "spell/grammar" words-atd)
    ("w" "twitter" words-twitter)))
 

(defun words ()
  "Offer menu of functions to run defined in `words-funcs'."
  (interactive)
   (message
   (concat
    (mapconcat
     (lambda (tup)
       (concat "[" (elt tup 0) "]"
	       (elt tup 1) " "))
     words-funcs "") ": "))
   (let ((input (read-char-exclusive)))
     (funcall
      (elt
       (assoc
	(char-to-string input) words-funcs)
       2))))

(provide 'words)

;;; words.el ends here
