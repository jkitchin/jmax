;;; words.el --- Functions to operate on word at point or region  -*- lexical-binding: t -*-
;;; Header:

;;; Commentary:

;; These functions mostly provide easy access to web-based searches of the word at point, or the selected words. The following functions are available.

;; - words-dictionary
;; - words-thesaurus
;; - words-atd :: a spell/grammar checker

;; - words-google
;; - words-twitter

;; - words-google-scholar
;; - words-scopus
;; - words-wos :: Search Web of Science
;; - words-crossref

;; - words-bibtex :: search default bibtex file

;; - words-mdfind :: search local computer with mdfind (Mac)
;; - words-finder :: search local computer with finder (Mac)

;; These functions just open websites for convenience.
;; - wos :: open Web of Science
;; - pubmed :: open pubmed
;; - scopus :: open Scopus

;; - words :: offers a menu of functions for word at point or region

;; ;;; Using :lentic to edit this file
;; This file is a native emacs-lisp file. But, we can use lentic to help edit in either emacs-lisp or org-mode. On opening the emacs-lisp file, you can split the buffer into emacs-lisp and org-mode views.

;; C-c , t    split right
;; C-c , b    split below

;; Then you can edit the code in one window, and the narrative text in the other window in org-mode. The best of both worlds!

;; C-c , h will move the curse to here from the org file to the emacs-lisp file.
;; C-c , s will swap the windows.

;;; Code:

;; ** Dictionary/thesaurus/grammar
;; #+BEGIN_SRC emacs-lisp
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
;; #+END_SRC

;; ** web functions
;; #+BEGIN_SRC emacs-lisp
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
;; #+END_SRC

;; ** Scientific search functions
;; #+BEGIN_SRC emacs-lisp
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


(defun words-scopus ()
  "Scopus the word at point or selection."
  (interactive)
  (browse-url
   (format
    "http://www.scopus.com//search/submit/basic.url?field1=TITLE-ABS-KEY&searchterm1=%s"
    (if (region-active-p)
	(mapconcat 'identity (split-string
			      (buffer-substring (region-beginning)
						(region-end))) "+")
      (thing-at-point 'word)))))


(defun words-wos ()
  "Open the word at point or selection in Web of Science."
  ;; the url was derived from this page: http://wokinfo.com/webtools/searchbox/
  (interactive)
  (browse-url
   (format "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
    (if (region-active-p)
	(mapconcat 'identity (split-string
			      (buffer-substring (region-beginning)
						(region-end))) "+")
      (thing-at-point 'word)))))


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
;; #+end_src

;; ** Convenience functions for scientific queries
;; These just open websites, with no search queries.

;; #+BEGIN_SRC emacs-lisp
(defun wos ()
  "Open Web of Science search page in browser."
  (interactive)
  (browse-url "http://apps.webofknowledge.com"))


(defun pubmed ()
  "Open Pubmed in browser."
  (interactive)
  (browse-url "http://www.ncbi.nlm.nih.gov/pubmed"))


(defun scopus ()
  "Open Scopus in browser."
  (interactive)
  (browse-url "http://www.scopus.com"))
;; #+END_SRC

;; ** bibtex search

;; #+BEGIN_SRC emacs-lisp
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

;; #+END_SRC

;; ** Search functions for Mac

;; #+BEGIN_SRC emacs-lisp
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
;; #+END_SRC



;; ** words menu
;; #+BEGIN_SRC emacs-lisp

(defvar words-funcs '()
 "Functions to run in `words'.  Each entry is a list of (char menu-name function).")

(setq words-funcs
  '(("d" "ictionary" words-dictionary)
    ("t" "hesaurus" words-thesaurus)
    ("g" "oogle" words-google)
    ("c" "CrossRef" words-crossref)
    ("s" "Scopus" words-scopus)
    ("b" "ibtex" words-bibtex)
    ("f" "inder" words-finder)
    ("m" "dfind" words-mdfind)
    ("G" "google-scholar" words-google-scholar)
    ("S" "spell/grammar" words-atd)
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
;; #+END_SRC

;;; End:
;; #+BEGIN_SRC emacs-lisp
(provide 'words)
;;; words.el ends here
;; #+END_SRC


;; # Local Variables:
;; # lentic-init: lentic-orgel-org-init
;; # End:
