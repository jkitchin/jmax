;;; swish-e.el --- Interface to swish-e

;;; Commentary:
;;

;; This is the configuration file I used.
;; # Example configuration file

;; # where to save the index
;; IndexFile /Users/jkitchin/.swish-e/index-org.swish-e

;; PropertyNames swish-position

;; # Save descriptions for context on search results.
;; StoreDescription XML <desc> 500
;; StoreDescription XML* <desc> 500

;; # index all tags for searching
;; UndefinedMetaTags auto
;; UndefinedXMLAttributes auto

;; Run the index command as:
;; swish-e -c ~/.swish-e/swish-org.conf -S prog -i ~/bin/swish-org-documents.el

;;; Code:

(defvar swish-e-index
  "~/.swish-e/index-org.swish-e"
  "Path to the index for searching.")


(defun helm-swish-e-candidates (query)
  "Generate a list of cons cells (swish-e result . path)."
  (let* ((result (shell-command-to-string
		  (format "swish-e -f %s -x \"<swishrank>\t<swishdocpath>\t<swish-position>\t<swishtitle>\t<swishdescription>\n\" -w %s"
			  swish-e-index
			  (shell-quote-argument query))))
	 (lines (s-split "\n" result t))
	 (candidates '()))
    (loop for line in lines
	  unless (or  (s-starts-with? "#" line)
		      (s-starts-with? "." line))
	  collect  (let* ((fields (s-split "\t" line))
			  (rank (nth 0 fields))
			  (docpath (nth 1 fields))
			  (position (nth 2 fields))
			  (title (nth 3 fields))
			  (description (nth 4 fields)))
		     (cons (format "%4s %s%s\n%s\n"
				   rank title (if (not (string= "" position))
						  (concat  "::" position)
						"")
				   description)
			   docpath)))))


(defun helm-swish-e (query)
  "Run a swish-e query and provide helm selection buffer of the results.
Example queries:
paragraph=foo
src-block.language=python
not foo
foo near5 bar

see http://swish-e.org/docs/swish-search.html
"
  (interactive "sQuery: ")
  (helm :sources `(((name . ,(format "swish-e: %s" query))
		    (multiline)
		    (candidates . ,(helm-swish-e-candidates query))
		    (action . (("open" . (lambda (f)
					   (org-open-link-from-string f))))))
		   ((name . "New search")
		    (dummy)
		    (action . (("search" . (lambda (f)
					     (helm-swish-e helm-pattern)))))))))

(defun swish-e-todo ()
  (interactive)
  (helm-swish-e "todo-keyword=TODO"))

(provide 'swish-e)

;;; swish-e.el ends here
