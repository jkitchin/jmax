;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.11",
;;;     date            = "04 February 2002",
;;;     time            = "06:10:42 MST",
;;;     filename        = "bibtex-misc.el",
;;;     address         = "Center for Scientific Computing
;;;                        University of Utah
;;;                        Department of Mathematics, 322 INSCC
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 585 1640, +1 801 581 4148",
;;;     URL             = "http://www.math.utah.edu/~beebe",
;;;     checksum        = "52247 1937 6919 62597",
;;;     email           = "beebe@math.utah.edu (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "bibliography, BibTeX, GNU Emacs",
;;;     supported       = "yes",
;;;     docstring       = "This file contains miscellaneous editing
;;;                        support functions for handling BibTeX
;;;                        files in GNU Emacs.
;;;
;;;                        The functions defined here are working
;;;                        prototypes of code that will eventually
;;;                        be moved into bibtex-support.el.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

(defconst bibtex-misc-version "1.11")		;NB: update this at every change
(defconst bibtex-misc-date "[04-Feb-2002]")	;NB: update this at every change

;;; Revision history (reverse chronological order):
;;;
;;; 1.11 [04-Feb-2002]
;;;	Add bibtex-base-26, bibtex-insert-pdf-page-number, and
;;;	fix-duplicate-labels-matching.
;;;
;;; 1.10 [22-Oct-1999]
;;;	Add en-dashify-issue-numbers.
;;;
;;; 1.09 [29-May-1999]
;;;	Move bibtex-fill-value and internal-month-number to bibtools.el,
;;;	so that it does not require this one.
;;;
;;; 1.08 [05-Feb-1999]
;;;	Remove convert-author, because a newer version,
;;;	convert-author-semicolons-to-ands is present in bibtools.el.
;;;
;;;	Move bibtex-get-value and bibdate-to-isodate to bibtools.el,
;;;	so that that file does not require this one.
;;;
;;;	Add (require 'bibtools), since we need about ten functions
;;;	from that file.
;;;
;;; 1.07 [12-Nov-1998]
;;;	Move these stable functions to bibtools.el:
;;;		find-missing-annote
;;;		find-missing-country
;;;		find-missing-day
;;;		find-missing-subject
;;;		fix-duplicate-bibdate
;;;		fix-duplicate-searchkey
;;;		fix-duplicate-subject
;;;		fix-greek-letter-names
;;;
;;; 1.06 [07-Nov-1998]
;;;	Add find-missing-day.
;;;
;;; 1.05 [22-Oct-1998]
;;;	Add bibtex-brace-titleword-everywhere,
;;;	bibtex-generate-unique-label, find-missing-annote, and
;;;	fix-duplicate-searchkey.
;;;
;;; 1.04 [09-Oct-1998]
;;;	Add bibtex-generate-unique-label.
;;;
;;; 1.03 [29-Apr-1998]
;;;	Add fix-siamjna-month-from-1982.
;;;
;;; 1.02 [22-Oct-1997]
;;;	Move these functions to bibtools.el:
;;;		bibdelete
;;;		bibextract
;;;		find-abstract
;;;		find-binary-character
;;;		find-duplicate-author
;;;		find-greek-outside-math
;;;		find-label
;;;		find-long-label
;;;		find-missing-issn
;;;		find-missing-keywords
;;;		find-missing-mrclass
;;;		find-missing-mrnumber
;;;		find-similar-labels
;;;		find-tex-math-item
;;;		fix-accents-with-embedded-spaces
;;;		fix-bibtex-strings
;;;		fix-elided-final-page-number
;;;		fix-missing-italic-correction
;;;		fix-numeric-months
;;;		get-string
;;;		gsub
;;;		isbn-checkdigit
;;;		make-lc-issn-lookup-command
;;;		make-melvyl-codenn-lookup-command
;;;		make-melvyl-issn-lookup-command
;;;		make-oclc-issn-lookup-command
;;;		make-oclc-s-ti-command
;;;		qr12
;;;
;;;	Rename check-page-ranges to check-page-ranges-1, since the
;;;	version in bibtools.el is the current one, and I may want
;;;	the old version again some time.
;;;
;;;	Delete
;;;		fix-duplicate-bibsource
;;;		fix-duplicate-classification
;;;		find-missing-acknowledgement
;;;		find-missing-coden
;;;		fix-old-style-bracing
;;;		make-oclc-isbn-lookup-command
;;;		remove-duplicate-entries
;;;	since the newer versions are already in bibtools.el.

(provide 'bibtex-misc)				;used by sun-keys.el

(require 'bibtex-mods)
(require 'bibtools)

;; NB: Please keep these functions in ALPHABETICAL order, ignoring lettercase

(defun bibtex-base-26 (number)
  "Convert NUMBER to a base-26 [digits a..z] number and return its
string representation."
  (let ((s "") (i))
    (while (> number 0)
      (setq i (% number 26))
      (setq number (/ number 26))
      (setq s (concat
	       (substring "abcdefghijklmnopqrstuvwxyz" i (1+ i))
	       s)))
    (if (string-equal s "")		;NUMBER == 0
	(setq s "a"))
    s))


(defun bibtex-brace-titleword-everywhere (&optional count)
  "With point on or after a titleword, supply braces around each
instance of this word in title or booktitle strings in the entire
BibTeX buffer.  An argument, COUNT, means to brace that many words
preceding point; the default is one word.

Replacement is done by query-replace, so that replacements can be
controlled manually if desired."
  (interactive "P")
  (setq count (if count (prefix-numeric-value count) 1))
  (let ((braced-words nil)
	(end nil)
	(original-start nil)
	(regexp nil)
	(start nil)
	(words nil))
    (setq original-start (point))
    (backward-word 1)
    (forward-word 1)
    (setq end (point))
    (backward-word count)
    (setq words (buffer-substring (point) end))
    (message (format "bracing title word%s %s"
		     (if (> count 1) "(s)" "")
		     words))
    (setq braced-words (concat "{" words "}"))
    (setq regexp (concat "\\("
			 "^ *\\(booktitle\\|title\\) *= *\"[^\"]*[^-{A-Za-z']"
			 words
			 "[^-}A-Za-z']"
			 "\\)\\|\\("
			 "^ *\\(booktitle\\|title\\) *= *\""
			 words
			 "[^-}A-Za-z']"
			 "\\)"))
    (goto-char (point-min))
    (with-output-to-temp-buffer "*braced-titlewords*"
      (while (re-search-forward regexp nil t)
	(setq start (match-beginning 0))
	(search-forward "\",")
	(setq end (point))
	(narrow-to-region start end)
	(goto-char start)


	(search-forward words)
	(bibtex-backward-brace-words count)
	;; (query-replace words braced-words)

	(widen)
	(princ (buffer-substring start (+ 3 end)))
	(goto-char end)))
    (goto-char original-start)))


(defun bibtex-convert-book-to-proceedings ()
  "Convert the entry in which point resides from a Book to a
Proceedings entry."
  (interactive)
  (let ((end) (start) (value-start) (value))
    (save-excursion
      (save-restriction
	(re-search-backward "^@")
	(if (not (looking-at "^@Book"))
	    (error "Not in @Book{...} entry")
	  (setq start (point))
	  (re-search-forward "^}")
	  (setq end (point))
	  (narrow-to-region start end)

	  ;; Change @Book to @Proceedings
	  (goto-char start)
	  (delete-char 5)
	  (insert "@Proceedings")

	  ;; Change author to editor
	  (if (re-search-forward "^ *author *=" nil t)
	      (progn
		(back-to-indentation)
		(delete-char 6)
		(insert "editor")))

	  ;; Add booktitle if needed
	  (goto-char start)
	  (if (not (re-search-forward "^ *booktitle *=" nil t))
	      (progn
		(goto-char start)
		(if (re-search-forward "^ *title *= *\"" nil t)
		    (progn
		      (setq value-start (1- (point)))
		      (re-search-forward "\", *$")
		      (setq value (buffer-substring value-start
						    (point)))
		      (goto-char value-start)
		      (beginning-of-line)
		      (insert (format "  booktitle =    %s\n" value))))))

	  ;; Mark the change
	  (update-bibdate)))))
  ;; move into next entry, for convenience in converting multiple entries
  (re-search-forward "^}")
  (forward-line 3))


(defun bibtex-copy-title-to-booktitle ()
  "In the entry in which point lies, copy the title value string to
the booktitle string."
  (interactive)
  (if (re-search-backward "^@" nil t)
      (let ((start) (end))
	(if (re-search-forward "^ *title *=\\|^}"  nil t)
	    (progn
	      (beginning-of-line)
	      (if (looking-at "^ *title *=")
		  (progn
		    (setq start (point))
		    (re-search-forward "\", *\n")
		    (setq end (point))
		    (goto-char start)
		    (insert (buffer-substring start end))
		    (goto-char start)
		    (back-to-indentation)
		    (insert "book")
		    (search-forward "=")
		    (delete-horizontal-space)
		    (insert "    ")))
	      (re-search-forward "^}"))))))


(defun bibtex-generate-unique-label (label)
  "Generate unique citation labels for entries in a BibTeX buffer from
the argument LABEL by appending lower-case letters a, b, c, ....

There is no check for collisions with existing labels."
  (interactive "sCitation label: ")
  (let ((n 97))
    (goto-char (point-min))
    (while (re-search-forward (concat "^@[A-Za-z]+{" label ",") nil t)
      (backward-char 1)
      (insert-char n 1)
      (setq n (1+ n))
      (if (> n ?z)
	  (error "More than 26 suffixes applied")))))


(defun bibtex-insert-pdf-page-number ()
  "Scan a buffer containing a script with pdfinfo output, find the
BibTeX entry in the other window with that PDF file in a URL, then
search backwards for the pages key, and replace any ??  there with the
correct final page number."
  (interactive)
  (let ((count) (page) (pdffile) (start))
    (re-search-forward "[.]pdf")
    (backward-word 2)
    (setq start (point))
    (forward-word 2)
    (setq pdffile (buffer-substring start (point)))
    (re-search-forward "^Pages:")
    (forward-word 1)
    (backward-word 1)
    (setq start (point))
    (forward-word 1)
    (setq count (string-to-number (buffer-substring start (point))))
    (other-window 1)
    (search-forward pdffile)
    (re-search-backward "^@\\|^ *pages *= *\"[0-9]+-+[?][?]\",")
    (beginning-of-line)
    (if (looking-at "^@")
	(error "No missing page number found")
      (forward-word 2)
      (backward-word 1)
      (setq start (point))
      (forward-word 1)
      (setq page (string-to-number (buffer-substring start (point))))
      (search-forward "??")
      (delete-backward-char 2)
      (insert (number-to-string (+ page count -1))))
    (other-window 1)))

(defun bibtex-sort-keywords ()
  "Sort keywords in a BibTeX keywords value string.
WARNING: This does not work: it goes into an infinite loop, sigh... Why???"
  (interactive)
  (if (re-search-forward "^ *keywords *= *\"" nil t)
      (let ((start (point)))
	(search-forward "\",")
	(sort-regexp-fields nil "[^,;]*" "\\&" start (- (point) 2)))))


(defun bibtex-supply-missing-key (key)
  "If the current entry lacks KEY, supply one with an unknown value."
  (let ((start (point)))
    (re-search-backward "^@")
    (re-search-forward (concat "^ *" key " *=\\|^}"))
    (if (string-equal "}" (buffer-substring (match-beginning 0)
					    (1+ (match-beginning 0))))
	(progn
	  (beginning-of-line)
	  (insert "  "
		  key
		  " ="
		  (substring "                " 0 (- 13 (length key)))
		  "\"????\",\n")
	  (goto-char start)))))(

defun convert-book-to-proceedings ()
  "If the current entry is a Book type, change the type from Book to
Proceedings, and the author to editor, and then create a booktitle
from the title value."
  (interactive)
  (re-search-backward "^@")
  (if (looking-at "@Book")
      (let ((start (point)) (title))
	;; (recenter 1)
	(delete-char 5)
	(insert "@Proceedings")
	(re-search-forward "^ *author *=\\|^}")
	(if (string-equal "}" (buffer-substring (match-beginning 0)
						(1+ (match-beginning 0))))
	    (error "No author found!"))
	(delete-region (match-beginning 0) (match-end 0))
	(insert "  editor =")
	(goto-char start)
	(re-search-forward "^ *title *= *\"\\|^}")
	(if (string-equal "}" (buffer-substring (match-beginning 0)
						(1+ (match-beginning 0))))
	    (error "No title found!"))
	(setq start (point))
	(search-forward "\",")
	(setq title (buffer-substring start (- (point) 2)))
	(goto-char start)
	(beginning-of-line)
	(insert "  booktitle =    \"" title "\",\n"))))


;;; (defun check-page-ranges ()
;;;   "In a BibTeX buffer that has been sorted into publication order with
;;; bibsort -byvolume, check that page ranges of adjacent articles are
;;; consistent.  Problem entries are copied to a temporary buffer *check
;;; page ranges* for possible printing and manual correction."
;;;   (interactive)
;;;   (with-output-to-temp-buffer "*check page ranges*"
;;;     (let ((initial) (final) (previous 0) (previous-entry "") (this-entry) (errors 0))
;;;       (while (re-search-forward "^ *pages *= *\"\\([0-9]+\\)--\\([0-9]+\\)\"," nil t)
;;; 	(setq initial (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
;;; 	(setq final (string-to-number (buffer-substring (match-beginning 2) (match-end 2))))
;;; 	(save-excursion
;;; 	  (mark-paragraph)
;;; 	  (setq this-entry (buffer-substring (region-beginning) (region-end))))
;;; 	(if (> previous 0)
;;; 	    (progn
;;; 	      (if (not (= (1+ previous) initial))
;;; 		  (progn
;;; 		    (setq errors (1+ errors))
;;; 		    (princ previous-entry)
;;; 		    (princ "\n")
;;; 		    (princ this-entry)
;;; 		    (message "%d: Inconsistent page range: %d : %d--%d"
;;; 			     errors previous initial final)
;;; 		    (princ (format "\nInconsistent page range: %d : %d--%d"
;;; 				   previous initial final))
;;; 		    (princ "--------------------\n\n")))))
;;; 	(setq previous-entry this-entry)
;;; 	(setq previous final)))))


(defun check-page-ranges-1 ()
  "In a BibTeX buffer that has been sorted into publication order with
bibsort -byvolume, check that page ranges of adjacent articles are
consistent."
  (interactive)
  (let ((initial) (final) (previous 0))
    (while (re-search-forward "^ *pages *= *\"\\([0-9]+\\)--\\([0-9]+\\)\"," nil t)
      (setq initial (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
      (setq final (string-to-number (buffer-substring (match-beginning 2) (match-end 2))))
      (if (> previous 0)
	  (progn
	    (if (not (= (1+ previous) initial))
		(error "Inconsistent page range: %d : %d--%d" previous initial final))))
      (setq previous final))))


(defun check-page-ranges-2 ()
  "In a BibTeX buffer that has been sorted into publication order with
bibsort -byvolume, check that page ranges of adjacent articles are
consistent.  Problem entries are copied to a temporary buffer *check
page ranges* for possible printing and manual correction."
  (interactive)
  (with-output-to-temp-buffer "*check page ranges*"
    (let ((initial) (final) (previous 0) (previous-entry "") (this-entry) (errors 0))
      (while (re-search-forward "^ *pages *= *\"\\([0-9]+\\)--\\([0-9]+\\)\"," nil t)
	(setq initial (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
	(setq final (string-to-number (buffer-substring (match-beginning 2) (match-end 2))))
	(save-excursion
	  (mark-paragraph)
	  (setq this-entry (buffer-substring (region-beginning) (region-end))))
	(if (> previous 0)
	    (progn
	      (if (not (= (1+ previous) initial))
		  (progn
		    (setq errors (1+ errors))
		    (princ previous-entry)
		    (princ "\n")
		    (princ this-entry)
		    (message "%d: Inconsistent page range: %d : %d--%d"
			     errors previous initial final)
		    (princ (format "\nInconsistent page range: %d : %d--%d"
				   previous initial final))
		    (princ "--------------------\n\n")))))
	(setq previous-entry this-entry)
	(setq previous final)))))


(defun check-star-names ()
  "Check DVI driver family function header comments for correctly
centered names in starred comments of the form

/***** name *****/

The current fill-column determines the expected line length.
When mismatches are found, a corrected comment line is inserted
in the buffer, with point left on the incorrect line."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^/[*]* \\([a-z0-9_]+\\) [*]*/$" nil t)
    (let* ((line) (name (buffer-substring (match-beginning 1) (match-end 1)))
	   (n (length name)) (n1) (n2))
      (setq n1 (/ (- fill-column n 2) 2))
      (setq n2 (- fill-column n 2 n1))
      (setq line (concat "/" (make-string (1- n1) ?*) " " name " "
			 (make-string (1- n2) ?*) "/"))
      (beginning-of-line)
      (setq n (point))
      (end-of-line)
      (if (not (string-equal line (buffer-substring n (point))))
	  (progn
	    (insert "\n" line)
	    (beginning-of-line 0)
	    (error "Mismatch:")))))
  (goto-char (point-min))
  (message "[done]"))


(defun check-volume-year-consistency ()
  "Check volume and year consistency in a journal with yearly volumes
beginning in 1975.  [Yes, this is for a specific journal, but I forget
which...]."
  (interactive)
  (let ((volume) (year) (vpos))
    (while (re-search-forward "^ *volume *= *\"\\([0-9]+\\)\"" nil t)
      (setq vpos (match-beginning 1))
      (setq volume (string-to-int
		    (buffer-substring (match-beginning 1)
				      (match-end 1))))
      (re-search-forward "^ *year = *\"")
      (setq year (string-to-int (buffer-substring (point) (+ 4 (point)))))
      (if (not (= volume (- year 1975)))
	  (progn
	    (goto-char vpos)
	    (kill-word 1)
	    (insert (format "%d" (- year 1975)))
	    (error "volume/year mismatch: expected volume %d found %d"
		   (- year 1975) volume))))))


(defun convert-article-to-inproceedings ()
  "If the current entry is an Article type, change the type from
Article to InProceedings, and the journal key to a booktitle key, and
supply any missing publisher, address, ISBN, and LCCN key/values."
  (interactive)
  (re-search-backward "^@")
  (if (looking-at "@Article")
      (let ((start (point)) (title))
	;; (recenter 1)
	(delete-char 8)
	(insert "@InProceedings")
	(re-search-forward "^ *journal *=\\|^}")
	(if (string-equal "}" (buffer-substring (match-beginning 0)
						(1+ (match-beginning 0))))
	    (error "No journal found!"))
	(delete-region (match-beginning 0) (match-end 0))
	(insert "  booktitle =")
	(bibtex-supply-missing-key "ISBN")
	(bibtex-supply-missing-key "LCCN")
	(bibtex-supply-missing-key "publisher")
	(bibtex-supply-missing-key "address"))))


(defun convert-en-dashes-to-em-dashes ()
  "Convert en dashes to em dashes in the current buffer, where this
appears to be necessary.  The job is done with query-replace-regexp,
so that there is an opportunity to override the change."
  (interactive)
  (query-replace-regexp "\\([^?0-9-]\\)--\\([^?0-9-]\\)" "\\1 --- \\2" nil))


(defun convert-sgml-accents-to-tex-accents()
  "Convert SGML accents to TeX accents in the current buffer."
  (interactive)
  (convert-sgml-accents-to-tex-accents-helper "&aacute;" "{\\'a}")
  (convert-sgml-accents-to-tex-accents-helper "&auml;" "{\\\"a}")
  (convert-sgml-accents-to-tex-accents-helper "&eacute;" "{\\'e}")
  (convert-sgml-accents-to-tex-accents-helper "&iacute;" "{\\'\\i}")
  (convert-sgml-accents-to-tex-accents-helper "&ouml;" "{\\\"o}")
  (convert-sgml-accents-to-tex-accents-helper "&uuml;" "{\\\"u}")
  )


(defun convert-sgml-accents-to-tex-accents-helper (sgml tex)
  "This is a private helper function for convert-sgml-accents-to-tex-accents."
  (goto-char (point-min))
  (replace-string sgml tex))


(defun convert-sub-sup-to-tex (beg end)
  "Convert library catalog /sub .../ and /sup .../ sequences to
TeX-style mathematical markup in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (query-replace-regexp "/sub[ \n]+\\(.\\)/" "_\\1" nil)
      (goto-char (point-min))
      (query-replace-regexp "/sup[ \n]+\\(.\\)/" "^\\1" nil)
      (goto-char (point-min))
      (query-replace-regexp "/sub[ \n]+\\(..+\\)/" "_{\\1}" nil)
      (goto-char (point-min))
      (query-replace-regexp "/sup[ \n]+\\(..+\\)/" "^{\\1}" nil))))


(defun d-coden ()
  "What does this do??"
  (interactive)
  (let ((msg "d 1-/coden/issn\n"))
    (x-set-selection 'PRIMARY msg)
    (x-set-cut-buffer msg)))


(defun d-isbn ()
  "What does this do??"
  (interactive)
  (let ((msg "d 1-/pages/isbn\n"))
    (x-set-selection 'PRIMARY msg)
    (x-set-cut-buffer msg)))


(defun delete-publisher-and-address-in-article()
  "Delete publisher and address key/value pairs in @Article{...}."
  (interactive)
  (while (re-search-forward "^@Article" nil t)
    (let ((start (point)) (end))
      (re-search-forward "^}")
      (setq end (point))
      (goto-char start)
      (while (re-search-forward "^ *publisher *=\\|^ *address *=" end t)
	(beginning-of-line)
	(kill-line 1)))))


(defun en-dashify-date-ranges ()
 "Find apparent hyphenated date ranges starting at point in the
current buffer, and change the hyphens to en-dashes."
 (interactive)
 (query-replace-regexp " \\([0-9][0-9]?\\)-\\([0-9][0-9]?\\)\\([^0-9]\\)" " \\1--\\2\\3" nil))


(defun en-dashify-issue-numbers ()
 "Find apparent hyphenated and slashed issue number pairs starting at
point in the current buffer, and change the hyphens and slashes to
en-dashes."
 (interactive)
 (query-replace-regexp "^ *number *= *\"\\([0-9]+\\)[-/]\\([0-9]+\\)\","
		       "  number =       \"\\1--\\2\"," nil))


(defun find-abbreviated-definition ()
  "Find define() calls with  embedded abbreviations."
  (interactive)
  (let ((s) (start))
    (catch 'DONE
      (while (re-search-forward "^ *define([^,]+,[ \t]*\".*[a-z][.]" nil t)
	(set-mark (point))
	(beginning-of-line)
	(re-search-forward "^ *define([^,]+,[ \t]*\"" nil t)
	(setq start (point))
	(beginning-of-line)
	(setq s (buffer-substring (point) start))
	(forward-line -1)
	(if (looking-at s)
	    (goto-char start)
	  (goto-char start)
	  (throw 'DONE nil))))))


(defun find-address-without-country ()
  "Find the next address field that has only a single (city) name, or
just a city and two-letter state."
  (interactive)
  (if (re-search-forward "^ *address *= *\"[^,?]+\",\\|^ *address *= *\"[^,?]+, *[A-Z]+\"," nil t)
      (progn
	(if (string-equal (buffer-substring (- (match-end 0) 4) (- (match-end 0) 2)) "UK")
	    (find-missing-country)
	  (push-mark)))))


(defun find-bad-number-range ()
  "Find number (usually, date) ranges that need an em-dash instead of a
hyphen.  See find-bad-date-range for an alternative implementation."
  (interactive)
  (if (re-search-forward "[^0-9-][0-9][0-9]?-[0-9][0-9]?[^0-9-]" nil t)
      (search-backward "-")
    (error "no more bad number ranges")))


(defun find-broken-inline-math ()
  "Move to the next line containing an odd number of non-backslashed
dollar signs, possibly inline math that has been split across line
boundaries."
  (interactive)
  (let ((n 0) (dollar-pos))
    (beginning-of-line)
    (while (< (point) (point-max))
      (re-search-forward "[$\n]")
      (cond
       ((string-equal "\n" (buffer-substring (1- (point)) (point)))
	(if (= 1 (% n 2))
	    (progn
	      (goto-char dollar-pos)
	      (error "broken inline math %d" n)))
	(setq n 0))
       ((not (string-equal (buffer-substring (- (point) 2) (1- (point))) "\\"))
	(setq dollar-pos (1- (point)))
	(setq n (1+ n)))))))


(defun find-double-month ()
  "Search forward for next bibliography entry containing two month entries.
This is for bibliographies where month, OPTmonth, and UNKmonth have been used."
  (interactive)
  (while (re-search-forward "^@[A-Za-z]+{" nil t)
    (re-search-forward "^ *[A-Za-z]*month *=")
    (point-to-register ?0)
    (forward-line 1)
    (re-search-forward "^ *[A-Za-z]*month *=\\|^ *}")
    (beginning-of-line)
    (if (looking-at "^ *[A-Za-z]*month *=")
	(error "second month value"))))

(defun find-duplicate-set_xxxx ()
  "Find adjacent pairs of set_xxxx() calls for the same journal."
  (interactive)
  (let ((s) (start))
    (while (re-search-forward "^ *set_[A-Za-z]+(\"[^\"]*\"," nil t)
      (setq start (point))
      (beginning-of-line)
      (setq s (buffer-substring (point) start))
      (forward-line 1)
      (if (looking-at s)
	  (error "Duplicate set_xxxx() call")))))


(defun find-long-author ()
  "Find the next long author/editor value."
  (interactive)
  (re-search-forward "^ *\\(author\\|editor\\) *= *\"" nil t)
  (end-of-line)
  (if (> (current-column) 70)
      (progn
	(push-mark)
	(recenter 2)
	(error "long value"))
    (find-long-author)))


(defun find-multiple-definitions ()
  "Find adjacent pairs of define() calls for the same journal, and, if
transient-mark-mode is enabled, highlight the first of them."
  (interactive)
  (let ((s) (start))
    (catch 'DONE
      (while (re-search-forward "^ *define([^,]+,[ \t]*\"" nil t)
	(setq start (point))
	(beginning-of-line)
	(setq s (buffer-substring (point) start))
	(set-mark (point))
	(forward-line -1)
	(if (not (looking-at "^$"))
	    (re-search-forward "^$")
	  (forward-line 2)
	  (if (looking-at s)
	      (throw 'DONE nil)))))))


(defun find-next-unknown-value ()
  "Starting on the next line, move to the start of the next unknown
value in a BibTeX entry, marked by two or more queries, and push the
mark there."
  (interactive)
  (forward-line 1)
  (search-forward "??")
  (backward-char 2)
  (push-mark))


(defun find-next-unknown-value-and-update ()
  "Starting on the next line, move to the start of the next unknown
value in a BibTeX entry, marked by two or more queries, and push the
mark there.  Delete to the remainder of the field, and update the
bibdate value.  This is convenient for supplying missing values in a
bibliography after a database or library search."
  (interactive)
  (forward-line 1)
  (search-forward "??")
  (backward-char 2)
  (delete-to-end-of-BibTeX-field)
  (save-excursion
    (update-bibdate))
  (push-mark))


(defun find-present-annote ()
  "Search forward for next Thesis bibliography entry containing an `annote' value."
  (interactive)
  (find-present-xxx "^@MastersThesis\\|@PhDThesis" "^[ \t]*annote[ \t]*="))


(defun find-present-series ()
  "Search forward for next Thesis bibliography entry containing an `annote' value."
  (interactive)
  (find-present-xxx "^@TechReport" "^[ \t]*series[ \t]*="))


(defun find-present-xxx (entry-pattern entry-type)
  "Search forward for next bibliography entry matching ENTRY-PATTERN
that contains a value for ENTRY-TYPE."
  (re-search-forward entry-pattern)
  (let ((start (point)) (end))
    (catch 'EXIT
      (while (re-search-forward "^}" nil t)
	(setq end (point))
	(goto-char start)
	(setq start (1+ end))
	(if (not (re-search-forward entry-type end t))
	    (progn
	      (goto-char start)
	      (re-search-forward entry-pattern nil t)
	      (setq start (point)))
	  (goto-char start)
	  (throw 'EXIT nil)))))
  (if (= (point) (point-max))
      (message "[Done]")))


(defun find-problems ()
  "Find common BibTeX file coding problems."
  (interactive)
  (re-search-forward
   "[:;.,!?]}\\|{[^}\n]*-\\|[a-zA-Z]- +[A-Za-z]\\|su[bp] ")
  (set-mark (match-beginning 0))
  (push-mark (match-beginning 0))
  (recenter 2))


(defun find-similar-labels-2 ()
  "Search forward for consecutive bibliography entries with similar
BibNet-style citation labels, matching in the author name and year."
  (interactive)
  (catch 'DONE
    (let ((label "") (pos nil))
      (while (re-search-forward "^@[A-Za-z]+{\\([^:]*:\\)" nil t)
	(setq label (buffer-substring (match-beginning 1) (match-end 1)))
	(setq pos (match-beginning 0))
	(if (and (re-search-forward "^@[A-Za-z]+{" nil t)
		 (looking-at label))
	    (progn
	      (push-mark pos)
	      (throw 'DONE nil)))
	(goto-char (1+ pos)))
      (error "[done]"))))


(defun fix-annote ()
  "Remove unwanted text from annote values."
  (interactive)
  (flush-lines "^  annote =       \"Biblio[a-z.]+ *: *leaf *[\\[\\]0-9\\-]+[.]?\",$")
  (flush-lines "^  annote =       \"Biblio[a-z.]+ *: *leaves *[\\[\\]0-9\\-]+[.]?\",$")
  (flush-lines "^  annote =       \"Biblio[a-z.]+ *: *p. *[\\[\\]0-9\\-]+[.]?\",$")
  (flush-lines "^  annote =       \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(leaf [\\[\\]0-9]+)[.]?\",$")
  (flush-lines "^  annote =       \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(leaves *[\\[\\]0-9\\-]+[.]?)\",$")
  (flush-lines "^  annote =       \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(p[.] *[\\[\\]0-9]\\-)[.]?\",$")
  (flush-lines "^  annote =       \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(p[.] *[\\[\\]0-9]\\-) and indexes[.]?\",$")
  (flush-lines "^  annote =       \"Includes bibliography[.]?\",$")
  (flush-lines "^  annote =       \"Includes index[.]?\",$")
  (replace-regexp "Includes bibliographical references (p[.] [0-9\\-]+) and index\\(es\\)?[.]" "" nil))



;; Temporary arbitrary alternative until the real
;; fix-duplicate-bibdate() is completed:
;; (defun fix-duplicate-bibdate ()
;;  (interactive)
;;  (while (re-search-forward "^ *bibdate *=" nil t)
;;    (forward-line 1)
;;    (if (looking-at "^ *bibdate *=")
;;	(kill-line 1))))


(defun fix-duplicate-issn-1 ()
  "Check for adjacent duplicate ISSN fields, and delete one of them."
  (interactive)
  (let ((start) (s))
    (while (re-search-forward "^ *ISSN *=" nil t)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq s (buffer-substring start (point)))
      (forward-line 1)
      (if (looking-at s)
	  (kill-line 1)))))


(defun fix-duplicate-issn-2 ()
  "Check for duplicate ISSN fields in the same entry, and delete one of them."
  (interactive)
  (let ((start) (s))
    (while (re-search-forward "^ *ISSN *=" nil t)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq s (buffer-substring start (point)))
      (re-search-forward "^ *ISSN *=\\|^}")
      (beginning-of-line)
      (if (looking-at s)
	  (kill-line 1)))))


(defun fix-duplicate-labels-matching (label)
  "Add a base-26 (a..z) suffix to citation labels exactly matching
LABEL in all entries from point to end-of-buffer."
  (let ((n 0)
	(pattern (concat "@[A-RS-T][a-z]+{" label "R,")))
    (while (search-forward pattern nil t)
      (backward-char 1)
      (insert (bibtex-base-26 n))
      (setq n (1+ n)))))


(defun fix-duplicate-pages ()
  "Position to the next pages entry with ?? as its final page number."
  (interactive)
  (let ((s) (start))
    (catch 'DONE
      (while (re-search-forward "^\\( *pages *= *\"[0-9]+\\)--" nil t)
	(setq start (match-beginning 1))
	(setq s (buffer-substring (match-beginning 1) (match-end 1)))
	(if (looking-at "[?][?]")
	    (progn
	      (message s)
	      (forward-line 1)
	      (beginning-of-line)
	      (if (string-equal s (buffer-substring (point) (+ (point) (length s))))
		  (progn
		    (forward-line -1)
		    (beginning-of-line)
		    (throw 'DONE nil)))
	      (forward-line -2)
	      (beginning-of-line)
	      (if (string-equal s (buffer-substring (point) (+ (point) (length s))))
		  (progn
		    (forward-line 1)
		    (beginning-of-line)
		    (throw 'DONE nil)))))
	(goto-char start)
	(forward-line 1)))))


(defun fix-duplicate-pages-2 ()
  "Fix adjacent duplicate pages, collapsing them into the form
\"xx (or yy??)\"."
  (interactive)
  (query-replace-regexp "^\\( *pages *= *\"[^\"]+\\)\",\n *pages *= *\"\\([^\"]+\\)" "\\1 (or \\2??)" nil))


(defun fix-duplicate-url ()
  "Merge duplicate URL lines into single ones."
  (interactive)
  (query-replace-regexp
     "\\(URL *= *\"\\)\\([^\"]*\\)\",\n *\\(URL *= *\"\\)\\([^\"]*\\)\","
     "\\1\\2, \\4\","))


(defun fix-empty-coden ()
  "Find adjacent pairs of set_CODEN() calls for the same journal,
and if only one of them is empty, delete it."
  (interactive)
  (let ((s) (start))
    (while (re-search-forward "^ *set_CODEN(.*\"\")" nil t)
      (backward-char 3)
      (setq start (point))
      (beginning-of-line)
      (setq s (buffer-substring (point) start))
      (forward-line 1)
      (if (looking-at s)
	  (progn
	    (forward-line -1)
	    (kill-line 1))))))


(defun fix-empty-issn ()
  "Find adjacent pairs of set_ISSN() calls for the same journal,
and if only one of them is empty, delete it."
  (interactive)
  (let ((s) (start))
    (while (re-search-forward "^ *set_ISSN(.*\"\")" nil t)
      (backward-char 3)
      (setq start (point))
      (beginning-of-line)
      (setq s (buffer-substring (point) start))
      (forward-line 1)
      (if (looking-at s)
	  (progn
	    (forward-line -1)
	    (kill-line 1))))))


(defun fix-jr()
  "Convert an author name of the form \"Jr. {Ryan, D. J. }\" to standard
form \"D. J. {Ryan, Jr.}\", repairing a problem with some of the older
database conversion software.  Names with Sr. instead of Jr. are also
handled."
  (interactive)
  (goto-char (point-min))
  (query-replace-regexp
	 "\\([JS]\\)r[.]?[ \n]*{\\([A-Z][A-Za-z'-]*\\)[ \n]*,[ \n]*\\(\\([A-Z][A-Za-z'-]*[.]?[ \n]*\\)+\\)}"
	 "\\3 {\\2, \\1r.}")
  (goto-char (point-min))
  (query-replace-regexp "\\([A-Z][.]\\)  {" "\\1 {"))


(defun fix-mgquery-typescript ()
 "Fixup an mgquery search typescript, inserting bibsource lines
to record the origin of each bibliography entry."
  (interactive)
  (goto-char (point-min))
  (fix-typescript)
  (goto-char (point-min))
  (let ((start) (end))
    (while (re-search-forward "^File=" nil t)
      (setq start (point))
      (search-forward " ")
      (setq end (1- (point)))
      (re-search-forward "^}")
      (beginning-of-line)
      (insert (format "  bibsource = \"%s\",\n"
		      (buffer-substring start end))))))


(defun fix-missing-pages ()
  "In a bibliography that is sorted in publication order, e.g. with
bibsort -byvolume, and in which ending page numbers are sometimes
missing (and so indicated by a \"??\" value), and for which the journal
does not leave empty pages between articles, supply the missing page
numbers."
  (interactive)
  (let ((field)
	(missing-pages)
	(next-number)
	(next-pages)
	(next-volume)
	(number)
	(pages)
	(start)
	(volume))
    (while (re-search-forward "^ *pages *= *\"[0-9]+--[?][?]\"," nil t)
      (setq missing-pages (- (point) 4))
      (setq number (bibtex-get-value "number"))
      (setq volume (bibtex-get-value "volume"))

      ;; If we can get (volume, number, pages) data from the
      ;; next entry, and (volume, number) match, and pages
      ;; is positive, then we can supply a final page number.
      (if (re-search-forward "^@" nil t)
	  (progn
	    (forward-char 1)
	    (setq next-number (bibtex-get-value "number"))
	    (setq next-volume (bibtex-get-value "volume"))
	    (setq next-pages (bibtex-get-value "pages"))
	    (setq pages (1- (string-to-int next-pages)))
	    (goto-char missing-pages)
	    (if (and (string-equal next-volume volume)
		     (string-equal next-number number)
		     (> pages 0))
		(progn
		  (delete-char 2)
		  (insert (format "%d" pages)))))))))


(defun fix-note ()
  "Remove unwanted text from note values."
  (interactive)
  (flush-lines "^  note =         \"Biblio[a-z.]+ *: *leaf *[\\[\\]0-9\\-]+[.]?\",$")
  (flush-lines "^  note =         \"Biblio[a-z.]+ *: *leaves *[\\[\\]0-9\\-]+[.]?\",$")
  (flush-lines "^  note =         \"Biblio[a-z.]+ *: *p. *[\\[\\]0-9\\-]+[.]?\",$")
  (flush-lines "^  note =         \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(leaf [\\[\\]0-9]+)[.]?\",$")
  (flush-lines "^  note =         \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(leaves *[\\[\\]0-9\\-]+[.]?)\",$")
  (flush-lines "^  note =         \"Inc[a-z.]+ biblio[a-z.]+ ref[a-z.]+ *(p[.] *[\\[\\]0-9]\\-)[.]?\",$")
  (flush-lines "^  note =         \"Includes bibliography[.]?\",$")
  (flush-lines "^  note =         \"Includes index[.]?\",$")
  (replace-regexp "Includes bibliographical references (p[.] [0-9\\-]+) and index\\(es\\)?[.]" "" nil)
)


(defun fix-oclc-pages ()
  "Convert OCLC pages value strings to standard forms."
  (interactive)
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *diagrs[.] *[0-9]+ *cm[.]\"," "\\1\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *[pl]?[.] *illus[.]? *[0-9]+ *cm[.]\","  "\\1\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *[pl]?[.] *\\(illus[.]\\)? *[0-9]+ *cm[.]\","  "\\1\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *[pl]?[.] *[0-9]+ *cm[.]\"," "\\1\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *[pl]?[.] [0-9]+ cm[.]\"," "\\1\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *[pl]?[.] illus. *[0-9]+ *cm[.]\"," "\\1\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\)[0-9]+ *v[.] *(loose-*leaf) *\\(illus.\\)? *[0-9]+ *cm[.]\"," "\\1 various\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\)[0-9]+ *v[.] *(various *pagings) *\\(illus.\\)? *[0-9]+ *cm[.]\"," "\\1 various\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\)[0-9]+ *v[.] *(var[.] *pagings) *\\(illus.\\)? *[0-9]+ *cm[.]\"," "\\1 various\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *(various *pagings) *\\(illus.\\)? [0-9]+ cm[.]\"," "\\1 various\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\) *(var[.] *pagings) *\\(illus.\\)? [0-9]+ cm[.]\"," "\\1 various\",")
  (internal-fix-oclc-pages "\\(^ *pages *= *\\\".*\\)\\([0-9]+ *v[.]\\) *\\([0-9]+ *cm[.]\\)\"," "\\1 various\",")
  )


(defun fix-oclc-script ()
  "Fix an OCLC search typescript created by oclc.sh.  This supplies
missing line breaks before vertical bars, and removes F (forward
page) characters at ends of lines beginning with vertical bars."
  (interactive)
  (goto-char (point-min))
  (fix-typescript)

  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)

    (goto-char (point-min))
    (message "Fixing vertical bars...")
    (while (re-search-forward "\\([^\n]\\)|" nil t)
      (replace-match  "\\1\n|" nil nil))

    (goto-char (point-min))
    (message "Flushing F commands...")
    (flush-lines "^F$")
    (goto-char (point-min))
    (flush-lines "RECORD NUMBER (or Action): F$")

    (goto-char (point-min))
    (message "Fixing misplaced F commands...")
    (while (re-search-forward "\\(^|.*\\)F\n\\([^|]\\)" nil t)
      (progn
	(replace-match "\\1\\2" nil nil)
	(forward-line -1)))

    ;; The above replacements can miss a few misplaced F characters,
    ;; so provide a way to handle them manually, sigh...
    (goto-char (point-min))
    (occur "^|.*[F]$" nil)
    (message "Fix any further errors via the *Occur* buffer...")
    (setq case-fold-search old-case-fold-search)))


(defun fix-old-style-bracing-2 ()
  "Convert old-style BibTeX braced title word d{Base} to new-style {dBase}."
  (interactive)
  (setq case-fold-search nil)
  (query-replace-regexp "\\([0-9a-zA-Z]+\\){\\([A-Za-z0-9\\-]+\\)}" "{\\1\\2}"))


(defun fix-old-style-bracing-3 ()
  "Convert old-style BibTeX braced title word {Base}-10 to new-style {Base-10}."
  (interactive)
  (setq case-fold-search nil)
  (query-replace-regexp "{\\([0-9a-zA-Z]+\\)}\\([A-Za-z0-9\\-]+\\)" "{\\1\\2}"))


(defun fix-one-month ()
  "For a journal that has monthly issues, set the month field from the
issue number field in the current entry, then move to the start of the
next entry, so the command can usefully be repeated."
  (interactive)
  (re-search-backward "^@")
  (let ((start (point)))
    (re-search-forward "^ *number *= *\"\\|^ *}" nil t)
    (if (looking-at "[0-9]+\",")
	(let ((number (string-to-number
		       (buffer-substring (point) (+ (point) 2))))
	      (monthnames ["jan" "feb" "mar" "apr" "may" "jun"
			   "jul" "aug" "sep" "oct" "nov" "dec"]))
	  (goto-char start)
	  (re-search-forward "^ *month *= *\\|^ *}" nil t)
	  (if (looking-at "\"[?]*\",")
	      (progn
		(kill-line nil)
		(insert (aref monthnames (1- number)) ",")
		(update-bibdate))
	    (beginning-of-line)
	    (if (looking-at " *}")
		(let ((end (point)))
		  (re-search-backward "^ *year *=" nil t)
		  (if (or (not (looking-at "^ *year *="))
			  (> start (point)))
		      (goto-char end))
		  (insert "  month =        "
			  (aref monthnames (1- number)) ",\n")
		  (update-bibdate))
	      (if (not (looking-at "^ *month *= *[a-z][a-z][a-z], *$"))
		  (error "Unrecognized month key/value")))))))
  (re-search-forward "^@\\|^ *}" nil t))


(defun fix-pages ()
  "Change --?? in a pages value to --."
  (interactive)
  (re-search-forward "pages =.*--[?][?]")
  (backward-char 2)
  (delete-char 2))


(defun fix-sciam-months ()
  "Find entries in sciam.bib with missing month data, and if the
volume and number are available, compute and supply the month."
  (interactive)
  (re-search-forward "^\\( *month *= *\\)\"[?]*\",")
  (let ((month-pos (match-end 1)) (beginning) (end) (volume) (number))
    (re-search-backward "^@Article")
    (setq beginning (point))
    (re-search-forward "^ *}")
    (setq end (point))
    (goto-char beginning)
    (if (re-search-forward "^ *volume *= *\"\\([0-9]+\\)\"," end t)
	(progn
	  (setq volume
		(string-to-int (buffer-substring (match-beginning 1) (match-end 1))))
	  (goto-char beginning)
	  (if (re-search-forward "^ *number *= *\"\\([0-9]+\\)\"," end t)
	      (progn
		(setq number
		      (string-to-int (buffer-substring
				      (match-beginning 1) (match-end 1))))
		(goto-char month-pos)
		;; (princ (format "volume = %d number = %d" volume number))
		;; (sit-for 3)
		(kill-line nil)
		(if (= (% volume 2) 0)
		    (insert (aref ["jan" "feb" "mar" "apr" "may" "jun"] (1- number)))
		  (insert (aref ["jul" "aug" "sep" "oct" "nov" "dec"] (1- number))))
		(insert ",")))))
    (goto-char end)))


(defun fix-siamjna-apr ()
  "Fix a BibTeX entry for the SIAM Journal on Numerical Analysis,
supplying a publication month from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"2\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "apr,")))))


(defun fix-siamjna-aug ()
  "Fix a BibTeX entry for the SIAM Journal on Numerical Analysis,
supplying a publication month from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"4\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "aug,")))))


(defun fix-siamjna-dec ()
  "Fix a BibTeX entry for the SIAM Journal on Numerical Analysis,
supplying a publication month from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"6\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "dec,")))))


(defun fix-siamjna-feb ()
  "Fix a BibTeX entry for the SIAM Journal on Numerical Analysis,
supplying a publication month from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"1\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "feb,")))))


(defun fix-siamjna-jun ()
  "Fix a BibTeX entry for the SIAM Journal on Numerical Analysis,
supplying a publication month from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"3\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "jun,")))))


(defun fix-siamjna-oct ()
  "Fix a BibTeX entry for the SIAM Journal on Numerical Analysis,
supplying a publication month from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"5\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "oct,")))))


(defun fix-siamjna-month-from-1982 ()
  "Supply a month name for entries in the siamjapplmath.bib file from
1982 to date, where the issue-number/month correspondences are constant.
Run biborder afterward to order key/value pairs and eliminate duplicates."
  (interactive)

  (goto-char (point-min))
  (replace-string
   "  number =       \"1\","
   "  number =       \"1\",\n  month =        feb,")

  (goto-char (point-min))
  (replace-string
   "  number =       \"2\","
   "  number =       \"2\",\n  month =        apr,")

  (goto-char (point-min))
  (replace-string
   "  number =       \"3\","
   "  number =       \"3\",\n  month =        jun,")

  (goto-char (point-min))
  (replace-string
   "  number =       \"4\","
   "  number =       \"4\",\n  month =        aug,")

  (goto-char (point-min))
  (replace-string
   "  number =       \"5\","
   "  number =       \"5\",\n  month =        oct,")

  (goto-char (point-min))
  (replace-string
   "  number =       \"6\","
   "  number =       \"6\",\n  month =        dec,"))


(defun fix-siamreview ()
  "Fix a BibTeX entry for the SIAM Review, reducing doubled braces to
single ones."
  (interactive)
  (let ((start))
    (while (re-search-forward "title *= *\"[^\"]*({{" nil t)
      (setq start (- (point) 2))
      (search-forward ")")
      (narrow-to-region start (point))
      (goto-char (point-min))
      (replace-regexp "[{}]" "")
      (goto-char (point-min))
      (insert "{")
      (goto-char (1- (point-max)))
      (insert "}")
      (widen))))


(defun fix-siamreview-dec ()
  "Fix a BibTeX entry for the SIAM Review, supplying a publication month
from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"4\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "dec,")))))


(defun fix-siamreview-jun ()
  "Fix a BibTeX entry for the SIAM Review, supplying a publication month
from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"2\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "jun,")))))


(defun fix-siamreview-mar ()
  "Fix a BibTeX entry for the SIAM Review, supplying a publication month
from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"1\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "mar,")))))


(defun fix-siamreview-sep ()
  "Fix a BibTeX entry for the SIAM Review, supplying a publication month
from the issue number."
  (interactive)
  (while (re-search-forward "^ *number *= *\"3\"," nil t)
    (re-search-forward "^ *month =*")
    (beginning-of-line)
    (if (looking-at "^ *month = *\"[?]+*\"")
	(progn
	  (beginning-of-line)
	  (search-forward "\"")
	  (backward-char 1)
	  (kill-line nil)
	  (insert "sep,")))))


(defun fixup-entry ()
  "Fix duplicate bibsource and keyword fields after a manual merger of
two BibTeX entries."
  (interactive)
  (let ((start))
    (save-excursion
      (save-restriction
	(re-search-backward "^@")
	(setq start (point))
	(re-search-forward "^}")
	(narrow-to-region start (point))
	(goto-char (point-min))
	(fix-duplicate-bibsource)
	(goto-char (point-min))
	(fix-duplicate-keywords))
	(goto-char (point-min))
	(if (re-search-forward "^ *bibsource *= *\"" nil t)
	    (bibtex-fill-value))
	(goto-char (point-min))
	(if (re-search-forward "^ *keywords *= *\"" nil t)
	    (bibtex-fill-value)))))


(defun fixup-oclc-parenthesized-author-names ()
  "Convert OCLC author names from
	Gorelik, A. M. (Alla Moiseevna)
to
       A. M. (Alla Moiseevna) Gorelik
This function should NEVER be run in automatic mode!"
  (interactive)
  (query-replace-regexp "\\([A-Za-z]+\\), *\\(\\([A-Z][.] *\\)+([A-Za-z. ]+)\\)"
			"\\2 \\1"))


(defun fixup-parenthesized-author-names ()
  "Convert OCLC author names from
	Gorelik, A. M. (Alla Moiseevna)
to
       A. M. (Alla Moiseevna) Gorelik
This function should NEVER be run in automatic mode!"
  (interactive)
  (query-replace-regexp "\\([A-Za-z]+\\), *\\(\\([A-Z][.] *\\)+([A-Za-z. ]+)\\)"
			"\\2 \\1"))


(defun fixup-parenthesized-author-names-2 ()
  "Convert OCLC author names from
	Gorelik, Alla M. (Alla Moiseevna)
to
       Alla M. (Alla Moiseevna) Gorelik
This function should NEVER be run in automatic mode!"
  (interactive)
  (query-replace-regexp "\\([A-Za-z]+\\), *\\(\\([A-Za-z]+\\) *\\([A-Z][.] *\\)+([A-Za-z. ]+)\\)"
			"\\2 \\1"))


(defun foo-1 ()
  "This is a catchall for various complicated operations.  It is
NOT intended to be executed, just to protect these function calls
from being executed if we do a load-file on this file."
  (occur "[:;.,!?]}\\|{[^}\n]*-\\|[a-zA-Z]- +[A-Za-z]\\|su[bp] ")

  (query-replace-regexp "\\(^ *author[^(\n]*\\)\\(([^)]*)\\)" "\\1")


  (query-replace-regexp "\\(^ *title[^{}\n]*\\)\\(Fortran\\)"
			"\\1{\\2}")
  (query-replace-regexp "pub_str\\[\"\\([^\"]*\\)\"\\][ 	]*=[ 	]*\\(.*\\)"
			"define(\\2,\"\\1\")" nil)

  (query-replace-regexp "pub_exp\\[\"\\([^\"]*\\)\"\\][ 	]*=[ 	]*\\(.*\\)"
			"define(\"\\1\",\\2)" nil)

  (query-replace-regexp "pub_adr\\[\"\\([^\"]*\\)\"\\][ 	]*=[ 	]*\\(.*\\)"
			"define(\"\\1:adr\",\\2)" nil)


  (query-replace-regexp "abbrev\\[\"\\([^\"]*\\)\"\\][ 	]*=[ 	]*\\(.*\\)"
			"define(\\2,\"\\1\")" nil)

  ;; actions to fixup concatenated month/day pairs:

  (query-replace-regexp
   "\\(month *= *[a-z][a-z][a-z]\\) *# *\"\\([^\"]*\\)\","
   "\\1,\n  day =          \"\\2\"," nil)

  (query-replace-regexp
   "\\(month *= *\\)\"\\([^\"]*\\)\" *# *\\([a-z][a-z][a-z]\\)"
   "\\1\\3,\n  day =          \"\\2\"," nil)

  (query-replace-regexp "\\(title *= *\"\\)\\([^\"]*\\)\"," "\\1{\\2}\"," nil)

  (query-replace-regexp "\\([a-zA-Z]\\)--\\([a-zA-Z]\\)" "\\1 --- \\2")

  (query-replace-regexp "\\([a-zA-Z]\\)-- " "\\1 --- ")

  (query-replace-regexp "\\([a-zA-Z]\\)---\\([a-zA-Z]\\)" "\\1 --- \\2")

  (query-replace-regexp "\\([a-zA-Z]\\)- \\([a-zA-Z]\\)" "\\1-\\2")

  (query-replace-regexp "}[ \n]+{" " ")

  )


(defun foo-2 ()
  "What does this do??"
  (interactive)
  (let ((n 0) (a))
    (while (re-search-forward "-> [0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\|free([0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f])"
			      nil t)
      (goto-char (match-beginning 0))
      (if (looking-at "-> ")
	  (forward-char 3)
	(forward-char 5))
      (setq a (buffer-substring (point) (+ (point) 5)))
      (beginning-of-line)
      (setq n (1+ n))
      (insert (format "xxxx %05d %s " n a))
      (forward-line))))


(defun foo-3 ()
  "What does this do??"
  (interactive)
  (let ((start) (tag) (prevtag ""))
    (while (re-search-forward "@Article{\\([^:]*\\):\\([^:]*\\):" nil t)
      (setq tag (buffer-substring (match-beginning 1) (match-end 2)))
      (if (string-equal tag prevtag)
	  (progn
	    (recenter 30)
	    (error "Match!")))
      (setq prevtag tag))))


(defun foo-4 ()
  "What does this do??"
  (interactive)
  (let ((start))
  (while (search-forward "define(\"j-" nil t)
    (setq start (point))
    (search-forward "\"")
    (upcase-region start (point)))))


(defun foo-5 ()
  "What does this do??"
  (interactive)
  (re-search-forward "Iv[^a-zA-Z]"))


(defun foo-6 ()
  "What does this do??"
  (interactive)
  (while (search-forward "=" nil t)
    (backward-delete-char 1)
    (delete-horizontal-space)
    (insert "\"")
    (while (> 48 (current-column))
      (insert "\t"))
    (end-of-line)
    (backward-char 1)
    (if (looking-at "}")
	(progn
	  (delete-char 1)
	  (insert ")")))))


(defun foo-7 ()
  "What does this do??"
  (interactive)
  (while (re-search-forward "\",\t*\"" nil t)
    (goto-char (+ 2 (match-beginning 0)))
    (delete-horizontal-space)
    (insert "\t")
    (while (> 48 (current-column))
      (insert "\t"))
    (end-of-line)
    (backward-char 1)))


(defun foo-8 ()
  "What does this do??"
  (interactive)
  (let ((previous "") (this))
    (catch 'DONE
      (while (re-search-forward "^ *define(\"j-[A-Z-]+" nil t)
	(setq this (buffer-substring (match-beginning 0) (match-end 0)))
	(if (string-equal previous this)
	    (throw 'DONE "Check"))
	(setq previous this)))))


;;; (defun get-string ()
;;;   "Extract a string around point, terminating at surrounding quotation
;;; marks, or HTML tags, and leave it in the kill ring after discarding
;;; any leading and trailing whitespace."
;;;   (interactive)
;;;   (let ((start (point)) (end-regexp) (first))
;;;     (re-search-backward ">[ \n\t]*\\|\"[ \n\t]*") ; find opening delimiter
;;;     (if (string-equal "\"" (buffer-substring (point) (1+ (point))))
;;; 	  (setq end-regexp "[ \n\t]*\"")
;;;       (setq end-regexp "[ \n\t]*</[^>]*>"))
;;;     (goto-char (match-end 0))
;;;     (setq first (point))
;;;     (re-search-forward  end-regexp) ; find closing delimiter
;;;     (copy-region-as-kill first (match-beginning 0))
;;;     (goto-char start)))


(defun get-string-2 ()
  "Extract a string around point, terminating at surrounding quotation
marks, or HTML tags, and return it after discarding any leading and
trailing whitespace."
  (interactive)
  (let ((start (point)) (end-regexp) (first) (last))
    (re-search-backward ">[ \n\t]*\\|\"[ \n\t]*") ; find opening delimiter
    (if (string-equal "\"" (buffer-substring (point) (1+ (point))))
	(setq end-regexp "[ \n\t]*\"")
      (setq end-regexp "[ \n\t]*</[^>]*>"))
    (goto-char (match-end 0))
    (setq first (point))
    (re-search-forward  end-regexp)	; find closing delimiter

    ;; Now convert any embedded newlines to space
    (goto-char first)
    (setq last (match-beginning 0))
    (while (search-forward "\n" last t)
      (replace-match " " nil t))
    (goto-char start)
    (buffer-substring first last)))


(defun how-long ()
  "Report the number of characters between mark and point."
  (interactive)
  (message "%d characters" (abs (- (point) (mark t)))))


(defun internal-fix-oclc-pages (old new)
  "Internal utility function for fix-oclc-pages."
  (goto-char (point-min))
  (query-replace-regexp old new nil))


(defun keep-book-entries ()
  "Delete non-book entries from point to end-of-buffer.  This is
useful for creating a books-only bibliography for bookstore/library
checking."
  (interactive)
  (setq case-fold-search t)
  (while (re-search-forward "^@" nil t)
    (cond
     ((looking-at "book") t)
     ((looking-at "string") t)
     ((looking-at "preamble") t)
     (t
      (mark-paragraph)
      (delete-region (point) (mark))))))


(defun kill-key-value-pair ()
  "Delete the BibTeX key/value pair in which point currently lies,
leaving it on the kill ring for possible later recovery."
  (interactive)
  (beginning-of-line)
  (if (or (looking-at "^ *[a-zA-Z]+ *= *\\(\"\\|[a-zA-Z]\\)")
	  (re-search-backward "^ *[a-zA-Z]+ *= *\\(\"\\|[a-zA-Z]\\)" nil t))
      (let ((start (point)))
	(if (re-search-forward "\", *\n" nil t)
	    (kill-region start (point))
	  (error "Cannot find end of key/value pair")))
    (error "No key/value pair found")))



;; Define some useful key bindings
(global-set-key [f10] 'get-string)
(global-set-key [A-SunF36] 'global-set-key)


(defun my-insert-register (char &optional arg)
  "Insert contents of register REG.  REG is a character.
Normally puts point after and mark before the inserted text, just as
if the text had been typed in manually.  If optional second arg is
non-nil, puts mark after and point before.  Interactively, second arg
is non-nil if prefix arg is supplied."
  (interactive "cInsert register: \nP")
  (push-mark)
  (let ((val (get-register char)))
    (cond
     ((consp val)
      (insert-rectangle val))
     ((stringp val)
      (insert val))
     ((integerp val)
      (princ val (current-buffer)))
     ((and (markerp val) (marker-position val))
      (princ (marker-position val) (current-buffer)))
     (t
      (error "Register does not contain text"))))
  (if arg (exchange-point-and-mark)))


(define-key ctl-x-map "g" 'my-insert-register)


(defun next-entry ()
  "Position to the next BibTeX entry, placing it at the top of the
screen."
  (interactive)
  (re-search-forward "^@.*,$")
  (recenter 2)
  (re-search-forward "^ *title *= *")
  (set-mark (point))
  (end-of-line)
)


(defun normalize-pages ()
  "Convert the BibTeX pages value in the current entry to the form xiv + 235."
  (interactive)
  (save-excursion
    (save-restriction
      (re-search-backward "^@")
      (if (and (re-search-forward "^}\\|^ *pages *= *\" *[ivxlcdm]+ *," nil t)
	       (string-equal (buffer-substring (1- (point)) (point)) ","))
	  (progn
	    (delete-char -1)
	    (delete-horizontal-space)
	    (insert " + "))))))


(defun repair-duplicate-label ()
  "With point in a BibTeX entry, find the citation label, and all
other definitions of the same label, and augment them with
single-letter suffixes.  Unlike fix-duplicate-labels, this function
does NOT require that the entries be in any particular order."
  (interactive)
  (let ((begin) (start) (label) (regexp) (n))
    (setq begin (point))
    (re-search-backward "^@" nil t)
    (if (looking-at "^@[A-Za-z]+{")
	(progn
	  (goto-char (match-end 0))
	  (setq start (point))
	  (end-of-line)
	  (delete-horizontal-space)
	  (setq label (buffer-substring start (point)))
	  (goto-char (point-min))
	  (setq regexp (concat "^@[A-Za-z]+{" label))
	  (setq n 0)
	  (while (and (< n 26) (re-search-forward regexp nil t))
	    (backward-char 1)
	    (insert-char (aref "abcdefghijklmnopqrstuvwxyz" n) 1)
	    (setq n (1+ n)))
	  (if (> n 25)
	      (error "More than 26 duplicate labels")))
      (error "Unrecognized citation label"))
    (goto-char begin)))


(defun show-greek-letters ()
  "Find lines containing Greek letter names that are not already TeX
macro names and show them in an occur-mode buffer."
  (interactive)
  (goto-char (point-min))
  (occur "[^\\\\a-zA-Z]\\(alpha\\|beta\\|gamma\\|delta\\|epsilon\\|zeta\\|eta\\|theta\\|iota\\|kappa\\|lambda\\|mu\\|nu\\|xi\\|omicron\\|pi\\|rho\\|sigma\\|tau\\|upsilon\\|phi\\|chi\\|psi\\|omega\\)[^a-zA-Z]"))


(defun supply-missing-pages ()
  "Find entries that are missing a pages value, and supply a dummy one."
  (interactive)
  (let ((start nil) (pages))
    (while (find-missing-pages)
      (setq start (point))
      (re-search-backward "^@")
      (if (looking-at "^@Article\\|^@In")
	  (setq pages "??--??")
	(setq pages "????"))
      (goto-char start)
      (forward-line -1)
      (insert (concat "  pages =        \"" pages "\",\n")))))


(defun supply-missing-volume ()
  "Supply a missing volume value for a journal that began publishing
yearly volumes in 1975. [Yes, this is for a specific journal, but I
forget which...]."
  (interactive)
  (let ((volume) (vpos))
    (while (re-search-forward "^ *volume *= *\"\"" nil t)
      (backward-char 1)
      (setq vpos (point))
      (re-search-forward "^ *year = *\"")
      (if (looking-at "19[0-9][0-9]")
	  (progn
	    (setq volume (- (string-to-int (buffer-substring (point) (+ 4
									(point)))) 1975))
	    (goto-char vpos)
	    (insert (format "%d" volume)))))))


(defun sysadmin ()
  "Convert a sysadmin.back article entry into BibTeX form,
assuming register 1 contains a template for this issue.
The frame must be in two-window mode, with point in the
sysadmin.back window."
  (interactive)
  (let ((start) (end) (next) (author) (title))
    (search-forward "  by ")
    (setq start (point))
    (re-search-forward "^[^ ]")
    (setq author (buffer-substring start (- (point) 2)))
    (setq next (point))
    (goto-char start)
    (backward-char 3)
    (backward-word 1)
    (forward-word 1)
    (setq end (point))
    (re-search-backward "^[^ ]")
    (setq title (buffer-substring (point) end))
    (recenter 0)
    (goto-char next)
    (other-window 1)
    (goto-char (point-max))
    (insert "\n")
    (insert-register ?1)
    (re-search-forward " *author *= *\"")
    (insert author)
    (bibtex-fill-value)
    (re-search-forward " *title *= *\"")
    (insert title)
    (bibtex-fill-value)
    (end-of-line)
    (delete-blank-lines)
    (bibtex-insert-standard-BibNet-citation-label)
    (goto-char (point-max))
    (insert "\n")
    (recenter -1)
    (setq fill-prefix "")
    (other-window 1)))


(defun uncover-search-cmd ()
  "With point in the current entry, make an UnCover database search
command in the kill ring."
  (interactive)
  (let ((str (get-string-2)) (start (point)) (cmd) (k))
    (beginning-of-line)
    (cond
     ((looking-at "^ *author *=\\|^ *editor *=")
      (setq k 0)
      (while (and (< k (- (length str) 5))
		  (not (string-equal
			(substring str k (+ 5 k)) " and ")))
	(setq k (1+ k)))
      (if (< k  (- (length str) 5))
	  (setq str (substring str 0 k)))
      (setq cmd (concat "//n " str "\n")))
     ((looking-at "^ *\\(book\\)?title *=")
      (setq cmd (concat "//w " str "\n")))
     (t (error "Cannot search UnCover for this string")))
    (x-set-selection 'PRIMARY cmd)
    (x-set-cut-buffer cmd)
    (goto-char start)))


(defun wrap-missing-coden-isbn-issn-lines ()
  "Wrap awk -f journal.awk -v PRINT_MISSING=1 foo.bib output lines for
library searching."
  (interactive)
  (while (< (point) (point-max))
    (forward-line 1)
    (end-of-line)
    (while (> (current-column) 100)
      (progn
	(backward-char (- (current-column) 100))
	(forward-word 1)
	(forward-word -1)
	(insert "\n\t\t\t\t\t\t\t")
	(end-of-line)))))


(defun year-coverage ()
  "Insert a year coverage report in a BibTeX bibliography file.  This
function requires an external awk program, year-coverage.awk, to
do the work.  Point should be on the line after which the report
is to be inserted, and the line must end with \"this:\"."
  (interactive)
  (end-of-line)
  (backward-char 5)
  (if (not (looking-at "this:$"))
      (error "Cannot find year-coverage report insertion point!"))
  (forward-line 1)
  (beginning-of-line)
  (let ((start (point)))
    (if (search-forward "%%%                             Total entries:" nil t)
	(progn
	  (forward-line 1)
	  (if (looking-at "^%%% *$")
	      (kill-line 1))
	  (kill-region start (point)))))
  (call-process-region (point-min) (point-max)
		       "/bin/sh"
		       nil
		       (buffer-name)
		       nil
		       "-c" "gawk -f /u/sy/beebe/tex/bib/year-coverage.awk"))
