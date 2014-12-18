;;; /u/sy/beebe/emacs/bibtex-mods.el, Sat Jun 30 11:35:57 2012
;;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Add DOI to mandatory fields in Article and InProceedings entries.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Mon Mar  5 07:24:22 2007
;;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Add URL to optional fields in several entries.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Wed Nov  6 18:22:36 2002
;;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Tighten pattern match in update-bibdate.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Mon Feb 17 07:05:22 1997
;;; Edit by  <beebe@sunrise.math.utah.edu>
;;; Add (provide 'bibtex-mods), and use require instead of load.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Thu May 25 22:09:29 1995
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Add bibtex-default-acknowledgement, and generate acknowledgement
;;; entry as a required field in several entries.
;;; Change publisher address from optional to required, to avoid 
;;; the bother of having to remove the OPT prefix.
;;; Include default currency in price fields.
;;; Modify bibtex-remove-OPT to leave point inside value string, 
;;; instead of before it.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Fri Mar 24 16:06:39 1995
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; Update bibtex-fix-address to run bibtex-remove-OPT
;;; /u/sy/beebe/emacs/bibtex-mods.el, Wed Aug 10 20:46:02 1994
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Update bibtex-fix-address to recognize organization = org-XYZ
;;; /u/sy/beebe/emacs/bibtex-mods.el, Wed Aug 10 17:03:33 1994
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Add make-new-style-tag and make-old-style-tag
;;; /u/sy/beebe/emacs/bibtex-mods.el, Sun Jul 24 13:33:04 1994
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Make update-bibdate start at beginning of current entry, instead
;;; of at current point.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Sat Jul 16 10:59:24 1994
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Extend bibtex-fix-address to handle institutions as well
;;; /u/sy/beebe/emacs/bibtex-mods.el, Thu Jul 14 17:43:43 1994
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; Add LCCN to bibtex-Proceedings
;;; /u/sy/beebe/emacs/bibtex-mods.el, Fri Jul  8 08:11:19 1994
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; Add bibtex-fix-address
;;; /u/sy/beebe/emacs/bibtex-mods.el, Thu Jul  7 16:35:13 1994
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; Add -keep-parbreaks to bibclean-options, and replace space
;;; before one-digit day by zero in update-bibdate.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Sun Mar 27 16:52:11 1994
;;; Edit by Nelson H. F. Beebe <beebe@solitude.math.utah.edu>
;;; Fix error in bibtex-InProceedings
;;; /u/sy/beebe/emacs/bibtex-mods.el, Mon Nov 15 14:09:29 1993
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Add acknowledgement, price, and LCCN keywords for book-type entries
;;; /u/sy/beebe/emacs/bibtex-mods.el, Mon Jun 14 18:41:16 1993
;;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;;; Update for Emacs 19, maintaining compatibility with Emacs 18 too.
;;; /u/sy/beebe/emacs/bibtex-mods.el, Fri Apr  2 10:32:20 1993
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; Move defconsts to start, and add explicit load of bibtex
;;; /u/sy/beebe/emacs/bibtex-mods.el, Mon Jan 18 10:03:05 1993
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; /u/sy/beebe/emacs/bibtex-mods.el, Sat Nov  7 11:46:09 1992
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;; /u/sy/beebe/emacs/bibtex-mods.el, Wed Oct 28 11:49:18 1992
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;;
;;; This file contains private customizations of some functions from
;;; nbibtex.el, and adds the new functions bibclean-entry on C-c C-c
;;; bibclean-region on C-c C-r, and bibsort-region on C-c C-s.
;;;
;;; When the GNU Emacs regexp code finally works properly, this code
;;; should be moved into new-bibtex.el.
;;;

(provide 'bibtex-mods)

(if (string-equal (substring emacs-version 0 2) "19")
    (require 'bibtex)
  (load-library "bibtex"))

(defconst bibclean-options
  "-delete-empty -remove-OPT-prefixes -keep-parbreaks"
  "*Command-line switches to pass to bibclean.")

(defconst bibtex-default-acknowledgement
  'ack-nhfb
  "*Default acknowledgement value.")

;; Lengthen bibtex-text-alignment by 1 char to match bibclean's output
(defconst bibtex-name-alignment 2
  "Alignment for the name part in BibTeX fields.
Chosen on aesthetic grounds only.")

(defconst bibtex-text-alignment (length "  organization = ")
  "Alignment for the text part in BibTeX fields.
Equal to the space needed for the longest name part.")

;;; These are new functions to invoke bibclean on an entry or region,
;;; and bibsort on a region.

(defun bibclean-entry ()
  "Run bibclean on the current (preceding, if outside) BibTeX entry.
If bibclean finds errors, they will appear in the buffer in place of
the entry.  Just use undo (on \\[undo]) to restore the entry, fix the
errors, and try again."
  (interactive)
  (save-excursion
    (bibtex-move-outside-of-entry)	;move past current entry
    (push-mark nil t)			;mark start of region
    (re-search-backward "^@")
    ;; old (version 1.x):
    ;; (bibtex-filter-region "bibclean")
    ;; new (version 2.0):
    (bibtex-filter-region (concat "bibclean " bibclean-options))
    (search-forward "\n}\n")
    (delete-blank-lines)
    (newline)))

(defun bibclean-region ()
  "Run bibclean on the current region.  If bibclean finds errors, they
will appear in the buffer in place of the original text.  Just use
undo (on \\[undo]) to restore the region, fix the errors, and try
again."
  (interactive)
    ;; old (version 1.x):
    ;; (bibtex-filter-region "bibclean")
    ;; new (version 2.0):
    (bibtex-filter-region (concat "bibclean " bibclean-options)))

(defun bibsort-region ()
  "Run bibsort on the current region.  If bibsort finds errors, they
will appear in the buffer in place of the original text.  Just use
undo (on \\[undo]) to restore the region, fix the errors, and try
again."
  (interactive)
  (bibtex-filter-region "bibsort"))

(defun bibtex-filter-region (filter)
  "Run a BibTeX filter on the current region.  If the filter finds
errors, they will appear in the buffer in place of the filtered text.
Just use undo (on \\[undo]) to restore the region, fix the errors, and
try again."
  (interactive)
  (save-excursion
    (let ((start (region-beginning)) (end (region-end)))
      (message "Invoking %s on region beginning %s...please wait"
	       filter
	       (buffer-substring (progn
				   (goto-char start)
				   (search-forward "@")
				   (1- (point)))
				 (progn
				   (goto-char start)
				   (search-forward ",")
				   (1- (point)))))
      (shell-command-on-region start end filter 1 1)))
  (message "[done]"))

(defun bibtex-fix-address ()
  "With point inside a partially-complete BibTeX entry with the
publisher already supplied as pub-XYZ, or institution or school as
inst-XYZ, fill in the OPTaddress field with pub-XYZ:adr or inst-XYZ:adr." 
  (interactive)
  (let ((entry-end) (entry-start) (publisher) (start))
    (save-excursion
      (re-search-backward "^@")
      (setq entry-start (point))
      (re-search-forward "^}")
      (setq entry-end (point))
      (goto-char entry-start)
      (re-search-forward 
       "^ *\\(OPT\\)?\\(organization\\|school\\|publisher\\|institution\\) *= *" entry-end)
      (setq start (point))
      (search-forward "," entry-end)
      (setq publisher (buffer-substring start (1- (point))))
      (goto-char entry-start)
      (re-search-forward "^ *\\(OPT\\)?address *= *\"\\([?]*\\)\"" entry-end)
      (delete-region (match-beginning 2) (match-end 2))
      (backward-delete-char 2)
      (insert publisher ":adr")
      (back-to-indentation)
      (bibtex-remove-OPT))))


;;; Fix shell-command-on-region from /usr/local/emacs/lisp/simple.el
;;; to do (push-mark nil t) instead of (push-mark) so we don't get the
;;; "Mark set" message that wipes out our previous message.
;;; This problem exists in both Emacs 18 and 19, but shell-command-on-region
;;; differs between the two versions, so we check for both here.

(if (string-equal (substring emacs-version 0 2) "18")
    (defun shell-command-on-region (start end command &optional flag interactive)
      "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer;
Prefix arg means replace the region with it.
Noninteractive args are START, END, COMMAND, FLAG.
Noninteractively FLAG means insert output in place of text from START to END,
and put point at the end, but don't alter the mark."
      (interactive "r\nsShell command on region: \nP\np")
      (if flag
	  ;; Replace specified region with output from command.
	  (let ((swap (and interactive (< (point) (mark)))))
	    ;; Don't muck with mark
	    ;; unless called interactively.
	    ;; NHFB: [28-Oct-1992] add t argument to push-mark
	    (and interactive (push-mark nil t))
	    (call-process-region start end shell-file-name t t nil
				 "-c" command)
	    (and interactive swap (exchange-point-and-mark)))
	(let ((buffer (get-buffer-create "*Shell Command Output*")))
	  (if (eq buffer (current-buffer))
	      ;; If the input is the same buffer as the output,
	      ;; delete everything but the specified region,
	      ;; then replace that region with the output.
	      (progn (delete-region end (point-max))
		     (delete-region (point-min) start)
		     (call-process-region (point-min) (point-max)
					  shell-file-name t t nil
					  "-c" command))
	    ;; Clear the output buffer, then run the command with output there.
	    (save-excursion
	      (set-buffer buffer)
	      (erase-buffer))
	    (call-process-region start end shell-file-name
				 nil buffer nil
				 "-c" command))
	  (if (save-excursion
		(set-buffer buffer)
		(> (buffer-size) 0))
	      (set-window-start (display-buffer buffer) 1)
	    (message "(Shell command completed with no output)"))))))

(if (string-equal (substring emacs-version 0 2) "19")
    (defun shell-command-on-region (start end command &optional flag interactive)
      "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.
Noninteractive args are START, END, COMMAND, FLAG.
Noninteractively FLAG means insert output in place of text from START to END,
and put point at the end, but don't alter the mark.

If the output is one line, it is displayed in the echo area,
but it is nonetheless available in buffer `*Shell Command Output*'
even though that buffer is not automatically displayed.  If there is no output
or output is inserted in the current buffer then `*Shell Command Output*' is
deleted."
      (interactive (list (region-beginning) (region-end)
			 (read-string "Shell command on region: "
				      last-shell-command-on-region)
			 current-prefix-arg
			 (prefix-numeric-value current-prefix-arg)))
      (if flag
	  ;; Replace specified region with output from command.
	  (let ((swap (and interactive (< (point) (mark)))))
	    ;; Don't muck with mark
	    ;; unless called interactively.
	    ;; NHFB: [28-Oct-1992] add t argument to push-mark
	    (and interactive (push-mark nil t))
	    (call-process-region start end shell-file-name t t nil
				 "-c" command)
	    (if (get-buffer "*Shell Command Output*")
		(kill-buffer "*Shell Command Output*"))
	    (and interactive swap (exchange-point-and-mark)))
	;; No prefix argument: put the output in a temp buffer,
	;; replacing its entire contents.
	(let ((buffer (get-buffer-create "*Shell Command Output*")))
	  (if (eq buffer (current-buffer))
	      ;; If the input is the same buffer as the output,
	      ;; delete everything but the specified region,
	      ;; then replace that region with the output.
	      (progn (delete-region end (point-max))
		     (delete-region (point-min) start)
		     (call-process-region (point-min) (point-max)
					  shell-file-name t t nil
					  "-c" command))
	    ;; Clear the output buffer, then run the command with output there.
	    (save-excursion
	      (set-buffer buffer)
	      (erase-buffer))
	    (call-process-region start end shell-file-name
				 nil buffer nil
				 "-c" command))
	  ;; Report the amount of output.
	  (let ((lines (save-excursion
			 (set-buffer buffer)
			 (if (= (buffer-size) 0)
			     0
			   (count-lines (point-min) (point-max))))))
	    (cond ((= lines 0)
		   (message "(Shell command completed with no output)")
		   (kill-buffer "*Shell Command Output*"))
		  ((= lines 1)
		   (message "%s"
			    (save-excursion
			      (set-buffer buffer)
			      (goto-char (point-min))
			      (buffer-substring (point)
						(progn (end-of-line) (point))))))
		  (t
		   (set-window-start (display-buffer buffer) 1))))))))

;;; I always want ISBN and LCCN fields in books, so put them in the required
;;; list so the OPT prefix does not need to be stripped.
;;; I want the address field for publishers to be required, so move it
;;; out of the optional list.
;;; I also want optional pages and price fields for books, and for
;;; convenience, I want a default currency in the price value.


(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article" '("author" "title" "journal" "year" "DOI" "URL")
		'("volume" "number" "pages" "month" "note" "keyword" "annote")))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book"
		(` ("author" "title" "publisher" "address" "year" "ISBN" "LCCN"
		     ("acknowledgement" . (, bibtex-default-acknowledgement))))
		'( "editor" "volume" "number" "series" 
		   "edition" "month" "note" "pages" ("price" . "US\\$"))))

(defun bibtex-InBook ()
  (interactive)
  (bibtex-entry "InBook"
		(` ("author" "title" "chapter" "publisher" "address" "year" 
		    "ISBN" "LCCN"
		    ("acknowledgement" . (, bibtex-default-acknowledgement))))
		'( "editor" "volume" "number" "series" 
		   "edition" "month" "type" "note" "pages" ("price" . "US\\$"))))

(defun bibtex-InCollection ()
  (interactive)
  (bibtex-entry "InCollection"
		(` ("author" "title" "booktitle" "publisher" "address" "year" 
		    "ISBN" "LCCN"
		    ("acknowledgement" . (, bibtex-default-acknowledgement))))
		'( "editor" "volume" "number" "series" "type" "chapter"
		   "edition" "month" "note" "pages" ("price" . "US\\$"))))


(defun bibtex-InProceedings ()
  (interactive)
  (bibtex-entry "InProceedings"
		(` ("author" "title" "booktitle" "year" "DOI" "ISBN" "LCCN" "URL" 
		    ("acknowledgement" . (, bibtex-default-acknowledgement))))
		'( "editor" "volume" "number" "series" "organization"
		   "publisher" "address" "month" "note" "pages" 
		   ("price" . "US\\$"))))

;;; Periodical is a new entry type added by NHFB

(defun bibtex-Periodical ()
  "Insert a periodical (i.e. annual, magazine, journal, monthly,
newspaper, quarterly, ...) entry.  Its use requires extended BibTeX
style files, is-*.bst, available in the TeX Users Group library, tuglib,
at math.utah.edu.  This entry type will be supported by all styles in
BibTeX 1.0."
  (interactive)
  (bibtex-entry "Periodical"
		'("key" "title")
		'("editor" "organization" "volume" "number" "pages"
		  "month" "year" "ISSN" "publisher" "address"
		  "howpublished" "note" "keyword")))

(defun bibtex-Proceedings ()
  (interactive)
  (bibtex-entry "Proceedings"
		(` ("title" "year" "ISBN" "LCCN"
		     ("acknowledgement" . (, bibtex-default-acknowledgement))))
		'( "editor" "volume" "number" "series" "publisher"
		   "organization" "address" "month" "note" "pages" )))

(if (string-equal (substring emacs-version 0 2) "19")
    (define-key bibtex-mode-map [menu-bar entry-types bibtex-Periodical]
      '("            Periodical             " . bibtex-Periodical)))

;;; C-c C-e followed by P, p, and C-p are already taken, so use j (journal)
(define-key bibtex-mode-map "\C-c\C-ej" 'bibtex-Periodical)

;;; Replace the normal binding to bibtex-clean-entry, because it
;;; doesn't work if strings contain embedded quotes (e.g. umlauts)
;;; and add a new binding for bibclean-region and bibsort-region.
(define-key bibtex-mode-map "\C-c\C-c" 'bibclean-entry)
(define-key bibtex-mode-map "\C-c\C-r" 'bibclean-region)
(define-key bibtex-mode-map "\C-c\C-s" 'bibsort-region)

(if (string-equal (substring emacs-version 0 2) "19")
    (defun bibtex-entry (entry-type &optional required optional)
      (interactive
       (let* ((completion-ignore-case t)
	      (e-t (completing-read "Entry Type: " bibtex-entry-field-alist
				    nil t)))
	 (list e-t)))
      (if (and (null required) (null optional))
	  (let* ((e (assoc-string-equalp entry-type bibtex-entry-field-alist))
		 (r-n-o (elt e 1))
		 (c-ref (elt e 2)))
	    (if (null e)
		(error "Bibtex entry type %s not defined!"))
	    (if (and bibtex-include-OPTcrossref c-ref)
		(setq required (elt c-ref 0)
		      optional (elt c-ref 1))
	      (setq required (elt r-n-o 0)
		    optional (elt r-n-o 1)))))
      (let ((key (if bibtex-maintain-sorted-entries
		     (read-string (format "%s key: " entry-type)))))
	(if key
	    (find-bibtex-entry-location key))
	(bibtex-move-outside-of-entry)
	(insert "@" entry-type "{")
	(if key
	    (insert key))
	(save-excursion
	  (mapcar 'bibtex-make-field required)
	  (if bibtex-include-OPTcrossref
	      (bibtex-make-optional-field "crossref"))
	  (if bibtex-include-OPTkey
	      (bibtex-make-optional-field "key"))
	  (mapcar 'bibtex-make-optional-field optional)
	  (mapcar 'bibtex-make-optional-field
		  bibtex-mode-user-optional-fields)
	  (if bibtex-include-OPTannote
	      (bibtex-make-optional-field "annote"))
	  (bibtex-make-field "bibdate")
	  (insert "\n}\n\n")
	  (backward-char 5)
	  (insert (current-time-string))
	  (forward-char 1)
	  (insert ",")
	  (backward-char 18)
	  (if (looking-at " [0-9]")
	      (progn
		(delete-char 1)
		(insert-char ?0 1))
	    (forward-line 1)))
	(if key
	    (bibtex-next-field t))
	(run-hooks 'bibtex-add-entry-hook))))

;;; Replace bibtex-make-*-entry from bibtex.el with similar functions
;;; from nbibtex.el (but with changed names).

(if (string-equal (substring emacs-version 0 2) "18")
    (progn
      (defun bibtex-make-entry (str)
	(interactive "sBibTeX entry type: ")
	(indent-to-column bibtex-name-alignment)
	(insert str " = ")
	(indent-to-column bibtex-text-alignment)
	(insert "\"\",\n")
	nil)

      (defun bibtex-make-OPT-entry (str)
	(interactive "sOptional BibTeX entry type: ")
	(indent-to-column bibtex-name-alignment)
	(insert "OPT" str " = ")
	(indent-to-column bibtex-text-alignment)
	(insert "\"\",\n")
	nil)

      (defun bibtex-entry (entry-type required optional)
	(bibtex-move-outside-of-entry)
	(insert (concat "@" entry-type "{,\n}\n\n"))
	(previous-line 2)
	(insert (mapconcat 'bibtex-make-entry required ""))
	(insert (mapconcat 'bibtex-make-OPT-entry optional ""))
	(bibtex-make-entry "bibdate")
	(backward-char 3)
	(insert (current-time-string))
	(up-list -1)
	(forward-char 1))))

(defun extended-bibtex-find-text (arg)
  "Go to end of text of current field; with arg, go to beginning.
However, if point is already at the end of the field, go to the next
one."
  (interactive "P")
  (if (looking-at "\",$\\|,$")
     (forward-line 1))
  (bibtex-find-text arg))

(defun make-new-style-tag ()
  "Convert an old-style BibTeX citation tag, Lastname:ABCyy, to BibNet
BibTeX tag form, Lastname:19yy:ABC.  Point must be on a line beginning
with a new BibTeX entry, @xxx{...}."
  (interactive)
  (let ((newtag))
    (beginning-of-line)
    (if (looking-at 
	 "\\(@[A-Za-z]+{[A-Za-z]+:\\)\\([A-Za-z]+\\)\\([0-9][0-9]\\)\\([a-z]?\\)")
	(progn
	  (princ (buffer-substring (match-beginning 0) (match-end 0)))
	  (setq newtag 
		(concat
		 (buffer-substring (match-beginning 1) (match-end 1))
		 "19"
		 (buffer-substring (match-beginning 3) (match-end 3))
		 ":"
		 (buffer-substring (match-beginning 2) (match-end 2))
		 (buffer-substring (match-beginning 4) (match-end 4))))
	  (message "Old: [%s] New: [%s]"
		   (buffer-substring (match-beginning 0) (match-end 0))
		   newtag)
          (delete-region (match-beginning 0) (match-end 0))
	  (insert newtag))
      nil)))

(defun make-old-style-tag ()
  "Convert a new-style BibNet BibTeX citation tag, Lastname:19yy:ABC, to 
the old style BibTeX tag form, Lastname:ABCyy.  Point must be on a
line beginning with a new BibTeX entry, @xxx{...}."
  (interactive)
  (let ((oldtag))
    (beginning-of-line)
    (if (looking-at 
	 "\\(@[A-Za-z]+{[A-Za-z]+:\\)\\([12][0-9][0-9][0-9]\\):\\([A-Z]+\\)\\([a-z]?\\)")
	(progn
	  (princ (buffer-substring (match-beginning 0) (match-end 0)))
	  (setq oldtag 
		(concat
		 (buffer-substring (match-beginning 1) (match-end 1))
		 (buffer-substring (match-beginning 3) (match-end 3))
		 (buffer-substring (+ 2 (match-beginning 2)) (match-end 2))
		 (buffer-substring (match-beginning 4) (match-end 4))))
	  (message "Old: [%s] New: [%s]"
		   (buffer-substring (match-beginning 0) (match-end 0))
		   oldtag)
          (delete-region (match-beginning 0) (match-end 0))
	  (insert oldtag))
      nil)))

(defun update-bibdate ()
  "Find the bibdate key/value pair in the current .bib file entry and
update it to the current date.  If none is found, add one.  We replace
a space by a leading zero on one-digit day numbers, so as to keep the
spacing consistent with the output of bibclean."
  (interactive)
  (re-search-backward "^@")
  (re-search-forward "^}\\|^ *bibdate *= *\"")
  (if (string-equal (buffer-substring (match-beginning 0) (match-end 0)) "}")
      (progn
	(beginning-of-line)
	(insert "  bibdate =      \"" (current-time-string) "\",\n"))
    (kill-line)
    (insert  (current-time-string) "\",")
    (backward-char 18)
    (if (looking-at " [0-9]")
	(progn
	  (delete-char 1)
	  (insert-char ?0 1))
      (forward-line 1))))

; Add new menu-based accent support under Emacs 19

(if (not (string-lessp (substring emacs-version 0 2) "19"))
    (progn
      (require 'ltxmenu)
      (require 'ltxaccnt)
      (require 'btxaccnt)))

; Modify bibtex-remove-OPT to position inside value string, instead of
; before it.
(defun bibtex-remove-OPT ()
  "Removes the 'OPT' starting optional arguments and goes to end of text"
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (save-excursion
    (goto-char (match-beginning bibtex-name-in-field))
    (if (looking-at "OPT")
	;; sct@dcs.edinburgh.ac.uk
 	(progn
 	  (delete-char (length "OPT"))
 	  (search-forward "=")
 	  (delete-horizontal-space)
 	  (indent-to-column bibtex-text-alignment))))
  ;; (bibtex-inside-field)		;this puts point before field
  (beginning-of-line)			;these two put point inside the
  (extended-bibtex-find-text nil)	;(empty) field
)
