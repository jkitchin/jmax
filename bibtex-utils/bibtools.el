;;; -*-Emacs-Lisp-*-
;;; http://ftp.math.utah.edu/pub/emacs/bibtools.el
;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.90",
;;;     date            = "25 May 2014,
;;;     time            = "07:48:44 MDT",
;;;     filename        = "bibtools.el",
;;;     address         = "University of Utah
;;;                        Department of Mathematics, 110 LCB
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 581 4148",
;;;     checksum        = "64068 3981 15441 133969",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@computer.org (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "bibliography, BibTeX, editing support, GNU Emacs",
;;;     license         = "GNU General Public License",
;;;     supported       = "yes",
;;;     docstring       = "This file contains a collection of GNU Emacs
;;;                        functions developed by the above author for
;;;                        the support of editing of BibTeX
;;;                        bibliographies.  Loading this file will
;;;                        silently load a few other other files
;;;                        containing specialized support packages.
;;;
;;;                        While standard BibTeX mode provides for
;;;                        easy CREATION of bibliography entries, it
;;;                        provides little in the way of support for
;;;                        bibliography MAINTENANCE and UPDATE work,
;;;                        including
;;;
;;;                        * conversion of library catalog dumps to
;;;                          BibTeX entries,
;;;                        * merging of entries from another
;;;                          bibliography,
;;;                        * finding missing key/value pairs in
;;;                          entries,
;;;                        * bracing of selected title words (proper
;;;                          names, German nouns, ...) to protect
;;;                          against downcasing in some bibliography
;;;                          styles,
;;;                        * completing page-number ranges in journal
;;;                          bibliographies sorted in publication order,
;;;                        * generating BibNet Project citation
;;;                          labels, and
;;;                        * syntax checking.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

(defconst bibtools-version "1.90 [25-May-2014]"
   "Version number of the bibtools library, a collection of useful
BibTeX editing functions.")

;;; Revision history (reverse chronological order):
;;;
;;; 1.90 [25-May-2014]
;;;	Add Czech ring accent to tests in bibtex-isaccentletter.
;;;
;;; 1.89 [17-Apr-2013]
;;;	Extend bibtex-first-author-or-editor-name,
;;;	bibtex-normalize-author-editor-names, and bibtex-title-abbrev
;;;	to recognize OPT prefix on author, editor, and title values,
;;;	as a convenience during editing of newly-created entries using
;;;	emacs templates for BibTeX entries.
;;;
;;;	Add new function bibtex-replace-string to avoid byte-compile
;;;	warnings about use of interactive replace-string function in
;;;	Emacs Lisp code.
;;;
;;;	[Ignore byte-compile complaints about obsolete string-to-int
;;;	function, which did not exist before emacs-20.x; it is now
;;;	just an alias for string-to-number.]
;;;
;;; 1.88 [02-Feb-2013]
;;;	Remove slash from regexp in bibtex-normalize-author-editor-names
;;;	that defines characters that separate author names.  That
;;;	slash resulted in incorrect reduction of a corporate name
;;;	like "{ISO/IEC}" to "{ISO and IEC}".
;;;
;;; 1.87 [13-Jul-2012]
;;;	Tweak one regular expression in bibtex-fill-value to recognize
;;;	hyphens and digits in BibTeX field names.
;;;
;;; 1.86 [12-May-2012]
;;;	Revise sub and gsub to force case-sensitive matching.  That
;;;	removes a bug that would cause a name "Able Baker Xi" to be
;;;	reduced to "Baker", because "Xi" would be matched as if it
;;;	were the roman number "XI", and thus, stripped from the name.
;;;
;;; 1.85 [16-Mar-2012]
;;;	Revise bibtex-first-author-name to strip Jr. and Sr. suffixes,
;;;     independent of any surrounding braces, and whether or not the
;;;     suffix is preceded by a comma.
;;;
;;; 1.84 [31-Mar-2011]
;;;	Add bibtex-validate-citation-labels.
;;;
;;; 1.83 [12-Nov-2010]
;;;	Extend bibtex-standard-BibNet-citation-label to recognize year
;;;	values that begin with a four-digit year, followed by other text,
;;;     such as "2009/2010", because some journals assign multiple years
;;;     to a single issue.
;;;
;;; 1.82 [13-Jan-2009]
;;;	Change one character escape (question-backslash-caret) to octal
;;;	to satisfy emacs-23.
;;;
;;; 1.81 [07-Apr-2006]
;;;	Extend bibtex-fill-value so that a final comma after the
;;;	string is not required (safety feature).
;;;
;;; 1.80 [15-Jul-2005]
;;;	Update fix-old-style-bracing with another conversion pattern
;;;	to, e.g., convert Qo{S} to {QoS}.
;;;
;;; 1.79 [09-Jun-2005]
;;;	Add find-missing-ISBN-13.
;;;
;;; 1.78 [01-Dec-2003]
;;;	Add find-missing-DOI.
;;;
;;; 1.77 [17-Nov-2002]
;;;	Extend bibtex-normalize-author-editor-names to remove
;;;	duplicate and trailing "and" name separators.
;;;
;;; 1.76 [01-May-2002]
;;;	Add check in bibtex-normalize-author-editor-names that only an
;;;	author or editor string is being processed.
;;;
;;; 1.75 [23-Apr-2002]
;;;	Wrap ISBN/ISSN extract code inside (with-syntax-table ...) in
;;;	case hyphens are not word characters.
;;;
;;; 1.74 [25-Aug-2001]
;;;	Rewrite find-binary-character to behave more like isearch-forward.
;;;
;;; 1.73 [20-Apr-2001]
;;;	Update bibtex-standard-BibNet-citation-label to accept an OPTyear
;;;	key/value if a year key/value is missing, and if both are missing
;;;	to produce a clearer error message.  If the search for the start
;;;	of the BibTeX entry fails, issue a better error message than the
;;;	cryptic re-search-backward failing-search message.
;;;
;;; 1.72 [10-Feb-2001]
;;;	Update convert-author-editor-commas-to-ands to remove leading
;;;	and trailing commas, and reduce multiple adjacent commas to a
;;;	a single one, and then, on completion, to invoke
;;;	bibtex-normalize-author-editor-names and bibtex-fill-value to
;;;	clean up the entry.
;;;
;;; 1.71 [14-Nov-2000]
;;;	Make small change in regexp in
;;;	bibtex-normalize-author-editor-names to avoid substituting a
;;;	space after "\."; only do this if the "." is preceded by a
;;;	letter.  This fixes a bug wherein "Lech Papie{\.z}" was reduced
;;;	to "Lech Papie{\. z}", later producing an incorrect citation
;;;	label of the form "z:20xx:ABC", instead of the expected
;;;	"Papiez:20xx:ABC".
;;;
;;; 1.70 [16-Oct-1999]
;;;	Add new function bibtex-find-next-empty-string, with a
;;;	recommended binding (in window systems only) to Ctl-TAB, and
;;;	to M-TAB, by analogy with the binding of TAB to the related
;;;	functions bibtex-find-text or extended-bibtex-find-text.
;;;
;;;	Repair typos in the docstrings of bibtex-backward-brace-words
;;;	and bibtex-forward-brace-words.
;;;
;;; 1.69 [05-Oct-1999]
;;;	Extend bibtex-interpolate-page-numbers to ensure that it never
;;;	generates a final page number that is less than the initial one.
;;;
;;; 1.68 [04-Oct-1999]
;;;	Extend bibtex-fill-value to remove leading space.
;;;
;;; 1.67 [22-Sep-1999]
;;;	Extend bibtex-fill-value to remove space before colon and
;;;	semicolon, because such grammatically incorrect uses are
;;;	common in library databases and catalogs.
;;;
;;; 1.66 [17-Jul-1999]
;;;	Extend bibtex-title-abbrev to strip \emph, \hbox, \mbox, and
;;;	\vbox.
;;;
;;; 1.65 [14-Jul-1999]
;;;	Revise bibtex-first-author-name to strip small roman numeral
;;;	qualifiers on last names: "Baker, III" -> "Baker" and "Baker
;;;	III" -> "Baker".
;;;
;;; 1.64 [29-May-1999]
;;;	Move bibtex-fill-value and internal-month-number into this file
;;;	from bibtex-misc.el so that that file does not require this one.
;;;
;;;	Change remaining (replace-regexp...) calls with silent
;;;	equivalent, (bibtex-replace-regexp...).
;;;
;;;	Extend bibtex-first-author-name to include lowercase von-like
;;;	parts of names in the returned value, since we have found those
;;;	parts to be preferred in citation labels, and braced compound
;;;	names are not always present; a bell and warning is issued when
;;;	such a name is encountered.
;;;
;;; 1.63 [24-May-1999]
;;;	Extend bibtex-standard-BibNet-citation-label to permit
;;;	x as well as 0-9 in final two digits of years.
;;;
;;; 1.62 [24-Apr-1999]
;;;	Make bibtex-first-author-name call bibtex-fill-value
;;;	before returning, because its prior call to
;;;	bibtex-normalize-author-editor-names converted newlines
;;;	to spaces.  This produces more reasonable behavior when
;;;	bibtex-insert-standard-BibNet-citation-label is invoked
;;;	to generate a citation label.
;;;
;;; 1.61 [20-Apr-1999]
;;;	Extend convert-author-editor-commas-to-ands to handle "Sr."
;;;	suffix.
;;;
;;; 1.60 [16-Apr-1999]
;;;	Add convert-author-editor-commas-to-ands.
;;;
;;; 1.59 [13-Apr-1999]
;;;	Update bibtex-normalize-one-author-editor-name to supply
;;;	periods that are missing after initials, since some databases
;;;	lack them, and they are tedious to insert manually.
;;;
;;; 1.58 [18-Feb-1999]
;;;	In bibtex-normalize-author-editor-names, add additional code
;;;	to (1) remove surrounding space, (2) collapse multiple spaces
;;;	to single ones, and (3) convert slashes to " and ", since some
;;;	library catalogs separate author names that way.
;;;
;;;	Introduce new internal function, bibtex-replace-regexp, to
;;;	simplify coding, replacing (while (re-search-forward ...)
;;;	(replace-match ...)) loops in several places.
;;;
;;; 1.57 [16-Feb-1999]
;;;	Add a copy of internal-LaTeX-default from latex.el so that
;;;	big file need not be loaded.
;;;
;;; 1.56 [05-Feb-1999]
;;;	Move bibtex-get-value and bibdate-to-isodate into this file from
;;;	bibtex-misc.el so that we do not require that file.
;;;
;;;	Rewrite bibtex-get-value to restrict the range of the search to
;;;	the current entry, and to accept an empty value.  Make it raise
;;;	an error if it cannot recognize the start and end of the current
;;;	entry.  The old version could step into a following entry if the
;;;	key was not found in the current entry, or if its value was
;;;	empty.  Extend the function documentation to specify how an
;;;	entry is recognized, and to note that an error can be raised.
;;;
;;; 1.55 [20-Jan-1999]
;;;	Update bibtex-title-abbrev to remove (interactive) call, and
;;;	to raise an error if an empty reduced title string is produced.
;;;
;;;	Revise bibtex-standard-BibNet-citation-label to precompute the
;;;	year using bibtex-get-value instead of bibtex-first-author-name,
;;;	and to check its validity.
;;;
;;;	Revise bibtex-insert-standard-BibNet-citation-label to
;;;	precompute the label before deleting the old one.
;;;
;;;	These changes provide additional error checking in invocations
;;;	of bibtex-insert-standard-BibNet-citation-label, and ensure that
;;;	errors are caught before the buffer is modified.
;;;
;;; 1.54 [12-Dec-1998]
;;;	Replace find-missing-xxx with a completely-new version that is
;;;	shorter, and finally has the desired behavior of error
;;;	termination when no more missing xxx keys are found, so that it
;;;	stops loops with large repeat counts.  The new version also
;;;	ignores lettercase during searches, since BibTeX keys and entry
;;;	types are case-insensitive.
;;;
;;;	Move one function, find-missing-country, to restore conventional
;;;	alphabetical order of function definitions.
;;;
;;;	Add new functions find-missing-chapter, find-missing-edition,
;;;	find-missing-howpublished, find-missing-series, and
;;;	find-missing-type, so that all keywords in the value
;;;	bibtex-entry-field-alist defined in the library file
;;;	emacs/20.3/lisp/textmodes/bibtex.el are now supported by the
;;;	find-missing-* functions.
;;;
;;; 1.53 [08-Dec-1998]
;;;	Add find-missing-note.
;;;	Add handling of \cprime and \cydot accents to
;;;	fix-accents-with-embedded-spaces.
;;;
;;; 1.52 [30-Nov-1998]
;;;	Add bibtex-add-periods-after-author-initials.
;;;
;;; 1.51 [25-Nov-1998]
;;;	In find-missing-xxx, change to terminate with an error, like
;;;	search commands do, so this function, and its callers, can be
;;;	safely used in keyboard macros with infinite repeat counts:
;;;	the error return will terminate the macro.
;;;
;;; 1.50 [12-Nov-1998]
;;;	In bibtex-reduce-string, add call to bibtex-gsub to remove
;;;     TeX control words of three or more letters (\cedla, \cprime,
;;;     \soft, ...).
;;;
;;;     Move these stable functions from bibtex-misc.el to this file:
;;;		find-missing-annote
;;;		find-missing-country
;;;		find-missing-day
;;;		find-missing-subject
;;;		fix-duplicate-bibdate
;;;		fix-duplicate-searchkey
;;;		fix-duplicate-subject
;;;		fix-greek-letter-names
;;;
;;; 1.49 [30-Jul-1998]
;;;	In bibtex-first-author-name, prefix entryname with "[^A-Za-z]"
;;;	to prevent false matches with "xxauthor" or "xxeditor" keys.
;;;
;;; 1.48 [15-Jan-1998]
;;;	Change code in awk-like function gsub to use a loop with
;;;	re-search-forward and replace-match instead of replace-regexp,
;;;	to avoid printing anything in the echo area, and use set-buffer
;;;	instead of with-output-to-temp-buffer to avoid showing the
;;;	temporary buffer.
;;;
;;;	Add awk-like function sub patterned on new gsub implementation.
;;;
;;;	Update get-string to use gsub, so as to avoid modifying the
;;;	buffer, except prior to emacs version 19, where the old code
;;;	is retained because the needed kill-new function is not
;;;	available.  Change the substitution in get-string to reduce
;;;	multiple spaces, newlines, and tabs to single spaces, instead
;;;	of just replacing newlines by spaces.  Add an optional
;;;	argument to get-string to allow suppression of the default
;;;	display of the result string in the echo area.
;;;
;;; 1.47 [14-Jan-1998]
;;;	Extend bibtex-normalize-author-editor-names to trim leading
;;;	space, insert space after periods, handle space before commas,
;;;	and recognize ampersand as an author separator.
;;;
;;; 1.46 [02-Jan-1998]
;;;	Add fix-duplicate-annote.
;;;
;;; 1.45 [22-Oct-1997]
;;;	Insert new functions moved from bibtex-misc.el:
;;;		bibdelete
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
;;;		make-melvyl-coden-lookup-command
;;;		make-melvyl-issn-lookup-command
;;;		make-oclc-issn-lookup-command
;;;		make-oclc-s-ti-command
;;;		qr12
;;;
;;;	Replace
;;;		bibextract
;;;		find-duplicate-key
;;;		find-duplicate-label
;;;		find-missing-ISSN
;;;	with newer versions moved from bibtex-misc.el.  That file
;;;	contains development version of new code, and the moved
;;;	functions have now been sufficiently reliable and stable that
;;;	they are hereby officially released in bibtools.el.
;;;
;;;	Alphabetize all functions in this file, and move defconst,
;;;	defvar, and load calls to the start of this file, after version
;;;	information.  Delete all key bindings, since they are now
;;;	provided by bibtex-keys.el.
;;;
;;;
;;; 1.44 [15-Oct-1997]
;;;	Add fix-duplicate-MRclass, fix-duplicate-MRnumber, and
;;;	fix-duplicate-URL.
;;;
;;; 1.43 [17-Feb-1997]
;;;	Add "at" to bibtex-title-word-ignore-list.  Extend
;;;	bibtex-title-abbrev to ignore font changes, reduce
;;;	\$, \<space>, and \~ to space, and handle dotless i,
;;;	dotless j, and Polish l.
;;;
;;; 1.42 [13-Feb-1997]
;;;	Rewrite most of the function bodies of bibtex-reduce-string and
;;;	bibtex-first-author-name, and add new code-simplifying functions
;;;	bibtex-isaccentletter, bibtex-isalpha, bibtex-isdigit, and
;;;	bibtex-islabelchar, so as to be able to handle braced names,
;;;	names with Jr-like suffixes, and names with embedded accents.
;;;	Add 21 more words ("be", "is", "uber", ...) to
;;;	bibtex-title-word-ignore-list to be consistent with
;;;	already-present "are", "er", "ist", "on", "sur", ....
;;;
;;; 1.41 [08-Feb-1997]
;;;	Change comma separator to semicolon in fix-duplicate-bibsource.
;;;
;;; 1.40 [07-Jan-1997]
;;;	Add fix-duplicate-classification, and change fix-duplicate-keywords
;;;     to use semicolon separator instead of comma.
;;;
;;; 1.39 [20-Dec-1996]
;;;	Tighten pattern in bibtex-title-abbrev to prevent match with
;;;	booktitle.
;;;
;;; 1.38 [09-Dec-1996]
;;;	Modify to bibtex-insert-standard-BibNet-citation-label to make
;;;	hyphen a word separator, to match biblabel/citesub conventions
;;;
;;; 1.37 [30-Oct-1996]
;;; 	Extend bibtex-interpolate-page-numbers to recognize ?? in page
;;;	number ranges.
;;;
;;; 1.36 [21-Jun-1996]
;;;	Remove embedded newline from search command generated by
;;;	make-oclc-isbn-lookup-command.
;;;
;;; 1.35 [17-May-1996]
;;;     Modify find-missing-xxx to return nil if the entry-pattern is
;;;	not found at all.
;;;
;;; 1.34 [19-Feb-1996]
;;;	Modify find-missing-xxx to return a success indicator, so that
;;;	it can be used in a loop.  Add new functions
;;;	convert-book-to-manual, convert-book-to-mastersthesis,
;;;	convert-book-to-techreport, convert-braced-quotes,
;;;	convert-techreport-series-to-type,
;;;	delete-to-end-of-BibTeX-field, find-duplicate-bibdate,
;;;	find-duplicate-bibsource, fix-duplicate-keywords,
;;;	fix-missing-booktitle, fix-missing-booktitle, and
;;;	wrap-long-strings,
;;;
;;; 1.33 [11-Feb-1996]
;;;	Add find-missing-institution, find-missing-organization, and
;;;	find-missing-school.
;;;
;;; 1.32 [10-Feb-1996]
;;;	Extend patterns in find-missing-address.
;;;
;;; 1.31 [09-Feb-1996]
;;;	Add find-missing-crossref.
;;;
;;; 1.30 [08-Feb-1996]
;;;	Modify bibextract to temporarily set the fill prefix to nil,
;;;	because with a non-empty fill prefix, it would not extract
;;;	complete bibliography entries.
;;;
;;; 1.29 [23-Jan-1996]
;;;	Revise regexps in bibextract to match across line boundaries
;;;	(though they will now stop at a quotation mark, such as in an
;;;	umlaut accent).  I don't see a simple solution yet to the
;;;	problem of making this work more generally, so this patch is
;;;	only temporary.
;;;
;;; 1.28 [18-Jan-1996]
;;;	Add bibtex-normalize-author-editor-names and
;;;	bibtex-normalize-one-author-editor-name, and update
;;;	bibtex-first-author-name to use them.  This makes
;;;	bibtex-insert-standard-BibNet-citation-label do a much better
;;;	job, while simultaneously standardizing personal names.
;;;
;;; 1.27 [15-Jan-1996]
;;;	Add remove-duplicate-entries.
;;;
;;; 1.26 [07-Jan-1996]
;;;	Fix error in regexp in bibextract.
;;;
;;; 1.25 [06-Jan-1996]
;;;	Add check-page-ranges, delete-abstract-fields,
;;;	find-missing-CODEN, fix-duplicate-bibsource, fix-month-names,
;;;	and internal-fix-month-names.
;;;
;;; 1.24 [04-Jan-1996]
;;;	Add convert-binary-characters.
;;;
;;; 1.23 [03-Jan-1996]
;;;	Add fix-duplicate-labels.
;;;
;;; 1.22 [29-Dec-1995]
;;;	Add bibextract.
;;;
;;; 1.21 [27-Dec-1995]
;;;	Add fix-mathscinet-accents.
;;;
;;; 1.20 [27-Dec-1995]
;;;	Add convert-author-semicolons-to-ands, fix-braced-quotes, and
;;;	fix-missing-month.  Modify fix-conf-date to make underscore
;;;	optional in conf_date, since it is being replaced by confdate.
;;;
;;; 1.19 [15-Dec-1995]
;;;	Add find-missing-booktitle, and find-duplicate-title. Change
;;;	fix-old-style-bracing to start from point, rather than from
;;;	point-min.
;;;
;;; 1.18 [13-Dec-1995]
;;;	Extend fix-old-style-bracing to recognize multiple letters
;;;	in braces, such as {MH}z, which it now converts to {MHz}.
;;;
;;; 1.17 [12-Dec-1995]
;;;	Repair make-melvyl-isbn-lookup-command, which was broken by
;;;	change 1.14
;;;
;;; 1.16 [09-Dec-1995]
;;;	Add find-missing-abstract and make-abstract
;;;
;;; 1.15 [04-Dec-1995]
;;;	Add make-oclc-isbn-lookup-command.
;;;
;;; 1.14 [20-Nov-1995]
;;;	Add extra newline at start of LC ISBN string to synchronize
;;;	library catalog software.
;;;
;;; 1.13 [17-Oct-1995]
;;;	Supply missing argument on one (format ...) call
;;;
;;; 1.12 [12-Sep-1995]
;;;	Extend fix-old-style-bracing to permit embedded discretionary
;;;	hyphens.
;;;
;;; 1.11 [22-Aug-1995]
;;;	Extend regular expressions to handle braced strings, since
;;;	bibtex.el in emacs 19.29 changed to braced values from quoted
;;;	values.
;;;	Revise make-lc-isbn-lookup-command and
;;;	make-melvyl-isbn-lookup-command to store into the X cut buffer
;;;	as well (like xterm); otherwise, every second paste operation
;;;	into an xterm got the old cut buffer contents, instead of the
;;;	new selection contents.
;;;

(provide 'bibtools)

(require 'bibtex)
;(require 'bibtex-keys)
(require 'clsc)
(require 'isbn)
(require 'ltxaccnt)

(defconst key-regexp "^[ \t]*[A-Za-z][---A-Za-z0-9:.+\/']*[ \t]*=[ \t]*\\(\"[?]*\"\\|{[?]*}\\),"
  "Regular expression that matches a BibTeX key assignment.")

;;; ====================================================================
;;;                 Variables for personal customization
;;; ====================================================================

(defvar bibtex-acknowledgement-value "ack-xxx"
  "String value for an acknowledgement entry value.  This is
conventionally ack- followed by your initials.  The bibliography
should then contain an @String{ack-xxx = \"personal name, address,
telephone, FAX, e-mail\"} definition near the beginning of the file.")

(defvar bibtex-extended-syntax-table
  bibtex-mode-syntax-table
  "Extended syntax table for use in creating standardized BibNet
citation labels.  Accent control sequences and braces become word
characters.")

(defvar bibtex-title-word-ignore-list
  '(
    "a"
    "ab"
    "aber"
    "als"
    "an"
    "and"
    "are"
    "as"
    "auf"
    "aus"
    "az"
    "bei"
    "bir"
    "but"
    "da"
    "das"
    "dat"
    "de"
    "dei"
    "dem"
    "den"
    "der"
    "des"
    "det"
    "di"
    "die"
    "dos"
    "e"
    "een"
    "eene"
    "egy"
    "ei"
    "ein"
    "eine"
    "einen"
    "einer"
    "eines"
    "eit"
    "el"
    "en"
    "er"
    "es"
    "et"
    "ett"
    "eyn"
    "eyne"
    "for"
    "from"
    "fuer"
    "fur"
    "gl"
    "gli"
    "ha"
    "haben"
    "had"
    "hai"
    "has"
    "hat"
    "have"
    "he"
    "heis"
    "hen"
    "hena"
    "henas"
    "het"
    "hin"
    "hinar"
    "hinir"
    "hinn"
    "hith"
    "ho"
    "hoi"
    "i"
    "il"
    "in"
    "ist"
    "ka"
    "ke"
    "l"
    "la"
    "las"
    "le"
    "les"
    "lo"
    "los"
    "mia"
    "mit"
    "n"
    "na"
    "nji"
    "not"
    "o"
    "oder"
    "of"
    "on"
    "or"
    "os"
    "others"
    "s"
    "sie"
    "sind"
    "so"
    "t"
    "ta"
    "the"
    "to"
    "um"
    "uma"
    "un"
    "una"
    "und"
    "une"
    "uno"
    "unter"
    "von"
    "with"
    "y"
    "yr"

    ;; Additional words added later
    "also"
    "any"
    "away"
    "by"
    "cum"
    "dans"
    "down"
    "into"
    "its"
    "off"
    "onto"
    "out"
    "over"
    "sur"
    "that"
    "these"
    "this"
    "those"
    "unto"
    "up"
    "via"
    "without"
    "zu"
    "zum"
    "zur"

    ;; More words added [17-Feb-1997]
    "am"
    "at"
    "aus"
    "aux"
    "be"
    "bin"
    "bist"
    "gehabt"
    "hab"
    "habe"
    "habt"
    "haette"
    "hast"
    "hatte"
    "is"
    "ne"
    "nicht"
    "oben"
    "ohne"
    "pas"
    "seid"
    "uber"
    "vom"
)
  "List of words to be ignored when forming bibliographic citation
labels from title values.  This list is taken from biblabel.")


;;; ====================================================================
;;;	     Miscellaneous functions, in alphabetical order
;;; ====================================================================


(defun bibdate-to-isodate ()
  "After a regexp match with a bibdate field, return an ISO date string
of the form 1994.07.06.15:27:50 for which string comparison is also
chronological comparison."
  (let ((day) (month) (year) (hhmmss))
    (setq year (buffer-substring (match-beginning 5) (match-end 5)))
    (setq month  (buffer-substring (match-beginning 1) (match-end 1)))
    (setq day  (buffer-substring (match-beginning 2) (match-end 2)))
    (setq hhmmss  (buffer-substring (match-beginning 3) (match-end 3)))
    (format "%s.%02d.%02d.%s" year (internal-month-number month) (string-to-int day) hhmmss)))


(defun bibdelete (&optional key value)
  "Delete a subset of BibTeX entries from the current buffer into a
temporary buffer named *bibdelete* which is cleared before starting,
and selected on completion.

Any required @String{...} definitions MUST be copied manually: they
are NOT copied automatically, unlike the action of the slower
standalone bibextract program.

KEY is a regexp identifying the key, and VALUE is a regexp identifying
the value which must be matched for the current BibTeX entry to be
selected.

Reasonable formatting, such as that produced by bibclean, is expected
in order to make this work with simple programming."
  (interactive "sKey: \nsValue: ")
  (goto-char (point-min))
  (with-output-to-temp-buffer "*bibdelete*"
    (save-excursion
      (set-buffer "*bibdelete*")
      (bibtex-mode))
    (let ((regexp) (old-fill-prefix fill-prefix))
      (setq regexp (concat key " *= *[\"]?[^\"]*" value "[^\"]*[\"]?,"))
      (setq fill-prefix nil)
      (while (re-search-forward regexp nil t)
	(mark-paragraph)
	(append-to-buffer "*bibdelete*" (region-beginning) (region-end))
	(delete-region (region-beginning) (region-end))
	(goto-char (region-end)))
      (setq fill-prefix old-fill-prefix))))


(defun bibextract (&optional key value)
  "Extract a subset of BibTeX entries from the current buffer into a
temporary buffer named *bibextract* which is cleared before starting,
and selected on completion.

Any required @String{...} definitions MUST be copied manually: they
are NOT copied automatically, unlike the action of the slower
standalone bibextract program.

KEY is a regexp identifying the key, and VALUE is a regexp identifying
the value which must be matched for the current BibTeX entry to be
selected.

Reasonable formatting, such as that produced by bibclean, is expected
in order to make this work with simple programming."
  (interactive "sKey: \nsValue: ")
  (goto-char (point-min))
  (with-output-to-temp-buffer "*bibextract*"
    (save-excursion
      (set-buffer "*bibextract*")
      (bibtex-mode))
    (let ((regexp) (start))
      (setq regexp (concat key " *= *[\"]?.*" value ".*[\"]?,"))
      (while (re-search-forward regexp nil t)
	(re-search-backward "^@")
	(setq start (point))
	(re-search-forward "^ *} *\n")
	(princ (buffer-substring start (point)))
	(princ "\n")))))


(defun bibtex-add-periods-after-author-initials ()
  "Find author/editor entries in the current visible buffer, and add any
periods that are missing after initials."
  (interactive)
  (let ((begin) (end) (start))
    (setq begin (point))
    (goto-char (point-min))
    (while (re-search-forward "^ *\\(author\\|editor\\) *= *\"" nil t)
      (setq start (1- (point)))		;starting at quotation mark simplifies search below
      (search-forward "\",")
      (setq end (point))
      (goto-char start)
      (while (re-search-forward "[\" ][A-Z][ \"]" end t)
	(backward-char 1)
	(insert "."))
      (goto-char (- end 2))
      (search-forward "\","))
    (goto-char begin)))


(defun bibtex-backward-brace-words (&optional word-count)
  "Insert { ... } around previous ARG (default = 1) words (on \\<bibtex-mode-map>\\[bibtex-backward-brace-words])."
  (interactive)
  (setq word-count (internal-LaTeX-default word-count 1))
  (backward-word word-count)
  (internal-LaTeX-wrap "{" "}" word-count))


(defun bibtex-clean-entry ()
  "For all optional fields of current BibTeX entry: if empty, kill the
whole field; otherwise, remove the \"OPT\" string in the name.  For
all mandatory fields: if empty, signal error."
  (interactive)
  (beginning-of-bibtex-entry)
  (let ((start (point)))
    (save-restriction
      (narrow-to-region start (save-excursion (end-of-bibtex-entry) (point)))
      (while (re-search-forward bibtex-field (point-max) t 1)
	(let ((begin-field (match-beginning 0))
	      (end-field (match-end 0))
	      (begin-name (match-beginning bibtex-name-in-field))
	      (end-name (match-end  bibtex-name-in-field))
	      (begin-text (match-beginning bibtex-text-in-field))
	      (end-text (match-end bibtex-text-in-field))
	      )
	  (goto-char begin-name)
	  (cond ((and
		  (looking-at "OPT")
		  bibtex-clean-entry-zap-empty-opts)
		 (goto-char begin-text)
		 (if (looking-at "\"\"") ; empty: delete whole field
		     (delete-region begin-field end-field)
		   ; otherwise: not empty, delete "OPT"
		   (goto-char begin-name)
		   (delete-char (length "OPT"))
		   (progn
		     ;; fixup alignment. [alarson:19920309.2047CST]
		     (search-forward "=")
		     (delete-horizontal-space)
		     (indent-to-column bibtex-text-alignment))
		   (goto-char begin-field) ; and loop to go through next test
		   ))
		(t
		 (goto-char begin-text)
		 (cond ((looking-at "\"[0-9]+\"") ; if numerical,
			;;(goto-char end-text)
			;; (delete-char -1) ; delete enclosing double-quotes
			;;(goto-char begin-text)
			;;(delete-char 1)
			(goto-char end-field) ; go to end for next search
			;;(forward-char -2) ; to compensate for the 2 quotes deleted
			)
		       ((looking-at "\"\"") ; if empty quotes, complain
			(forward-char 1)
			(if (not (or (equal (buffer-substring
					     begin-name
					     (+ begin-name 3))
					    "OPT")
				     (equal (buffer-substring
					     begin-name
					     (+ begin-name 3))
					    "opt")))
			    (error "Mandatory field ``%s'' is empty"
				   (buffer-substring begin-name end-name))))
		       (t
			(goto-char end-field))))))))
    (goto-char start)
    (end-of-bibtex-entry)
    ;; sct@dcs.edinburgh.ac.uk
    (save-excursion
      (backward-char 1)
      (skip-syntax-backward " >")
      (if (eq (preceding-char) ?,)
 	  (backward-delete-char 1)))
    (skip-whitespace-and-comments)))


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
	    (error "Bibtex entry type %s not defined!" entry-type))
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

      (bibtex-make-field "acknowledgement")
      (backward-char 2)
      (delete-char 2)
      (insert bibtex-acknowledgement-value)

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
    (run-hooks 'bibtex-add-entry-hook)))


(defun bibtex-fill-value ()
  "With point inside a  BibTeX value string, fill the string to BibNet
Project standard indentation and line width."
  (interactive)
  (let ((start) (end) (line-width) (old-fill-prefix fill-prefix))
    (unwind-protect
	(progn
	  (re-search-backward "^ *[A-Za-z][-0-9A-Za-z]* *= *\"")
	  (setq start (match-end 0))
	  (re-search-forward "\",? *$")
	  (setq end (match-end 0))
	  (setq fill-prefix "                 ")
	  (save-excursion
	    (save-restriction
	      (narrow-to-region start end)
	      (goto-char start)
	      (bibtex-replace-regexp "[ \t\f\v\n][ \t\f\v\n]*" " ")
	      (goto-char start)
	      (bibtex-replace-regexp " +:" ":")
	      (goto-char start)
	      (bibtex-replace-regexp " +;" ";")
	      (goto-char (- end 2))
	      (delete-horizontal-space)
	      (insert "\n")
	      (goto-char start)
	      (setq line-width (- fill-column (length fill-prefix)))
	      (if (> line-width (- (point-max) start))
		  nil			;no need to fill
		(forward-char line-width)
		(search-backward " ")
		(delete-char 1)
		(insert "\n")
		(setq start (point))
		(insert fill-prefix)
		(fill-region start (point-max)))
	      (goto-char (point-max))
	      (search-backward "\",")
	      (delete-blank-lines)
	      (delete-horizontal-space)
	      (backward-char 1)
	      (if (looking-at "$")
		  (delete-char 1))
	      (search-forward "\",")
	      (delete-horizontal-space)
	      (goto-char (point-min))
	      (delete-horizontal-space)
	      (goto-char (point-max)))
	    (delete-blank-lines)))
      (setq fill-prefix old-fill-prefix))))


(defun bibtex-first-author-name (entryname)
  "Starting with point inside the current BibTeX entry, move to the
beginning of the entry, scan for the next ENTRYNAME string value, and
return the last name of the first author/editor, or NIL if no
author/editor entry is recognized.

Any names found will be standardized, in order to facilitate correct
extraction of the last name; see \\[bibtex-normalize-author-editor-names]."
  (let ((begin) (end) (fullname) (lastname nil))
    (re-search-backward "^[ \t]*@")
    (goto-char (match-end 0))		; move past @ to avoid matching
					; at this following search
    (if (re-search-forward
	 (concat "[^A-Za-z]" entryname "[ \t]*=[ \t]*\"\\|^[ \t]*[@}]") nil t)
	(progn
	  (if (string-match "^[ \t]*[@}]"
			    (buffer-substring (match-beginning 0) (match-end 0)))
	      nil		; end-of-entry: no entryname string found
	    ;; Else compute value string bounds: begin, end
	    (setq begin (point))

	    ;; Do fancy job of standard personal names
	    (bibtex-normalize-author-editor-names)
	    (goto-char begin)

	    (re-search-forward "[ \n]and[ \n]\\|\",? *$")
	    (goto-char (match-beginning 0))
	    (setq end (point))

	    ;; First author now lies in begin..end, and should look
	    ;; like one of these:
	    ;;  A               Superman                -> Superman
	    ;;  A B             Brenda Starr            -> Starr
	    ;;  A B C           Alfred North Whitehead  -> Whitehead
	    ;;  A B C D         P. D. Q. Bach           -> Bach
	    ;;  ...
	    ;;  A {B C}         Carl {de Boor}          -> deBoor
	    ;;  A {B, C}        James {Baker, III}      -> Baker

	    (goto-char begin)
	    (setq fullname (buffer-substring begin end))

	    ;; [14-Jul-1999] Remove small roman numeral qualifiers,
	    ;; which are often used without a preceding comma: "Baker
	    ;; III" and "Baker, III" are both seen.
	    (let ((case-fold-search nil))
	      (setq fullname (sub "[ ,]+[IVX]+}$" "}" fullname))
	      (setq fullname (sub "[ ,]+[IVX]+$" "" fullname))
	      ;; [16-Mar-2012] Remove Jr. and Sr. suffixes, whether or
	      ;; not they are preceded by a comma, and whether or not
	      ;; the name is braced:
	      ;;     "Daniel E. {Geer, Jr.}" -> Geer
	      ;;     "Daniel E. {Geer Jr.}"  -> Geer
	      ;;     "Daniel E. Geer, Jr."   -> Geer
	      ;;     "Daniel E. Geer Jr."    -> Geer
	      (setq fullname (sub "[ ,]+[JS]r[.]" "" fullname)))

	    ;; (message fullname)
	    ;; (sit-for 2)
	    (let ((case-fold-search nil))
	      (cond
	       ((string-match "[ \n]{\\([^,]+\\)\\(,.*\\)?}[ \n]*$" fullname)
		(setq lastname (substring fullname (match-beginning 1)
					  (match-end 1))))
	       ;; [29-May-1999]: recognize names with lowercase von-like parts:
	       ;;	Ludwig von Beethoven -> von Beethoven
	       ;;	Don Diego de la Vega -> de la Vega
	       ;;	P. van den Driessche -> van den Driessche
	       ;; This is only a partial solution, because it does not recognize
	       ;; names with uppercased von-like parts, e.g.,
	       ;; 	M. Van den Bergh -> den Bergh
	       ;; Issue a beep and a warning in the echo area for such names.
	       ((string-match "\\([ \n]\\([a-z]+[ \n]+\\)+[^ \n]+\\)[ \n]*$" fullname)
		(setq lastname (substring fullname (1+ (match-beginning 1))
					  (match-end 1)))
		(ding t)		; t so we don't terminate keyboard macros
		(message "Warning: unbraced von-like name [%s]" lastname))
	       ((string-match "\\([^ \n]+\\)[ \n]*$" fullname)
		(setq lastname (substring fullname (match-beginning 1)
					  (match-end 1))))
	       (t (error "Cannot determine last name")))
	      ;; Since bibtex-normalize-author-editor-names reduced
	      ;; newlines and tabs to spaces, we need to refill the
	      ;; value before returning
	      (bibtex-fill-value))
	    lastname))
      nil)))


(defun bibtex-first-author-or-editor-name ()
  (or (bibtex-first-author-name "author")
      (bibtex-first-author-name "editor")
      (bibtex-first-author-name "OPTauthor")
      (bibtex-first-author-name "OPTeditor")))


(defun bibtex-find-next-empty-string ()
  "Move to the next empty or unknown-value string in a entry in a
BibTeX file, moving from one entry to the next if necessary (on \\<bibtex-mode-map>\\[bibtex-find-next-empty-string]).

An unknown-value string is one containing nothing but queries.

This function is very convenient for updating bibliographic entries
with missing entries.

For convenience, on a successful search, the starting position before
the search begins is pushed onto the mark stack, as with Emacs search
commands, so that you can easily return to your recent editing
locations by popping the mark, or exchanging the mark and point."
  (interactive)
  (let ((start (point)))
    (if (re-search-forward
	 "^ *[a-zA-Z][-a-zA-Z0-9]+ *= *\\({[ ?]*}\\|\"[ ?]*\"\\)" nil t)
	(progn
	  (push-mark start t)
	  (backward-char 1)))))


(defun bibtex-forward-brace-words (&optional word-count)
  "Insert { ... } around next ARG (default = 1) words (on \\<bibtex-mode-map>\\[bibtex-forward-brace-words])."
  (interactive)
  (setq word-count (internal-LaTeX-default word-count 1))
  (internal-LaTeX-wrap "{" "}" word-count))


(defun bibtex-get-value (key)
  "Return the string value of KEY in the current BibTeX entry, or NIL
if none exists.  Point is unmodified on return.

String values are required to be delimited by quotes, not braces.

The current BibTeX entry starts at the closest previous @ character at
beginning-of-line, starting the search from the end of the line on
which point lies.  It ends at the first instance of a line with a
closing brace preceded by nothing but optional space.

An error is signalled if the start or end of the entry cannot be
determined."
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "^@")
    (let ((start (point)) (end))
      (forward-char 1)
      (if (re-search-forward "^ *}\\|^@" nil t)
	  (progn
	    (beginning-of-line)
	    (if (looking-at "^@")
		(error "Cannot find end of current BibTeX entry")
	      (setq end (point))))
	(error "Cannot find end of current BibTeX entry"))
      (goto-char start)
      (if (re-search-forward (concat "^ *" key " *= *\"\\([^\"]*\\)\",")
			     end t)
	  (buffer-substring  (match-beginning 1) (match-end 1))
	nil))))


(defun bibtex-gsub (regexp replacement s)
  "Substitute the first string matching REGEXP by REPLACEMENT in S, and
return a new string.  This function is pattern after the awk gsub() function."
  (interactive)
  (let ((result) (tmpbuf))
    (save-excursion
      (setq tmpbuf (get-buffer-create "*bibtex-sub*"))
      (set-buffer tmpbuf)
      (insert s)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match replacement nil nil))
      (setq result (buffer-substring (point-min) (point-max))))
    (kill-buffer tmpbuf)
    result))


(defun bibtex-insert-acknowledgement ()
  (interactive)
  (while
      (re-search-forward "^}\\|^[ \t]*bibsource[ \t]*=\\|^[ \t]*acknowledgement[ \t]*=" nil t)
    (goto-char (match-beginning 0))
    (if (looking-at "^}")
	(progn
	  (insert "  acknowledgement = " bibtex-acknowledgement-value ",\n")
	  (update-bibdate)))
    (re-search-forward "^@" nil t)))


(defun bibtex-insert-standard-BibNet-citation-label ()
  "Replace the citation label, if any, on the current BibTeX entry,
with one standardized to the BibNet Project form, LastName:1994:ABC.
No attempt is made to ensure that the label is unique.  Duplicate
citation labels will be detected by BibTeX or bibcheck."
  (interactive)
  (let ((old-syntax-table bibtex-mode-syntax-table) (begin) (label))
    (unwind-protect
	(progn
	  (set-syntax-table bibtex-extended-syntax-table)
	  ;; Exception: hyphens are word separators, to match biblabel/citesub
	  ;; conventions
	  (modify-syntax-entry ?\- " " bibtex-extended-syntax-table)
	  (if (not (re-search-backward "^[ \t]*@[A-Za-z]+{[^,]*,[ \t]*$" nil t))
	      (error "No preceding BibTeX entry found"))
	  (search-forward "{")
	  (setq label (bibtex-standard-BibNet-citation-label))
	  (kill-line nil)
	  (insert label ","))
      (set-syntax-table old-syntax-table))))


(defun bibtex-interpolate-page-numbers ()
  "For journal bibliographies where articles appear consecutively
without intervening blank pages, find the next page number of the form
123--, and supply the ending page number from one less than the next
starting number.  Loop over the remainder of the buffer until all
missing ending page numbers have been supplied."
  (interactive)
  (let ((begin) (end) (first-number) (number))
    (while (re-search-forward "^[ \t]*pages[ \t]*=[ \t]*\"[0-9]+--" nil t)
      (save-excursion
	(backward-char 2)
	(setq end (point))
	(backward-word 1)
	(setq first-number (string-to-number (buffer-substring (point) end))))
      (while (looking-at "[?]")
	(delete-char 1))
      (if (looking-at "\",")
	  (progn
	    (setq begin (point))
	    (re-search-forward "^[ \t]*pages[ \t]*=[ \t]*\"[0-9]+--")
	    (backward-char 2)
	    (setq end (point))
	    (backward-word 1)
	    (setq number
		  (1- (string-to-number (buffer-substring (point) end))))
	    (if (< number first-number)
		(setq number first-number))
	    (goto-char begin)
	    (insert (format "%d" number)))))))


(defun bibtex-isaccentletter (c)
  "Return t if C is a TeX accent letter, and otherwise, nil."
  (or (= c ?u)	;one of [uvHtcbd]  (p. 38, LaTeX Users Guide, 2nd ed.)
      (= c ?v)
      (= c ?H)
      (= c ?t)
      (= c ?c)
      (= c ?b)
      (= c ?d)
      (= c ?k)	;recently-added control sequence for Polish ogonek accent
      (= c ?r)	;recently-added control sequence for Czech ring accent
      ))


(defun bibtex-isalpha (c)
  "Return t if C is an alphabetic character, and otherwise, nil."
  (or
   (and (>= c ?a) (<= c ?z))
   (and (>= c ?A) (<= c ?Z))))


(defun bibtex-isdigit (c)
  "Return t if C is a digit character, and otherwise, nil."
  (and (>= c ?0) (<= c ?9)))


(defun bibtex-islabelchar (c)
  "Return t if C is a BibTeX label character, one of the
set [A-Za-z0-9:-], and otherwise, nil."
  (or
   (= c ?:)
   (= c ?-)
   (bibtex-isalpha c)
   (bibtex-isdigit c)))


(defun bibtex-make-dummy-article-labels ()
  "Change `@Article{:foo,' to `@Article{xxx-12345:foo,' from point to
end of buffer."
  (interactive)
  (let ((n 0))
    (while (re-search-forward "^@Article{:" nil t)
      (backward-char 1)
      (setq n (1+ n))
      (insert (format "xxx-%05d" n)))))


(defun bibtex-make-dummy-labels ()
  "Replace all citation labels in `@Name{label,' contexts with new
labels of the form xxx-12345, from the current point to end of buffer.
This may be useful in preparing a new bibliography which is later to
get standardized labels from biblabel and citesub."
  (interactive)
  (let ((n 0))
    (while (re-search-forward "^@[A-Za-z]+{" nil t)
      (if (or (string-equal (buffer-substring (match-beginning 0) (match-end 0))
			    "@String")
	      (string-equal (buffer-substring (match-beginning 0) (match-end 0))
			    "@Preamble"))
	  nil
	(setq n (1+ n))
	(kill-line nil)
	(insert (format "xxx-%05d," n))))))


(defun bibtex-make-extended-syntax-table ()
  (setq bibtex-extended-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" "w" bibtex-extended-syntax-table)
  (modify-syntax-entry ?\' "." bibtex-extended-syntax-table) ;so Mark's is two words
  (modify-syntax-entry ?\- "w" bibtex-extended-syntax-table) ;so Navier-Stokes is one word
  (modify-syntax-entry ?\: "w" bibtex-extended-syntax-table) ;so Jones:1967:ABC is a word
  (modify-syntax-entry ?\? "w" bibtex-extended-syntax-table) ;so ???? is a word
  (modify-syntax-entry ?\\ "w" bibtex-extended-syntax-table) ;so \TeX is a word
  (modify-syntax-entry ?\136 "w" bibtex-extended-syntax-table) ; literal ASCII caret
  (modify-syntax-entry ?\` "w" bibtex-extended-syntax-table)
  (modify-syntax-entry ?\{ "w" bibtex-extended-syntax-table)
  (modify-syntax-entry ?\} "w" bibtex-extended-syntax-table)
  (modify-syntax-entry ?\~ "w" bibtex-extended-syntax-table)
  ;; NB: Set syntax of space last: otherwise, it ends up with word
  ;; syntax, sigh....
  (modify-syntax-entry ?   "." bibtex-extended-syntax-table))


(defun bibtex-normalize-author-editor-names ()
  "With point inside a BibTeX value string (so its use is not
restricted to just author/editor strings), convert author/editor names
of the forms

	Young, David M. ; Gregory, Robert Todd
	Young, David M. & Gregory, Robert Todd
	von Beethoven, Ludwig
	Young, David M.
	Meyer, Carl D., Jr.
	Sprague,  C. F., III
	Zeller, A.B.C.

to standardized forms

	David M. Young and Robert Todd Gregory
	David M. Young and Robert Todd Gregory
	Ludwig von Beethoven
	David M. Young
	Carl D. {Meyer, Jr.}
	C. F. {Sprague, III}
	A. B. C. Zeller

This is a convenience function for converting names extracted from
library catalog data."
  (interactive)
  (let ((start) (end) (name-start) (next-name-start))

    (re-search-backward "=[ \t]*[{\"]")	;find beginning of value string
    (setq start (match-end 0))

    (beginning-of-line)
    (if (not (looking-at "^ *\\(OPT\\)?\\(author\\|editor\\) *="))
	(error "Not inside author/editor value"))

    (goto-char start)
    (re-search-forward "[^{\\\\][}\"][,\n]")
    (setq end (- (match-end 0) 2))	; exclude delimiter from value string

    (goto-char start)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(bibtex-replace-regexp "[ \t\n]+" " ") ; reduce whitespace

	(goto-char start)
	(bibtex-replace-regexp "[ ]," ",") ; remove space before comma

	; Convert author separator characters to " and "
	(goto-char start)
	;; [02-Feb-2013] Remove slash from regexp, so avoid incorrect
	;; reduction of "{ISO/IEC}" to "{ISO and IEC}".
	;; (bibtex-replace-regexp "[&\;/]" " and ")
	(bibtex-replace-regexp "[&;]" " and ")

	;; Trim superfluous trailing and's
	(goto-char start)
	(bibtex-replace-regexp "\\( +and\\)+ *$" "")

	;; Reduce duplicate and's
	(goto-char start)
	(while (re-search-forward "\\( +and +and +\\)+" nil t)
	  (goto-char start)
	  (bibtex-replace-regexp "\\( +and +and +\\)+" " and ")
	  (goto-char start))

	;; Trim surrounding space
	(goto-char start)
	(delete-horizontal-space)
	(goto-char (point-max))
	(delete-horizontal-space)

	;; This search-and-replace operation is tricky, and took some time to
	;; get right.  A successful return from
	;; bibtex-normalize-one-author-editor-name means that the buffer got
	;; changed unpredictably, so we throw a RESTART to restart the search
	;; from the beginning of the string.  If the inner while loop exits,
	;; then we throw a JOB-DONE to exit the outer loop.
	(catch 'JOB-DONE
	  (while t
	    (catch 'RESTART
	      (goto-char start)
	      (setq name-start start)	; remember start of first name
	      (while (re-search-forward "[ \n\t]and[ \n\t]" nil t)
		(setq next-name-start (match-end 0))
		(if (bibtex-normalize-one-author-editor-name
		     name-start (match-beginning 0))
		    (throw 'RESTART nil)) ; must restart search if buffer changed
		(goto-char next-name-start)
		(setq name-start next-name-start))
	      (throw 'JOB-DONE nil))))


	;; finally, handle last (or only) author/editor name
	(goto-char (point-max))
	(if (re-search-backward "[ \n\t]and[ \n\t]" nil t)
	    (goto-char (match-end 0))
	  (goto-char start))

	(bibtex-normalize-one-author-editor-name (point) (point-max))

	;; Now insert spaces after periods that are surrounded by letters
	;; to change, e.g., "D.E. Knuth" to "D. E. Knuth"
	(goto-char (point-min))
	(while (re-search-forward "[A-Za-z][.][A-Za-z]" nil t)
	  (backward-char 1)
	  (insert-char ?\  1))

	;; Trim surrounding space again
	(goto-char (point-min))
	(bibtex-replace-regexp "^[ \t\n][ \t\n]+" "")
	(goto-char (point-min))
	(bibtex-replace-regexp "[ \t\n][ \t\n]+$" "")

	;; Collapse duplicate spaces
	(goto-char (point-min))
	(bibtex-replace-regexp "[ \t\n][ \t\n]+" " ")))))

(defun bibtex-normalize-one-author-editor-name (start end)
  "Normalize one author/editor name in the region START to END from the forms

	Frankenstein, Hr. Professor Dr.
	von Beethoven, Ludwig
	Young, David M.
	Meyer, Carl D., Jr.
	Sprague,  C. F., III
	Bach, P D Q

to standardized forms

	Hr. Professor Dr. Frankenstein
	Ludwig von Beethoven
	David M. Young
	Carl D. {Meyer, Jr.}
	C. F. {Sprague, III}
	P. D. Q. Bach

and return T if anything was done, and otherwise, return NIL.

What distinguishes these forms is the number of commas.  If no commas
are found, or braces (possibly containing commas) are found, then the
name is left unchanged, so application of this function will not harm
standardized names.

Note that this function will always put a comma before Jr., Sr., II,
III, IV, etc; rare people omit this comma, but we cannot tell whether
to do so or not, because the inverted name form has lost that
information.

Periods are supplied after initials that lack them, even though in
rare cases, this will be wrong (e.g., the Chinese author Wei Nan E,
whose family name is E) because some databases (notably, Medline) drop
periods after initials, when such omission should be a function of the
bibliographic style, instead of encoded in the data.

This is an internal function for use by bibtex-normalize-author-editor-names."

  (let ((old-case-fold-search case-fold-search) (first-and-middle)
	(last) (junior) (result nil))
    (setq case-fold-search nil)		;we MUST distinguish case here
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(cond
	 ;; check for XXX, YYY, ZZZ
	 ((looking-at "^\\([A-Za-z'-]+\\),[ \n\t]+\\([A-Za-z. \n\t'-]+\\),[ \n\t]+\\([A-Za-z. \n\t'-]+\\)$")

	  (setq last (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq first-and-middle
		(buffer-substring (match-beginning 2) (match-end 2)))
	  (setq junior (buffer-substring (match-beginning 3) (match-end 3)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert first-and-middle " {" last ", " junior "}")
	  (setq result t))
	 ;; check for WWW XXX, YYY, ZZZ
	 ((looking-at
	   "^\\([A-Za-z'-]+ +[A-Za-z'-]+\\),[ \n\t]+\\([A-Za-z. \n\t'-]+\\),[ \n\t]+\\([A-Za-z. \n\t'-]+\\)$")

	  (setq last (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq first-and-middle
		(buffer-substring (match-beginning 2) (match-end 2)))
	  (setq junior (buffer-substring (match-beginning 3) (match-end 3)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert first-and-middle " {" last ", " junior "}")
	  (setq result t))
	 ;; check for WWW XXX, YYY
	 ((looking-at
	   "^\\([A-Za-z'-]+ +[A-Za-z'-]+\\),[ \n\t]+\\([A-Za-z. \n\t'-]+\\)$")

	  (setq last (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq first-and-middle
		(buffer-substring (match-beginning 2) (match-end 2)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert first-and-middle " " last)
	  (setq result t))
	 ;; check for XXX, YYY
	 ((looking-at "^\\([A-Za-z'-]+\\),[ \n\t]+\\([A-Za-z. \n\t'-]+\\)$")

	  (setq last (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq first-and-middle
		(buffer-substring (match-beginning 2) (match-end 2)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert first-and-middle " " last)
	  (setq result t)))

	(goto-char start)
	(while (re-search-forward "[ \t\n][ \t\n]+" nil t) ; reduce whitespace
	  (setq result t)
	  (replace-match " " nil t))
	;; insert any missing periods after initials
	(goto-char start)
	(while (re-search-forward "[ \t\n][A-Z][ \t\n]\\|^[A-Z][ \t\n]" nil t)
					; find initials lacking periods
	  (setq result t)
	  (backward-char 1)
	  (insert "."))))
    (setq old-case-fold-search case-fold-search)
    result))


(defun bibtex-reduce-string (s)
  "Squeeze out TeX accents and other characters from S that are not in
the set [A-Za-z0-9:-], and return the reduced string.  This function is
used for removing control sequences and bracing from tentative BibTeX
citation labels."

  ;; First remove TeX control words of three or more letters, e.g.,
  ;; \cedla, \cprime, \soft, ...  One- and two-character control words
  ;; (\c, \u, \v, ..., \aa, \ae, \oe) are handled specially below in the
  ;; first and second cases of the (cond ...)
  (setq s (bibtex-gsub "[\\][A-Za-z][A-Za-z][A-Za-z]+" "" s))

  (let ((len (length s)) (m 0) (n 0) (u))
    (setq u (make-string len 0))
    (while (< n len)
      (cond
       ((and (= (aref s n) ?\\) (< n (1- len))
	     (bibtex-isaccentletter (aref s (1+ n)))) ; then have TeX accent
	(setq n (1+ n)))		; so ignore it
       ((and (= (aref s n) ?\\) (< n (1- len)))	; then have TeX control word
					;like \oe, \aa, ..., \ss
	(while (and (< n (1- len)) (bibtex-isalpha (aref s (1+ n))))
	  (setq n (1+ n))		;copy the csname but not the backslash
	  (aset u m (aref s n))
	  (setq m (1+ m))))
       ((bibtex-islabelchar (aref s n))	; then have character in [A-Za-z0-9:-]
	(aset u m (aref s n))
	(setq m (1+ m)))
       (t				; else ignore this character
	nil))
      (setq n (1+ n)))
    (substring u 0 m)))


(defun bibtex-replace-regexp (regexp to-string)
  "Work much like replace-regexp, except be silent about it."
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))


(defun bibtex-replace-string (from-string to-string &optional unused-argument)
  "Work much like replace-string, except be silent about it."
  (while (re-search-forward from-string nil t)
    (replace-match to-string nil t)))


(defun bibtex-split (s regexp)
  "Return a list of strings from splitting S at the regular expression
REGEXP.  This function is patterned after the awk split() function."
  (if (or (null s) (null regexp)) ;then return nil if either s or regexp is nil
      nil
    (let ((p nil) (k 0)) ;else split the string and return a list of strings
      (while (and
	      (< k (length s))
	      (string-match regexp s k))
	(setq p (nconc p (list (substring s k (match-beginning 0)))))
	(setq k (match-end 0)))
      (setq p (nconc p (list (substring s k))))
      p)))


(defun bibtex-standard-BibNet-citation-label ()
  "Starting with point inside the current BibTeX entry, move to the
beginning of the entry, create a standard BibNet citation label, and
return it.  Point is guaranteed to be left unchanged on return.

The author/editor, year, and title string values are checked for
consistency, and an error is raised if they are invalid."
  (let ((begin (point)) (label nil) (year))
    (unwind-protect
      (setq year (or (bibtex-get-value "year")
		     (bibtex-get-value "OPTyear")
		     "ERROR: This entry has no year value"))
      (if (null (string-match "^[12][0-9][0-9x][0-9x]" year))
	  (error "Illegal year pattern [%s]" year))
      (setq label (bibtex-reduce-string
		   (concat (bibtex-first-author-or-editor-name) ":"
			   (substring year 0 4) ":"
			   (bibtex-title-abbrev))))
      (goto-char begin))
    label))


(defun bibtex-sub (regexp replacement s)
  "Substitute the first string matching REGEXP by REPLACEMENT in S, and
return a new string.  This function is pattern after the awk sub() function."
  (interactive)
  (let ((result) (tmpbuf))
    (save-excursion
      (setq tmpbuf (get-buffer-create "*bibtex-sub*"))
      (set-buffer tmpbuf)
      (insert s)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
	  (replace-match replacement nil nil))
      (setq result (buffer-substring (point-min) (point-max))))
    (kill-buffer tmpbuf)
    result))


(defun bibtex-title-abbrev ()
  "Starting with point inside the current BibTeX entry, move to the
beginning of the entry, scan forward for the next BibTeX title string
value, and return a 1- to 3-character string that abbreviates the
title, ignoring common words from the bibtex-title-word-ignore-list
variable."
  (let ((abbrev nil) (begin) (s) (word) (wordlist))
    (re-search-backward "^[ \t]*@")
    (if (re-search-forward "[^a-zA-Z]\\(OPT\\)?title[ \t]*=[ \t]*\"" nil t)
	(progn
	  (setq begin (point))
	  (if (re-search-forward "\",[ \t]*$" nil t)
	      (progn
		(setq s (buffer-substring begin (point)))

		;; NB: This code follows closely that in function
		;; title_string() in biblabel, so that the emacs and awk
		;; implementations will both generate the same labels.

		;; change \$, \<space>, and ~ to space
		(setq s (bibtex-gsub "[\\][$ ]" " " s))
		(setq s (bibtex-gsub "\\([^\\]\\)[~]" "\\1 " s))

		;; delete all math modes, including attached following
		;; letters (e.g. $k$th and $k$-th, but not $B$-spline)
		(setq s (bibtex-gsub "[$][^$]*[$]-th" " " s))
		(setq s (bibtex-gsub "[$][^$]*[$][A-Za-z]*" " " s))

		;; collapse runs of hyphen, slash and whitespace to single spaces
		(setq s (bibtex-gsub "[-\/ \t\n]+" " " s))

		;; convert TeX control sequences for dashes and slashes to single space
		(setq s (bibtex-gsub "\\emdash" " " s))
 		(setq s (bibtex-gsub "\\endash" " " s))
 		(setq s (bibtex-gsub "\\slash"  " " s))

		;; convert dotless-i to i, dotless-j to j, Polish
		;; slash-l to l, and slash-L to L
		(setq s (bibtex-gsub "{[\\]\\([ijlL] *\\)}" "\\1" s))
		(setq s (bibtex-gsub "[\\]\\([ijlL] *\\){}" "\\1" s))
		(setq s (bibtex-gsub "[\\]\\([ijlL]\\) +" "\\1" s))

		;; delete font changes
		(setq s (bibtex-gsub "[\\]\\(bf\\|em\\|it\\|rm\\|sc\\|sf\\|sl\\|tt\\) +" "" s))
		(setq s (bibtex-gsub "[\\]text\\(bf\\|it\\|md\\|rm\\|sc\\|sf\\|sl\\|tt\\|up\\) *" "" s))
		(setq s (bibtex-gsub "[\\]\\(it\\|sc\\|sl\\|up\\)shape *" "" s))
		(setq s (bibtex-gsub "[\\]\\(bf\\|md\\)series *" "" s))
		(setq s (bibtex-gsub "[\\]\\(rm\\|sf\\|tt\\)family *" "" s))
		(setq s (bibtex-gsub "[\\]\\(emph\\|[hmv]box\\) *{" "{" s))

		;; delete accents, including newly-added Polish ogonek
		(setq s (bibtex-gsub "[\\][.'`^=~]" "" s))
		(setq s (bibtex-gsub "[\\][uvHtcbdk][ {]" "" s))

		;; collapse other control sequences to unbackslashed names
		(setq s (bibtex-gsub "[\\]\\([a-zA-Z]+\\)" "\\1" s))

		;; remove Dutch, French, Italian, ... contractions
		(setq s (bibtex-gsub "[ ][DdLlNnTt]'" " " s))

		;; change quotation marks to space
		(setq s (bibtex-gsub "``\\|''" " " s))

		;; change selected punctuation to space
		(setq s (bibtex-gsub "[().,;:!?]" " " s))

		;; squeeze out remaining non-alphanumerics
		(setq s (bibtex-gsub "[^A-Za-z0-9 ]" "" s))

		;; need uniform case for ignore list lookup
		(setq s (downcase s))

		;; create a list of words
		(setq wordlist (bibtex-split s " +"))

		;; collect up to 3 initial letters for words that
		;; are absent from the ignore list
		(while (and (car wordlist) (< (length abbrev) 3))
		  (setq word (car wordlist))
		  (if (and
		       (> (length word) 0)
		       (bibtex-isalpha (aref word 0))
		       (not (member word bibtex-title-word-ignore-list)))
		      (setq abbrev (concat abbrev (substring word 0 1))))
		  (setq wordlist (cdr wordlist)))
		(if (= (length abbrev) 0)
		    (error "Empty title string"))
		(upcase abbrev))))
      (error "cannot find title string"))))

(defun bibtex-validate-citation-labels ()
  "Starting at the BibTeX entry at, or following, point, regenerate
citation labels, and check that they match existing labels, ignoring
any suffixes on the latter.  Execution terminates with an error at the
first mismatch, or normally at end of buffer."
  (interactive)
  (while (re-search-forward "^@[A-RT-Z][a-z]+{" nil t)
    (let ((newlabel nil)
	  (oldlabel nil)
	  (start (point)))
      (search-forward ",")
      (setq oldlabel (buffer-substring start (1- (point))))
      (setq newlabel (bibtex-standard-BibNet-citation-label))
      ;; (message "debug: old [%s] vs new [%s]" oldlabel newlabel)
      ;; (sit-for 5)
      (if (not (string-equal newlabel
			     (substring oldlabel 0
					(min (length newlabel)
					     (length oldlabel)))))
	  (error "Mismatch: expected label %s" newlabel))))
  (message "[done]"))

(defun check-page-ranges ()
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
		    (princ (format "\nInconsistent page range: %d : %d--%d\n"
				   previous initial final))
		    (princ "--------------------\n\n")))))
	(setq previous-entry this-entry)
	(setq previous final)))))


(defun convert-author-editor-commas-to-ands ()
  "Given a comma-separated list of author or editor names in the
current BibTeX value string, convert the commas to `` and '', convert
runs of whitespace to single spaces, and remove leading and trailing
space.  Name suffixes (Jr, II, III, and IV) are recognized and
handled.

Execution will abort with an error if this function is not invoked
inside an author or editor value string.

This is a convenience function for conversion of data cut-and-pasted
from online sources."
  (interactive)
  (let ((start))
    (save-excursion
      (save-restriction
	(re-search-backward "[ =]\"")
	(setq start (+ 2 (point)))
	(beginning-of-line)
	(if (not (looking-at "^ *\\(author\\|editor\\) *= *\""))
	    (error "not in author/editor string"))
	(goto-char start)
	(re-search-forward "\",")
	(narrow-to-region start (- (point) 2))
	(goto-char (point-min))
	(delete-horizontal-space)
	(goto-char (point-min))
	(bibtex-replace-regexp "[ \t\n]+" " ")
	(goto-char (point-min))
	(bibtex-replace-regexp "[, ]+$" "")
	(goto-char (point-min))
	(bibtex-replace-regexp "^,+" "")
	(goto-char (point-min))
	(bibtex-replace-regexp " *,\\( *,\\)+ *" ", ")
	(goto-char (point-min))
	(bibtex-replace-regexp " *, *" " and ")
	(goto-char (point-min))
	(bibtex-replace-string " and and " " and ")
	(goto-char (point-min))
	(bibtex-replace-regexp "\\([-A-Za-z']+\\) and \\([JS]r\\)[.]?" "{\\1, \\2.}")
	(goto-char (point-min))
	(bibtex-replace-regexp "\\([-A-Za-z']+\\) and \\(II+\\|IV\\)" "{\\1, \\2}")
	(setq start (point-min))))
    (goto-char start)
    (bibtex-normalize-author-editor-names)
    (goto-char start)
    (bibtex-fill-value)))


(defun convert-author-semicolons-to-ands ()
  "Convert a semicolon separated author list to BibTeX form in all
BibTeX entries.  The MathSciNet database produces author lists in this
form."
  (interactive)
  (while (< (point) (point-max))
    (re-search-backward "^@")
    (re-search-forward "^ *author *= *\"")
    (let ((start (point)))
      (re-search-forward "\",$")
      (narrow-to-region start (point))
      (goto-char (point-min))
      (bibtex-replace-string ";" " and ")
      (goto-char (point-min))
      (bibtex-replace-regexp "  +" " "))
    (widen)
    (re-search-forward "^@")))


(defun convert-binary-characters ()
  "Find characters in the range 128..255, and replace them by
sequences ?xyz that preserve their values in octal, while allowing
them to be processed by TeX."
  (interactive)
  (let ((n))
    (while (re-search-forward "[€-ÿ]" nil t)
      (backward-char 1)
      (setq n (string-to-char (buffer-substring (point) (1+ (point)))))
      (delete-char 1)
      (insert (format "?%03o" n)))))


(defun convert-book-to-manual ()
  "If the current entry is a Book type, change the type to Manual,
and the publisher to organization."
  (interactive)
  (re-search-backward "^@")
  (if (looking-at "@Book")
      (progn
	;; (recenter 1)
	(delete-char 5)
	(insert "@Manual")
	(re-search-forward "^ *publisher *=\\|^}")
	(if (string-equal "}" (buffer-substring (match-beginning 0)
						(1+ (match-beginning 0))))
	    (error "No publisher found!"))
	(delete-region (match-beginning 0) (match-end 0))
	(insert "  organization ="))))


(defun convert-book-to-mastersthesis ()
  "If the current entry is a Book type, change the type to MastersThesis,
and the publisher to school."
  (interactive)
  (re-search-backward "^@")
  (if (looking-at "@Book")
      (progn
	;; (recenter 1)
	(delete-char 5)
	(insert "@MastersThesis")
	(re-search-forward "^ *publisher *=\\|^ *school *=\\|^}")
	(if (string-equal "}" (buffer-substring (match-beginning 0)
						(1+ (match-beginning 0))))
	    (error "No publisher found!"))
	(delete-region (match-beginning 0) (match-end 0))
	(insert "  school ="))))


(defun convert-book-to-techreport ()
  "If the current entry is a Book type, change the type to TechReport,
and the publisher to institution."
  (interactive)
  (re-search-backward "^@")
  (if (looking-at "@Book")
      (progn
	;; (recenter 1)
	(delete-char 5)
	(insert "@TechReport")
	(let ((start (point)))
	  (re-search-forward "^ *publisher *=\\|^}")
	  (if (string-equal "}" (buffer-substring (match-beginning 0)
						  (1+ (match-beginning 0))))
	      (error "No publisher found!"))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert "  institution =")

	  (goto-char start)
	  (re-search-forward "^ *series *=\\|^}")
	  (if (string-equal "}" (buffer-substring (match-beginning 0)
						  (1+ (match-beginning 0))))
	      nil
	    (back-to-indentation)
	    (delete-char 6)
	    (insert "type  "))

	  (goto-char start)
	  (re-search-forward "^ *number *=\\|^}")
	  (if (string-equal "}" (buffer-substring (match-beginning 0)
						  (1+ (match-beginning 0))))
	      (progn
		(back-to-indentation)
		(insert "  number =       \"\",\n")
		(backward-char 3)))))))


(defun convert-braced-quotes ()
  "Convert braced quotes inside BibTeX value strings to `` and '',
warning if unbalanced quotes are found in a single entry."
  (interactive)
  (while (search-forward "{\"}" nil t)
    (let ((start) (end) (open))
      (backward-char 3)
      (search-backward "\"")
      (setq start (1+ (point)))
      (search-forward "\",")
      (setq end (- (point) 2))
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (setq open t)
	  (while (search-forward "{\"}" nil t)
	    (delete-backward-char 3)
	    (if open
		(insert "``")
	      (insert "''"))
	    (setq open (not open)))
	  (if (not open)
	      (error "Unbalanced brace quotes in this entry")))))))


(defun convert-techreport-series-to-type ()
  "Convert TechReport series keywords to type."
  (interactive)
  (while (re-search-forward "^@TechReport" nil t)
    (let ((start (point)))
      (re-search-forward "^ *series *=\\|^ *type *=\\|^}")
      (goto-char (match-beginning 0))
      (if (looking-at "^ *series *=")
	  (progn
	    (back-to-indentation)
	    (delete-char 6)
	    (insert "type  "))))))


(defun copy-last-BibTeX-value-string ()
  "Given an unset field string in any of the forms
	key = \"\",
	key = \"??\",
	OPTkey = \"\",
	OPTkey = \"??\",
copy the corresponding value from the previous entry, and remove the
OPT prefix, if any.  This is convenient for updating consecutive
journal entries."
  (interactive)
  (let ((label) (start) (value-pos) (value))
    (re-search-forward key-regexp nil t)
    (back-to-indentation)
    (if (string-equal "OPT" (buffer-substring (point) (+ 3 (point))))
	(forward-char 3))
    (setq start (point))
    (re-search-forward "[ \t]" nil t)
    (setq label (buffer-substring start (1- (point))))
    (if (string-equal "pages" label)
	(progn
	  (bibtex-remove-OPT)
	  (beginning-of-line)
	  (re-search-forward "[\"{]")
	  (while (looking-at "[?]")
	      (delete-char 1))
	  (error "Cannot copy pages value")))
    (re-search-forward "[\"{]")
    (while (string-equal "?" (buffer-substring (point) (1+ (point))))
      (delete-char 1))
    (setq value-pos (point))
    (beginning-of-line)
    (re-search-backward (concat "^[ \t]*" label "[ \t]*=[ \t]*") nil t)
    (goto-char (match-end 0))
    (if (looking-at "[\"{]")		;have key = "value",
	(progn
	  (setq start (1+ (point)))
	  (re-search-forward "[\"}],")
	  (setq value (buffer-substring start (- (point) 2)))
	  (goto-char value-pos)
	  (insert value))
      (setq start (point))		;else have key = abbrev,
      (search-forward ",")
      (setq value (buffer-substring start (- (point) 1)))
      (goto-char value-pos)
      (backward-char 1)
      (if (looking-at "\"\",\\|{},")
	  (delete-char 2))
      (insert value))
    (back-to-indentation)
    (if (looking-at "OPT")
	(bibtex-remove-OPT))))


(defun delete-abstract-fields ()
  "Delete abstract key/value pairs in bibliography entries from point
to end of buffer.  Bibliography conversion software may output abstracts,
but copyright restrictions may not permit their further dissemination,
sigh..."
  (interactive)
  (let ((start))
  (while (re-search-forward "^ *abstract *=" nil t)
    (beginning-of-line)
    (setq start (point))
    (search-forward "\",\n")
    (delete-region start (point)))))


(defun delete-to-end-of-BibTeX-field ()
  "Delete text from point to the end of the BibTeX value field"
  (interactive)
  (let ((start (point)))
    (re-search-forward "[^{\\]\"")
    (backward-char 1)
    (delete-region start (point))
    (delete-horizontal-space)))


(defun extended-insert-register (reg &optional arg)
  "Insert contents of register REG.  REG is a character.  Normally
puts point after and mark before the inserted text, just as if you had
typed it.  If the optional second arg is non-nil, puts mark after and
point before.  This is the opposite of the dumb default in GNU Emacs
18 and 19.  Interactively, second arg is non-nil if prefix arg is
supplied."
  (interactive "cInsert register: \nP")
  (insert-register reg (not (null arg))))


(defun extended-occur-mode-mouse-goto (event)
  "Move point in the *Occur* buffer to the clicked-on line, move that
line to the top of the window, and then execute
occur-mode-mouse-goto."
  (interactive "e")
  (switch-to-buffer "*Occur*")
  (mouse-set-point event)
  (goto-char (point))
  (recenter 1)
  (occur-mode-mouse-goto event))


(defun find-abstract ()
  "Find the next bibliography entry containing an abstract, position
it to the top of the screen, and highlight the abstract."
  (interactive)
  (search-forward "  abstract =" nil t)
  (search-backward "\n@" nil t)
  (forward-char 1)
  (recenter 0)
  (search-forward "  abstract =" nil t)
  (beginning-of-line)
  (transient-mark-mode t)
  (push-mark (point) t)
  (search-forward "\",\n" nil t)
  (exchange-point-and-mark)
  (exchange-point-and-mark))


(defun find-bad-date-range ()
  "Search forward for the next instance of a possible date range using
hyphen instead of en-dash.  If one is found, point is left after the
hyphen.  Otherwise, point is unchanged."
  (interactive)
  (let ((found nil) (start (point)))
    (catch 'DONE
      (while
	  (re-search-forward
	   "[^0-9---][0-9][0-9]?-[0-9][0-9]?[^0-9---X]" nil t)
	(search-backward "-")
	(forward-char 1)
	(save-excursion
	  (beginning-of-line)
	  (if (not (looking-at
		    "^@\\|^[ \t]*ISBN[ \t]*=\\|^[ \t]*ISSN[ \t]*=\\|^[ \t]*LCCN[ \t]*=\\|^[ \t]*number[ \t]*=\\|^[ \t]*review[ \t]*="))
	      (progn
		(setq start (point))
		(push-mark (point) t)
		(setq found t)
		(ding)
		(throw 'DONE nil)))))
      (goto-char start))
    (if (not found)
	(message "[Done]"))))


(defun find-bad-note ()
  "Find bad `note = \"...\"' and `annote = \"...\"' entries, missing
an initial capital letter, or a final period."
  (interactive)
  (catch 'done
    (while (re-search-forward "^[ \t]*a?n?note[ \t]*=" nil t)
      (search-forward "\"")
      (if (looking-at "[a-z]")
	  (progn
	    (message "Need initial capital")
	    (throw 'done t)))
      (search-forward "\",")
      (backward-char 3)
      (if (not (looking-at "[.?!]"))
	  (progn
	    (forward-char 1)
	    (message "Missing period!")
	    (throw 'done t)))))
  (message "[Done]")
  nil)


(defun find-binary-character ()
  "Search forward for the next character in the range 128..255, leaving
point following that character, or raising an error and leaving point
unchanged if no such character is found."
  (interactive)
  (let ((start (point)))
    (while (and (< (point) (point-max))
		(< (following-char) 128))
      (forward-char 1))
    (if (< (point) (point-max))
	(forward-char 1)
      (goto-char start)
      (error "No binary character found"))))


(defun find-duplicate-author ()
  "In a bibliography sorted in publication order, find consecutive
entries that have the same author, and might therefore be duplicates."
  (interactive)
  (let ((last-author ""))
    (while (re-search-forward "^@[A-Za-z]+{" nil t)
      (if (looking-at "[A-Z][a-z]+")
	  (progn
	    (if (string-equal last-author (buffer-substring
					   (match-beginning 0)
					   (match-end 0)))
		(error "Duplicate author"))
	    (setq last-author (buffer-substring (match-beginning 0)
						(match-end 0))))))))


(defun find-duplicate-bibdate ()
  (interactive)
  (let ((start))
    (while (re-search-forward "^@.*,$" nil t)
      (forward-line 1)
      (setq start (point))
      (re-search-forward  "^}\\|^ *bibdate *=" nil t)
      (beginning-of-line)
      (if (looking-at "^ *bibdate *=")
	  (progn
	    (forward-line 1)
	    (re-search-forward  "^}\\|^ *bibdate *=" nil t)
	    (beginning-of-line)
	    (if (looking-at "^ *bibdate *=")
		(error "duplicate bibdate"))))
      (goto-char start))))


(defun find-duplicate-bibsource ()
  (interactive)
  (let ((start))
    (while (re-search-forward "^@.*,$" nil t)
      (forward-line 1)
      (setq start (point))
      (re-search-forward  "^}\\|^ *bibsource *=" nil t)
      (beginning-of-line)
      (if (looking-at "^ *bibsource *=")
	  (progn
	    (push-mark (point))
	    (forward-line 1)
	    (re-search-forward  "^}\\|^ *bibsource *=" nil t)
	    (beginning-of-line)
	    (if (looking-at "^ *bibsource *=")
		(error "duplicate bibsource"))))
      (goto-char start))))


(defun find-duplicate-key ()
  "Search forward in a BibTeX file buffer for the next
entry with consecutive duplicate keys."
  (interactive)
  (let ((start) (old-key))
    (catch 'DONE
      (while (re-search-forward "^[ \t]*[A-Za-z][---A-Za-z0-9:.+\/']*[ \t]*=" nil t)
	(setq start (match-beginning 0))
	(setq old-key (buffer-substring start (point)))
	(re-search-forward "^[ \t]*[A-Za-z][---A-Za-z0-9:.+\/']*[ \t]*=")
	(if (string-equal old-key
			  (buffer-substring (match-beginning 0) (match-end 0)))
	    (progn
	      (goto-char (1+ start))
	      (beginning-of-line)
	      (recenter 2)
	      (message "Duplicate key")
	      (throw 'DONE nil)))
	(goto-char (1+ start))))))


(defun find-duplicate-label ()
  "Search forward in a BibTeX file buffer for the next pair of
consecutive entries with duplicate citation labels."
  (interactive)
  (catch 'DONE
    (let ((start) (old-label))
      (while (re-search-forward "^@[A-Za-z]+{" nil t)
	(setq start (match-end 0))
	(search-forward ",")
	(setq old-label (buffer-substring start (point)))
	(re-search-forward "^@[A-Za-z]+{")
	(if (string-equal old-label
			  (buffer-substring (point)
					    (+ (point) (length old-label))))
	    (progn
	      (goto-char start)
	      (recenter 2)
	      (throw 'DONE "Duplicate label")))
	(goto-char start)))))


(defun find-duplicate-title ()
  "Search forward in a BibTeX file buffer for the next
entry with consecutive duplicate titles."
  (interactive)
  (let ((start) (old-start) (old-title))
    (while (re-search-forward "^[ \t]*title[ \t]*=" nil t)
      (setq start (match-beginning 0))
      (setq old-start (match-beginning 0))
      (search-forward "\",")
      (setq old-title (buffer-substring start (point)))
      (re-search-forward  "^[ \t]*title[ \t]*=" nil t)
      (setq start (match-beginning 0))
      (search-forward "\",")
      (if (string-equal old-title (buffer-substring start (point)))
	  (progn
	    (goto-char (1+ old-start))
	    (re-search-backward "^@")
	    (recenter 0)
	    (forward-char 1)
	    (re-search-forward "^@")
	    (error "Duplicate title")))
      (goto-char (1+ start))))
     (message "[done]"))


(defun find-greek-outside-math ()
  "Find Greek letter name macros and verify that they are in math maod."
  (interactive)
  (catch 'DONE
    (let ((in-math nil))
      (while (re-search-forward
	      "^@\\|[_^]\\|[$]\\|\\([^\\a-zA-Z]\\)[\\]?\\(alpha\\|beta\\|gamma\\|delta\\|epsilon\\|zeta\\|eta\\|theta\\|iota\\|kappa\\|lambda\\|mu\\|nu\\|xi\\|omicron\\|pi\\|rho\\|sigma\\|tau\\|upsilon\\|phi\\|chi\\|psi\\|omega\\)\\([^$a-zA-Z]\\)" nil t)
	(goto-char (match-beginning 0))
	(cond
	 ((looking-at "^@")
	  (setq in-math nil))
	 ((looking-at "[$]")
	  (setq in-math (not in-math)))
	 (t
	  (if (not in-math)
	      (progn
		(message (format "Matched: [%s] in-math: [%s]"
				 (buffer-substring (match-beginning 0)
						   (match-end 0)) in-math))
		(throw 'DONE nil)))))
	(goto-char (match-end 0))))))


(defun find-label (label)
  "Given a BibTeX citation LABEL, start at the current point and move to
the first entry which matches that label.  Otherwise, assume the labels
are sorted as if by \"bibsort\", and move to just before the entry that
would follow an entry with that LABEL."
  (interactive "sCitation label: ")
  (let ((regexp (concat "^@[a-z]+{" label ", *$"))
	(old-case-fold-search case-fold-search)
	(start (point)))
    (unwind-protect
	(catch 'DONE
	  (setq case-fold-search t)
	  (setq label (downcase label))
	  (if (re-search-forward regexp nil t)
	      (beginning-of-line)
	    (goto-char start)
	    (while (re-search-forward
		    "^@[a-z]+{\\([^=,]*\\), *$" nil t)
	      (if (string-less-or-equalp label
					 (downcase (buffer-substring
						    (match-beginning 1)
						    (match-end 1))))
		  (throw 'DONE nil)))))
      (setq case-fold-search old-case-fold-search))
    (beginning-of-line)))


(defun find-long-label ()
  "Find the next long BibTeX citation label, because it is probably
wrong."
  (interactive)
  (catch 'DONE
    (while (re-search-forward "^@[A-Za-z]+{[^ ]*, *$" nil t)
      (end-of-line)
      (if (> (current-column) 40)
	  (progn
	    (push-mark)
	    (recenter 2)
	    (beginning-of-line)
	    (re-search-forward ":\\|$")
	    (backward-char 1)
	    (error "long label"))))))

;;; ====================================================================
;;;                Finding Missing Keys in a Bibliography
;;;
;;; The following functions allow you to easily find missing key/value
;;; pairs in bibliography entries.  For example, an @Article{...}
;;; entry should have author, title, journal, volume, usually number,
;;; sometimes month, always pages, and year assignments.
;;;
;;; In working with a large bibliography, you may have to invoke these
;;; functions many times, so it is most convenient to temporarily bind
;;; them to a key.  Some keyboards have a Find key which is rarely used
;;; by other Emacs code, so you might find something like
;;;
;;;	(local-set-key [f19] 'find-missing-title)
;;;
;;; convenient; function key f19 is the Find key on Sun Type 5
;;; keyboards.
;;; ====================================================================


(defun find-missing (key)
  "Search forward for next bibliography entry missing a KEY value
assignment."
  (interactive "sKey name:")
  (find-missing-xxx "^@" (concat "^[ \t]*" key "[ \t]*=")))


(defun find-missing-CODEN ()
  "Search forward for next bibliography entry missing a `CODEN' value"
  (interactive)
  (find-missing-xxx
   "^@Article\\|^@Book\\|^@Periodical\\|^@Proceedings" "^[ \t]*CODEN[ \t]*="))


(defun find-missing-DOI ()
  "Search forward for next bibliography entry missing an `DOI' value."
  (interactive)
  (find-missing-xxx  "^@" "^[ \t]*DOI[ \t]*="))


(defun find-missing-ISBN ()
  "Search forward for next bibliography entry missing an `ISBN' value"
  (interactive)
  (find-missing-xxx  "^@Book\\|^@Proceedings" "^[ \t]*ISBN[ \t]*="))


(defun find-missing-ISBN-13 ()
  "Search forward for next bibliography entry missing an `ISBN-13' value."
  (interactive)
  (find-missing-xxx  "^@Book\\|^@Proceedings" "^[ \t]*ISBN-13[ \t]*="))


(defun find-missing-ISSN ()
  "Search forward for next bibliography entry missing an `ISSN' value."
  (interactive)
  (find-missing-xxx  "^@Article\\|^@Book\\|^@Periodical\\|^@Proceedings" "^[ \t]*ISSN[ \t]*="))


(defun find-missing-LCCN ()
  "Search forward for next bibliography entry missing an `LCCN' value"
  (interactive)
  (find-missing-xxx  "^@Book\\|^@Proceedings" "^[ \t]*LCCN[ \t]*="))


(defun find-missing-URL ()
  "Search forward for next bibliography entry missing a `URL' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*URL[ \t]*="))


(defun find-missing-abstract ()
  "Search forward for next bibliography entry missing an
`abstract' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*abstract[ \t]*="))


(defun find-missing-acknowledgement ()
  "Search forward for next bibliography entry missing an
`acknowledgement' value."
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*acknowledgement[ \t]*="))


(defun find-missing-address ()
  "Search forward for next bibliography entry missing a `address' value"
  (interactive)
  (find-missing-xxx "^@Book\\|^@Manual\\|^@MastersThesis\\|^@PhdThesis\\|^@Periodical\\|^@Proceedings\\|^@TechReport\\|^@Unpublished"
		    "^[ \t]*address[ \t]*="))


(defun find-missing-annote ()
  "Search forward for next bibliography entry missing an `annote' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*annote[ \t]*="))


(defun find-missing-author ()
  "Search forward for next bibliography entry missing a `author' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*author[ \t]*="))


(defun find-missing-bibdate ()
  "Search forward for next bibliography entry missing a `bibdate' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*bibdate[ \t]*="))


(defun find-missing-bibsource ()
  "Search forward for next bibliography entry missing a `bibsource' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*bibsource[ \t]*="))


(defun find-missing-booktitle ()
  "Search forward for next bibliography entry missing a `booktitle' value"
  (interactive)
  (find-missing-xxx "^@Proceedings" "^[ \t]*booktitle[ \t]*="))


(defun find-missing-country ()
  "Search forward for next bibliography entry missing a `country' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*country[ \t]*="))


(defun find-missing-chapter ()
  "Search forward for next bibliography entry missing a `chapter' value."
  (interactive)
  (find-missing-xxx
   "^@InBook\\|^@InCollection" "^[ \t]*chapter[ \t]*="))


(defun find-missing-crossref ()
  "Search forward for next bibliography entry missing a `crossref' value"
  (interactive)
  (find-missing-xxx
   "^@InBook\\|^@InCollection\\|^@InProceedings" "^[ \t]*crossref[ \t]*="))


(defun find-missing-day ()
  "Search forward for next bibliography entry missing an `day' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*day[ \t]*="))


(defun find-missing-edition ()
  "Search forward for next bibliography entry missing a `edition' value."
  (interactive)
  (find-missing-xxx
   "^@Book\\|^@InBook\\|^@InCollection\\|^@Manual" "^[ \t]*edition[ \t]*="))


(defun find-missing-editor ()
  "Search forward for next Proceedings bibliography entry missing an
`editor' value."
  (interactive)
  (find-missing-xxx "^@Proceedings" "^[ \t]*editor[ \t]*="))


(defun find-missing-howpublished ()
  "Search forward for next bibliography entry missing a `howpublished' value."
  (interactive)
  (find-missing-xxx
   "^@Booklet\\|^@Misc" "^[ \t]*howpublished[ \t]*="))


(defun find-missing-institution ()
  "Search forward for next bibliography entry missing a `institution' value"
  (interactive)
  (find-missing-xxx
   "^@TechReport" "^[ \t]*institution[ \t]*="))


(defun find-missing-journal ()
  "Search forward for next bibliography entry missing a `journal' value"
  (interactive)
  (find-missing-xxx "^@Article" "^[ \t]*journal[ \t]*="))


(defun find-missing-keywords ()
  "Search forward for next bibliography entry missing a `keywords' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*keywords[ \t]*="))


(defun find-missing-month ()
  "Search forward for next bibliography entry missing a `month' value"
  (interactive)
  (find-missing-xxx "^@Article" "^[ \t]*month[ \t]*="))


(defun find-missing-mrclass ()
  "Search forward for next bibliography entry missing a `MRclass' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*MRclass[ \t]*="))


(defun find-missing-mrnumber ()
  "Search forward for next bibliography entry missing a `MRnumber' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*MRnumber[ \t]*="))


(defun find-missing-note ()
  "Search forward for next bibliography entry missing a `note' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*note[ \t]*="))


(defun find-missing-number ()
  "Search forward for next bibliography entry missing a `number' value"
  (interactive)
  (find-missing-xxx "^@Article" "^[ \t]*number[ \t]*="))


(defun find-missing-organization ()
  "Search forward for next bibliography entry missing a `organization' value"
  (interactive)
  (find-missing-xxx
   "^@Manual" "^[ \t]*organization[ \t]*="))


(defun find-missing-pages ()
  "Search forward for next bibliography entry missing a `pages' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*pages[ \t]*="))


(defun find-missing-publisher ()
  "Search forward for next bibliography entry missing a `publisher' value"
  (interactive)
  (find-missing-xxx "^@Book\\|^@Proceedings" "^[ \t]*publisher[ \t]*="))


(defun find-missing-school ()
  "Search forward for next bibliography entry missing a `school' value"
  (interactive)
  (find-missing-xxx
   "^@MastersThesis\\|^@PhdThesis" "^[ \t]*school[ \t]*="))


(defun find-missing-series ()
  "Search forward for next bibliography entry missing a `series' value."
  (interactive)
  (find-missing-xxx
   "^@Book\\|^@InBook\\|^@InCollection\\|^@InProceedings\\|^@Proceedings"
   "^[ \t]*series[ \t]*="))


(defun find-missing-subject ()
  "Search forward for next bibliography entry missing a `subject' value."
  (interactive)
  (find-missing-xxx
   "^@" "^[ \t]*subject[ \t]*="))


(defun find-missing-title ()
  "Search forward for next bibliography entry missing a `title' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*title[ \t]*="))


(defun find-missing-type ()
  "Search forward for next bibliography entry missing a `type' value"
  (interactive)
  (find-missing-xxx "^@InBook\\|^@InCollection\\|^@MastersThesis\\|^@PhdThesis\\|^@TechReport"
		    "^[ \t]*type[ \t]*="))


(defun find-missing-volume ()
  "Search forward for next bibliography entry missing a `volume' value"
  (interactive)
  (find-missing-xxx "^@Article" "^[ \t]*volume[ \t]*="))


(defun find-missing-xxx (entry-pattern key-pattern)
  "Search forward for the next bibliography entry matching ENTRY-PATTERN
that is missing a value for KEY-PATTERN.  Return T if a missing value is
identified, and otherwise, terminate with an error, so that this
function will safely stop a keyboard macro with an infinite repeat
count, or an infinite loop.  In either case, point is left at the end of
the last entry examined."
  (let ((end nil)
	(have-key-pattern t)
	(start nil)
	(case-fold-search t))
    (while (and have-key-pattern (re-search-forward entry-pattern nil t))
      (setq start (point))
      (re-search-forward "^[ \t]*}[ \t]*\n") ;NB: OK if this terminates with an error
      (setq end (point))
      (goto-char start)
      (if (not (re-search-forward key-pattern end t))
	  (setq have-key-pattern nil))
      (goto-char end))
    (if have-key-pattern
	(error "Failing find-missing-%s" (gsub "[^a-zA-Z0-9_-]" "" key-pattern))
      t)))


(defun find-missing-year ()
  "Search forward for next bibliography entry missing a `year' value"
  (interactive)
  (find-missing-xxx "^@" "^[ \t]*year[ \t]*="))


(defun find-similar-labels ()
  "Search forward for consecutive bibliography entries with similar
BibNet-style citation labels, matching in the author name and year."
  (interactive)
  (catch 'DONE
    (let ((label "") (pos nil))
      (while (re-search-forward "^@[A-Za-z]+{\\([^:]*:[12][0-9][0-9][0-9]:\\)" nil t)
	(setq label (buffer-substring (match-beginning 1) (match-end 1)))
	(setq pos (match-beginning 0))
	(if (and (re-search-forward "^@[A-Za-z]+{" nil t)
		 (looking-at label))
	    (progn
	      (push-mark pos)
	      (throw 'DONE nil)))
	(goto-char (1+ pos)))
      (error "[done]"))))


(defun find-tex-math-item ()
  "Find the next isolated letter that is a candidate for conversion
to TeX math mode."
  (interactive)
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (catch 'DONE
      (while (re-search-forward "[$]\\|[^a-zA-Z0-9'\"-][b-zB-HJ-Z][^a-zA-Z0-9.'-]\\|[^a-zA-Z0-9'-][b-zB-HJ-Z]$")
	(goto-char (match-beginning 0))
	(if (looking-at "[$]")
	    (progn
	      (goto-char (match-end 0))
	      (re-search-forward "[$]"))
	  (goto-char (match-beginning 0))
	  (throw 'DONE t)))
      (goto-char (match-end 0))
      nil)
    (push-mark (point))
    (setq case-fold-search old-case-fold-search)))


(defun fix-accents-with-embedded-spaces ()
  "Convert TeX accents with embedded spaces to equivalent braced form."
  (interactive)
  (goto-char (point-min))
  (query-replace-regexp "\\(\\\\[uvcHtdb]\\) +\\([^\\]\\)" "{\\1{\\2}}")
  (goto-char (point-min))
  (query-replace-regexp "\\(\\\\[uvcHtdb]\\) +\\(\\\\[ij] *\\)" "{\\1{\\2}}")
  (goto-char (point-min))
  (query-replace-regexp "\\(\\\\[l]\\) +" "{\\1}")
  (goto-char (point-min))
  (query-replace-regexp "\\\\oe +" "{\\\\oe}")
  (goto-char (point-min))
  (query-replace-regexp "\\\\u\\\\i +" "{\\\\u{\\\\i}} ")
  (goto-char (point-min))
  (query-replace-regexp "\\(\\\\polhk\\) +\\([^\\]\\)" "{\\\\k{\\2}}")
  (goto-char (point-min))
  (query-replace-regexp "\\\\cprime +" "\\\\cprime{}")
  (goto-char (point-min))
  (query-replace-regexp "\\\\cydot +" "\\\\cydot{}"))


(defun fix-acknowledgements ()
  "Regularize the spacing of @String{ack-xyz = \"...\"} entries in a
BibTeX .bib file."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]" nil t)
    (delete-horizontal-space)
    (insert-char ?\  20))

  (goto-char (point-min))
  (while (re-search-forward "=[ \t]*" nil t)
    (backward-char 1)
    (delete-horizontal-space)
    (insert-char ?\  (- 20 (current-column)))))


(defun fix-bibtex-strings ()
  "Narrow the buffer to the region containing only BibTeX @String{...}
abbreviations, then sort them into alphabetical order with duplicates
eliminated, line wrap long ones, and then widen the buffer to the
bounds at entry."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((start) (end))
	(goto-char (point-min))
	(if (re-search-forward "^@[Ss][Tt][Rr][Ii][Nn][Gg]{" nil t)
	    (progn
	      (beginning-of-line)
	      (setq start (point))
	      (if (re-search-forward "^@[^Ss]" nil t)
		  (beginning-of-line)
		(goto-char (point-max)))
	      (re-search-backward "^@[Ss][Tt][Rr][Ii][Nn][Gg]{")
	      (re-search-forward "\" *} *$")
	      (forward-line 1)
	      (narrow-to-region start (point))

	      (goto-char (point-min))
	      (bibtex-replace-regexp " *\n +" " ")

	      (goto-char (point-min))
	      (bibtex-replace-regexp "  +" " ")

	      (goto-char (point-min))
	      (mark-whole-buffer)
	      (sort-lines nil (point-min) (point-max))

	      (goto-char (point-min))
	      (flush-duplicate-lines)

	      (goto-char (point-min))
	      (wrap-long-strings)))))))


(defun fix-braced-quotes ()
  "Change ``{...}'' to {``...''} in all BibTeX entries from point to
end of buffer to standardize appearance."
  (interactive)
  (let ((start))
      (while (search-forward "``{" nil t)
	(setq start (- (point) 3))
	(search-forward "''")
	(narrow-to-region start (point))
	(goto-char (point-min))
	(if (search-forward "\\")
	    (error "TeX control sequence: convert this entry manually"))
	(goto-char (point-min))
	(bibtex-replace-regexp "[{}]" "")
	(goto-char (point-min))
	(insert "{")
	(goto-char (point-max))
	(insert "}")
	(widen))))


(defun fix-conf-date ()
  "Change hyphen to en-dash in conf_date entries."
  (interactive)
  (while (re-search-forward "conf_?date =" nil t)
    (re-search-forward "[0-9]-[0-9]\\|\",")
    (backward-char 3)
    (if (looking-at "[0-9]-[0-9]")
	(progn
	  (forward-char 1)
	  (insert "-")))))


(defun fix-duplicate-MRclass ()
  "Merge adjacent MRclass key/value pairs into a single MRclass
line.  If more than two such lines are adjacent, this function must
be run repeatedly."
  (interactive)
  (bibtex-replace-regexp
   "\\(MRclass *= *\"\\)\\([^\"]*\\)\",\n *\\(MRclass *= *\"\\)\\([^\"]*\\)\","
   "\\1\\2; \\4\","))


(defun fix-duplicate-MRnumber ()
  "Merge adjacent MRnumber key/value pairs into a single MRnumber
line.  If more than two such lines are adjacent, this function must
be run repeatedly."
  (interactive)
  (bibtex-replace-regexp
   "\\(MRnumber *= *\"\\)\\([^\"]*\\)\",\n *\\(MRnumber *= *\"\\)\\([^\"]*\\)\","
   "\\1\\2; \\4\","))


(defun fix-duplicate-URL ()
  "Merge adjacent URL key/value pairs into a single URL
line.  If more than two such lines are adjacent, this function must
be run repeatedly."
  (interactive)
  (bibtex-replace-regexp
   "\\(URL *= *\"\\)\\([^\"]*\\)\",\n *\\(URL *= *\"\\)\\([^\"]*\\)\","
   "\\1\\2; \\4\","))


(defun fix-duplicate-annote ()
  "Merge adjacent annote key/value pairs into a single annote line.
If more than two such lines are adjacent, this function must be run
repeatedly."
  (interactive)
  (bibtex-replace-regexp
   "\\(annote *= *\"\\)\\([^\"]*\\)\",\n *\\(annote *= *\"\\)\\([^\"]*\\)\","
   "\\1\\2; \\4\","))


(defun fix-duplicate-bibdate ()
  "Find the next instance of duplicate bibdate key/value pairs, and keep the
most recent one.  Date fields should take one of the two forms:
	\"Wed Jul 6 15:27:50 1994\"
	\"Wed Jul 6 15:27:50 MDT 1994\"
The time zone is ignored in comparing dates."
  (interactive)
  (let ((key1) (key2) (start1) (start2) (day) (month) (year) (hhmmss) (data))
    (while (re-search-forward
	    "^ *bibdate = *\"[A-Z][a-z][a-z] \\([A-Z][a-z][a-z]\\) \\([0-9]+\\) \\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\) \\([A-Z][A-Z][A-Z]\\)? *\\([12][0-9][0-9][0-9]\\)\"," nil t)
      (setq start1 (match-beginning 0))
      (setq key1 (bibdate-to-isodate))
      (re-search-forward
       "^ *bibdate = *\"[A-Z][a-z][a-z] \\([A-Z][a-z][a-z]\\) \\([0-9]+\\) \\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\) \\([A-Z][A-Z][A-Z]\\)? *\\([12][0-9][0-9][0-9]\\)\",\\|^ *}")
      (setq data (match-data))		;save because looking-at will destroy match info
      (beginning-of-line)
      (if (looking-at "^ *}")
	  nil
	(setq start2 (match-beginning 0))
	(store-match-data data)		;restore match info
	(setq key2 (bibdate-to-isodate))
	(cond
	 ((string-lessp key1 key2)
	  (goto-char start1)
	  (re-search-forward "\",\\|$")
	  (kill-region start1 (point))
	  (delete-blank-lines)
	  (re-search-backward "^ *@"))
	 ((string-less-or-equalp key2 key1)
	  (goto-char start2)
	  (re-search-forward "\",\\|$")
	  (kill-region start2 (point))
	  (delete-blank-lines)
	  (re-search-backward "^ *@")))))))


(defun fix-duplicate-bibsource ()
  "Merge adjacent bibsource key/value pairs into a single bibsource
line.  If more than two such lines are adjacent, this function must
be run repeatedly."
  (interactive)
  (bibtex-replace-regexp
   "\\(bibsource *= *\"\\)\\([^\"]*\\)\",\n *\\(bibsource *= *\"\\)\\([^\"]*\\)\","
   "\\1\\2; \\4\","))


(defun fix-duplicate-classification ()
  "Merge duplicate classification lines into single ones."
  (interactive)
  (query-replace-regexp
     "\\(classification *= *\"\\)\\([^\"]*\\)\",\n *\\(classification *= *\"\\)\\([^\"]*\\)\","
     "\\1\\2; \\4\","))


(defun fix-duplicate-keywords ()
  "Merge duplicate keywords lines into single ones."
  (interactive)
  (query-replace-regexp
     "\\(keywords *= *\"\\)\\([^\"]*\\)\",\n *\\(keywords *= *\"\\)\\([^\"]*\\)\","
     "\\1\\2; \\4\","))


(defun fix-duplicate-labels ()
  "Given a BibTeX buffer sorted by labels, find duplicate labels and
append a unique letter to them to remove the duplications.  A sorted
buffer is required in order to obtain an acceptable run time (O(N) instead
of O(N^2))."
  (interactive)
  (let ((first-label "")
	(duplicate-count)
	(entry-regexp "^\\(@[A-Za-z]+{\\)\\([^,]+\\),"))
    (while (re-search-forward entry-regexp nil t)
      (setq first-label (buffer-substring (match-beginning 2) (match-end 2)))
      (setq duplicate-count 0)
      (backward-char 1)
      (save-excursion
	(catch 'LOOP-EXIT
	  (while (re-search-forward entry-regexp nil t)
	    (if (string-equal
		 first-label (buffer-substring (match-beginning 2) (match-end 2)))
		(progn
		  (setq duplicate-count (1+ duplicate-count))
		  (if (> duplicate-count 25)
		      (error "More than 26 suffixes needed!"))
		  (goto-char (match-end 2))
		  (insert (substring "abcdefghijklmnopqrstuvwxyz"
				     duplicate-count (1+ duplicate-count))))
	      (throw 'LOOP-EXIT nil)))))
      (if (> duplicate-count 0)		;supply suffix on the first of the duplicates
	  (insert "a"))
      (setq duplicate-count 0))))


(defun fix-duplicate-searchkey ()
  "Merge duplicate searchkey lines into single ones."
  (interactive)
  (query-replace-regexp
     "\\(searchkey *= *\"\\)\\([^\"]*\\)\",\n *\\(searchkey *= *\"\\)\\([^\"]*\\)\","
     "\\1\\2 | \\4\","))


(defun fix-duplicate-subject ()
  "Find entries with two subject keywords, and delete the second
subject key/value pair."
  (interactive)
  (let ((start))
    (while (re-search-forward "^ *subject *=" nil t)
      (setq start (point))
      (re-search-forward "^ *subject *=\\|^ *}" nil t)
      (beginning-of-line)
      (if (looking-at "^ *subject *=")
	  (progn
	    (setq start (point))
	    (re-search-forward "\", *\n")
	    (delete-region start (point)))))))


(defun fix-elided-final-page-number ()
  "In a BibTeX bibliography file, find page ranges where common
leading digits have been omitted from the final page number (e.g.,
219--33, meaning 219--233), and supply them.  For safety, the ending
page number is then compared against the starting page number, and if
it is lexically smaller, an error is raised."
  (interactive)
  (let ((startpage 0) (endpage 0))
    (while (re-search-forward "^ *pages *= *\"\\([0-9]+\\)--\\([0-9]+\\)\"" nil t)
      (setq startpage (buffer-substring (match-beginning 1) (match-end 1)))
      (setq endpage (buffer-substring (match-beginning 2) (match-end 2)))
      (if (> (length startpage) (length endpage))
	  (progn
	    (goto-char (match-beginning 2))
	    (insert (substring startpage 0 (- (length startpage) (length endpage))))
	    (goto-char (match-beginning 2))
	    (setq endpage (buffer-substring (point) (+ (point) (length startpage))))
	    (if (string-lessp endpage startpage)
		(error "Ending page smaller than starting page")))))))


(defun fix-greek-letter-names ()
  "Find Greek letter names and convert them to TeX control sequences."
  (interactive)
  (query-replace-regexp "\\([^\\a-zA-Z]\\)\\(alpha\\|beta\\|gamma\\|delta\\|epsilon\\|zeta\\|eta\\|theta\\|iota\\|kappa\\|lambda\\|mu\\|nu\\|xi\\|omicron\\|pi\\|rho\\|sigma\\|tau\\|upsilon\\|phi\\|chi\\|psi\\|omega\\)\\([^a-zA-Z]\\)"
			"\\1\\\\\\2\\3"))


(defun fix-mathscinet-accents ()
  "Replace 8-bit MathSciNet accents in 128..255 with TeX equivalents."
  (interactive)
  (goto-char (point-min))
  (bibtex-replace-regexp "\\\\v \\([A-Za-z]\\)" "{\\\\v{\\1}}")
  (goto-char (point-min))
  (bibtex-replace-string "é" "{\\'e}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "è" "{\\`e}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "à" "{\\`a}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ä" "{\\\"a}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "Ö" "{\\\"O}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "É" "{\\'E}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ü" "{\\\"u}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ý" "{\\'y}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ø" "{\\o}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ó" "{\\'o}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ö" "{\\\"o}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "è" "{\\`e}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ç" "{\\c{c}}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "á" "{\\'a}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "È" "{\\`E}" nil)
  (goto-char (point-min))
  (bibtex-replace-string "ú" "{\\'u}" nil)
  )


(defun fix-missing-booktitle ()
  "Find Proceedings entries that are missing a booktitle entry,
and supply it from their title entries."
  (interactive)
  (while (find-missing-booktitle)
    (re-search-backward "^@\\|^ *title *= *\"")
    (if (looking-at "^ *title *= *\"")
	(let ((start (point)) (end))
	  (re-search-forward "\", *\n")
	  (setq end (point))
	  (goto-char start)
	  (insert (buffer-substring start end))
	  (goto-char start)
	  (back-to-indentation)
	  (insert "book")
	  (search-forward "=")
	  (delete-horizontal-space)
	  (insert "    ")))))


(defun fix-missing-italic-correction ()
 "Find {\\em ...} groups that need an italic correction, and supply it."
  (interactive)
  (while (re-search-forward "{\\\\\\(em\\|it\\)[ \n]" nil t)
    (re-search-forward "[^{}]}")
    (backward-char 1)
    (if (not (looking-at "}[.,]"))
	(insert "\\/"))))


(defun fix-missing-month ()
  "Given a journal with monthly issues, find entries from point to end
of buffer with an issue number, but missing the month value, and
supply the missing month."
  (interactive)
  (while (< (point) (point-max))
    (re-search-forward "^  number = *\"[0-9]+\"")
    (backward-word 1)
    (let ((start (point)) (number))
      (forward-word 1)
      (setq number (string-to-int (buffer-substring start (point))))
      (if (or (< number 1) (> number 12))
	  (error "Issue number out of range 1..12"))
      (re-search-forward "^  year =")
      (beginning-of-line)
      (insert "  month =        "
	      (nth (1- number) '("jan" "feb" "mar" "apr" "may" "jun"
				 "jul" "aug" "sep" "oct" "nov" "dec"))
	      ",\n"))))


(defun fix-month-names ()
  "Change month names to BibTeX standard abbreviations in the current
buffer.  Quoted and unquoted long names, and bracketed numbers, are
recognized, and letter case is ignored."
  (interactive)
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search t)

    (internal-fix-month-names "\"January\""   "jan")
    (internal-fix-month-names "\"February\""  "feb")
    (internal-fix-month-names "\"March\""     "mar")
    (internal-fix-month-names "\"April\""     "apr")
    (internal-fix-month-names "\"May\""       "may")
    (internal-fix-month-names "\"June\""      "jun")
    (internal-fix-month-names "\"July\""      "jul")
    (internal-fix-month-names "\"August\""    "aug")
    (internal-fix-month-names "\"September\"" "sep")
    (internal-fix-month-names "\"October\""   "oct")
    (internal-fix-month-names "\"November\""  "nov")
    (internal-fix-month-names "\"December\""  "dec")

    (internal-fix-month-names "January"       "jan")
    (internal-fix-month-names "February"      "feb")
    (internal-fix-month-names "March"         "mar")
    (internal-fix-month-names "April"         "apr")
    (internal-fix-month-names "May"           "may")
    (internal-fix-month-names "June"          "jun")
    (internal-fix-month-names "July"          "jul")
    (internal-fix-month-names "August"        "aug")
    (internal-fix-month-names "September"     "sep")
    (internal-fix-month-names "October"       "oct")
    (internal-fix-month-names "November"      "nov")
    (internal-fix-month-names "December"      "dec")

    (internal-fix-month-names "[1]"           "jan")
    (internal-fix-month-names "[2]"           "feb")
    (internal-fix-month-names "[3]"           "mar")
    (internal-fix-month-names "[4]"           "apr")
    (internal-fix-month-names "[5]"           "may")
    (internal-fix-month-names "[6]"           "jun")
    (internal-fix-month-names "[7]"           "jul")
    (internal-fix-month-names "[8]"           "aug")
    (internal-fix-month-names "[9]"           "sep")
    (internal-fix-month-names "[10]"          "oct")
    (internal-fix-month-names "[11]"          "nov")
    (internal-fix-month-names "[12]"          "dec")

    (setq case-fold-search old-case-fold-search)))


(defun fix-numeric-months ()
  "Change (possibly bracketed) numbered months to named months."
  (interactive)
  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?1\\]?\","
			 "month =        jan,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?2\\]?\","
			 "month =        feb,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?3\\]?\","
			 "month =        mar,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?4\\]?\","
			 "month =        apr,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?5\\]?\","
			 "month =        may,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?6\\]?\","
			 "month =        jun,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?7\\]?\","
			 "month =        jul,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?8\\]?\","
			 "month =        aug,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?9\\]?\","
			 "month =        sep,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?10\\]?\","
			 "month =        oct,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?11\\]?\","
			 "month =        nov,")

  (goto-char (point-min))
  (bibtex-replace-regexp "month *= *\"\\[?12\\]?\","
			 "month =        dec,"))


(defun fix-old-style-bracing ()
  "Convert old-style BibTeX braced title word {J}ones to new-style {Jones}."
  (interactive)
  (setq case-fold-search nil)
  (save-excursion
    (query-replace-regexp "{\\([A-Z]+\\)}\\([a-z\\-]+\\)" "{\\1\\2}"))
  (query-replace-regexp "\\([A-Za-z]+\\){\\([A-Z]+\\)}\\([a-z\\-]*\\)" "{\\1\\2\\3}"))


(defun get-string (&optional no-show-string)
  "Extract a string around point, terminating at surrounding quotation
marks, or HTML tags, and leave it in the kill ring after discarding
any leading and trailing whitespace.  If the optional argument
NO-SHOW-STRING is non-nil, do not print the string in the echo area."
  (interactive)
  (let ((start (point)) (end-regexp) (first) (last) (the-string))
    (re-search-backward ">[ \n\t]*\\|\"[ \n\t]*") ; find opening delimiter
    (if (string-equal "\"" (buffer-substring (point) (1+ (point))))
	(setq end-regexp "[ \n\t]*\"")
      (setq end-regexp "[ \n\t]*</[^>]*>"))
    (goto-char (match-end 0))
    (setq first (point))
    (re-search-forward end-regexp)	; find closing delimiter

    ;; Now convert any embedded newlines to space.  Prior to emacs
    ;; version 19, we can only copy from the current buffer to the kill
    ;; ring, so by changing newlines to spaces, we modify the buffer,
    ;; which is undesirable.  In emacs 19 or later, we can copy from a
    ;; string to the kill ring, thereby leaving the buffer intact.
    (goto-char first)
    (if (string-lessp (substring emacs-version 0 2) "19") ; earlier than 19.x
	(progn
	  (setq last (match-beginning 0))
	  (while (re-search-forward "\\( *[\n\t ] *\\)+" last t)
	    (replace-match " " nil t))
	  (copy-region-as-kill first last)
	  (setq the-string (buffer-substring first last)))
      (setq the-string (gsub "\\( *[\n\t ] *\\)+" " "
			     (buffer-substring first (match-beginning 0))))
      (kill-new the-string))
    (goto-char start)
    (if (not no-show-string)
	(princ the-string))))


(defun gsub (regexp replacement the-string)
  "Replace all occurrences of REGEXP by REPLACEMENT in THE-STRING, and
return a new string with the result."
  (interactive)
  (let ((new-string nil))
    (save-excursion
      (set-buffer (get-buffer-create "*gsub*"))
      (insert the-string)
      (goto-char (point-min))
      ;; [12-May-2012] force case-sensitive matching: case-fold-search
      ;; can be clobbered somewhat higher in the call chain, notably,
      ;; in bibtex-first-author-name, resulting in the call
      ;;     (gsub "[ ,]+[IVX]+$" "" "Able Baker Xi")
      ;; incorrectly returning "Able Baker" instead of the correct
      ;; "Able Baker Xi"
      (let ((case-fold-search nil))
	(while (re-search-forward regexp nil t)
	  (replace-match replacement nil nil)))
      (setq new-string (buffer-substring (point-min) (point-max))))
    (kill-buffer "*gsub*")
    new-string))


;;; The internal-LaTeX-default function is borrowed from latex.el, so we
;;; need not load that big file.
(defun internal-LaTeX-default (value default-value)
  "Return a non-nil value from VALUE, current-prefix-arg, or
DEFAULT-VALUE, in that order.  Do

   (setq argvar (internal-LaTeX-default argvar 23))

to set &optional argvar to (a) argvar, if argvar is non-nil, else (b)
current-prefix-arg, if that is non-nil, else (c) 23.  This simplifies
optional argument assignment in many functions."
  (cond
   ((not (null value))			;do nothing if explicit arg
    value)
   ((not (null current-prefix-arg))	;else use prefix arg if any
    (prefix-numeric-value current-prefix-arg))
   (t					;else no arg at all, so use default
    default-value)))

;;; The internal-LaTeX-wrap function is borrowed from latex.el, so we
;;; need not load that big file.
(defun internal-LaTeX-wrap (prefix suffix word-count)
  "Wrap with PREFIX and SUFFIX the next WORD-COUNT words."
  (if (> word-count 0)			;do something only if word-count > 0
      (progn
	(forward-word 1)
	(forward-word -1)		;position to start of next word
	(insert prefix)
	(forward-word word-count)
	(insert suffix))))


(defun internal-fix-month-names (old new)
  "Change month names to BibTeX standard abbreviations in the
current buffer.  This is a private internal function."
  (goto-char (point-min))
  (bibtex-replace-regexp
   (concat "^\\( *month *= *\\)\\(" old ",\\)")
   (concat "\\1" new ",")))


(defun internal-month-number (abbrev)
  "Return a month number corresponding to the 3-letter month ABBREV."
  (cond
   ((string-equal abbrev "Jan") 1)
   ((string-equal abbrev "Feb") 2)
   ((string-equal abbrev "Mar") 3)
   ((string-equal abbrev "Apr") 4)
   ((string-equal abbrev "May") 5)
   ((string-equal abbrev "Jun") 6)
   ((string-equal abbrev "Jul") 7)
   ((string-equal abbrev "Aug") 8)
   ((string-equal abbrev "Sep") 9)
   ((string-equal abbrev "Oct") 10)
   ((string-equal abbrev "Nov") 11)
   ((string-equal abbrev "Dec") 12)
   (t 99)))


(defun isbn-checkdigit (isbn)
  "Given an ISBN number as a string argument, print the
expected check digit, according to the formula
        (sum(k=1:9) digit(k) * k) mod 11 == digit(10)
where X as a digit has value 10, and hyphens are ignored, and
nil otherwise."
  (interactive "sISBN: ")
  (let ((sum 0) (k 0) (c 0) (pos 1) (result nil))
    (while (< k (length isbn))
      (setq c (isbn-digit (aref isbn k)))
      (setq k (1+ k))
      (cond
       ((and c (= pos 10))
        (setq result (= (mod sum 11) c) pos (1+ pos)))
       ((and c (< pos 10))
        (setq sum (+ sum (* c pos)) pos (1+ pos)))
       ))
    (if (= (mod sum 11) 10)
	(princ "X")
      (princ (mod sum 11)))))


(defun make-abstract ()
  "Convert an article abstract in the current region of an IBM JRD or
SYSJ .txt file converted to plain text by a WWW browser from an HTML
file at http://www.almaden.ibm.com/journal/ to a suitable BibTeX
abstract entry.  This function should also be usable for abstract
text found in other contexts."
  (interactive)
  (save-restriction
    (setq fill-column 72)
    (narrow-to-region (point) (mark))

    ;; Ignore any leading whitespace
    (goto-char (point-min))
    (re-search-forward "[^ \n]")
    (narrow-to-region (1- (point)) (mark))

    ;; Escapify special TeX characters
    (goto-char (point-min))
    (bibtex-replace-string "%" "\\%")

    (goto-char (point-min))
    (bibtex-replace-string "&" "\\&")

    (goto-char (point-min))
    (bibtex-replace-string "#" "\\#")

    ;; Convert "xxx" to ``xxx''
    (goto-char (point-min))
    (while (search-forward "\"" nil t)
      (backward-delete-char 1)
      (insert "``")
      (search-forward "\"")
      (backward-delete-char 1)
      (insert "''") )

    ;; Trim space
    (goto-char (point-min))
    (blank-trim-lines)

    ;; Insert indentation
    (goto-char (point-min))
    (bibtex-replace-regexp "^" "                 ")

    ;; Supply keyword assignment
    (goto-char (point-min))
    (insert             "  abstract =     \"")
    (delete-horizontal-space)

    ;; Wrap first line of string value at column 72
    (end-of-line)
    (if (> (current-column) 72)
	(progn
	  (while (> (current-column) 72)
	    (backward-word 1))
	  (insert "\n                 "))
      (forward-line 1))

    ;; Ignore trailing space at end of region, close string, and fill
    ;; string
    (let ((start (point)))
      (goto-char (point-max))
      (delete-horizontal-space)
      (if (looking-at "^")
	  (backward-delete-char 1))
      (insert "\",")
      (fill-region start (point)))

    ;; Save for copying to .bib file buffer
    (copy-region-as-kill (point-min) (point-max))))


(defun make-lc-isbn-lookup-command ()
  "Find the next BibTeX entry with a missing LCCN value, and then use
its ISBN number to construct in the X primary selection buffer a
Library of Congress catalog lookup command."
  (interactive)
  (let ((start) (LCCN-pos) (ISBN-string) (temp-syntax-table))
    (x-set-selection 'PRIMARY nil)	;forget current selection
    (if (re-search-forward "^[ \t]*LCCN[ \t]*=[ \t]*\"[?\"]" nil t)
	(progn
	  (setq LCCN-pos (1- (point)))
	  (modify-syntax-entry ?- "w")
	  (if (re-search-backward "^[ \t]*ISBN[ \t]*=[ \t]*\"\\|^@" nil t)
	      (progn
		(setq start (match-end 0))
		(if (looking-at "^[ \t]*ISBN *= *\"[?]*\"")
		    (progn
		      (re-search-forward "^ *}")
		      (error "No ISBN available.")))
		(if (looking-at "^[ \t]*ISBN")
		    (progn
		      (setq temp-syntax-table (syntax-table))
		      (with-syntax-table temp-syntax-table
			(modify-syntax-entry ?\- "word") ; hyphens are word charas
			(goto-char start)
			(forward-word 1)
			;; NB: Want extra newline at start of
			;; ISBN-string to synchronize catalog software
			(setq ISBN-string (concat "\nISBN "
						  (buffer-substring start (point))
						  "\n"))
			(x-set-selection 'PRIMARY ISBN-string)
			(x-set-cut-buffer ISBN-string)
			(goto-char LCCN-pos))))))))
    (message "[%s]" (if ISBN-string
			(substring ISBN-string 0 (1- (length ISBN-string)))
		      "done"))))


(defun make-lc-issn-lookup-command ()
  "Make a Library of Congress Contents1st catalog ISSN lookup command
for the word in which point lies."
  (interactive)
  (x-set-selection 'PRIMARY nil)	;forget current selection
  (let ((ISSN-string) (start) (temp-syntax-table))
    (setq temp-syntax-table (syntax-table))
    (with-syntax-table temp-syntax-table
      (modify-syntax-entry ?\- "word")	; hyphens are word charas
      (forward-word 1)
      (backward-word 1)
      (setq start (point))
      (forward-word 1)
      (setq ISSN-string
	    (concat "ISSN " (buffer-substring start (point)) "\n"))
      (message (substring ISSN-string 0 (1- (length ISSN-string))))
      (x-set-selection 'PRIMARY ISSN-string)
      (x-set-cut-buffer ISSN-string)
      )))


;;; ====================================================================
;;;      University of California MELVYL library catalog support
;;;
;;; MELVYL is typical of most Internet library catalogs, in that the
;;; information displayed somewhat resembles that on an old library
;;; index card.  In particular, the absence of markup makes it
;;; impossible for a computer to reliably recognize the various parts
;;; (author, title, publisher, ...) of an entry.
;;;
;;; Fortunately, MELVYL allows some control over what fields are
;;; displayed, and this in turn allows a computer program to recognize
;;; them and convert them to something much more useful: a BibTeX
;;; entry.
;;;
;;; To use the MELVYL catalog, first initialize session logging,
;;; either with a terminal emulator log capability, or with the UNIX
;;; script command, so that you can automatically record the results
;;; of your search.
;;;
;;; If you have only a few entries to look up, then you may be
;;; satisfied with window-system cut-and-paste facilities to copy
;;; search results into an Emacs buffer.
;;;
;;; Telnet to melvyl.berkeley.edu, specify `vt100' (if appropriate:
;;; closest to xterm for workstation users) at the terminal prompt,
;;; then `start cat' to start in the main catalog.  Select the output
;;; fields expected by this code with the commands
;;;
;;;	set display title author isbn publisher
;;;	set paging continuous
;;;
;;; Now you can do searches with commands like
;;;
;;; 	find author stallman, richard m
;;; 	fi au stallman, r m
;;;	fi isbn 1-882114-03-5
;;;	fi tw emacs
;;;
;;; ====================================================================


(defun make-melvyl-coden-lookup-command ()
  "Make a University of California Melvyl catalog CODEN lookup command
for the word in which point lies."
  (interactive)
  (x-set-selection 'PRIMARY nil)	;forget current selection
  (let ((CODEN-string) (start))
    (forward-word 1)
    (backward-word 1)
    (setq start (point))
    (forward-word 1)
    (setq CODEN-string
	  (concat "find CODEN " (buffer-substring start (point)) "\n"))
    (message (substring CODEN-string 0 (1- (length CODEN-string))))
    (x-set-selection 'PRIMARY CODEN-string)
    (x-set-cut-buffer CODEN-string)
    ))


(defun make-melvyl-isbn-lookup-command ()
  "Find the next BibTeX entry with a missing LCCN value, and then use
its ISBN number to construct in the X primary selection buffer a
University of California MELVYL catalog lookup command."
  (interactive)
  (make-lc-isbn-lookup-command)		;make "ISBN xxxxxxx" command
  (let ((ISBN-string
	 (concat "find "
		 (substring (x-get-selection  'PRIMARY 'STRING) 1))))
    (x-set-cut-buffer ISBN-string)
    (x-set-selection 'PRIMARY ISBN-string)
    (message "[%s]" (if ISBN-string
			(substring ISBN-string 0 (1- (length ISBN-string))) "done"))))


(defun make-melvyl-issn-lookup-command ()
  "Make a University of California Melvyl catalog ISSN lookup command
for the word in which point lies."
  (interactive)
  (x-set-selection 'PRIMARY nil)	;forget current selection
  (let ((ISSN-string) (start))
    (forward-word 1)
    (backward-word 1)
    (setq start (point))
    (forward-word 1)
    (setq ISSN-string
	  (concat "find ISSN " (buffer-substring start (point)) "\n"))
    (message (substring ISSN-string 0 (1- (length ISSN-string))))
    (x-set-selection 'PRIMARY ISSN-string)
    (x-set-cut-buffer ISSN-string)
    ))


(defun make-oclc-isbn-lookup-command ()
  "Find the next BibTeX entry with a missing LCCN value, and then use
its ISBN number to construct in the X primary selection buffer an
OCLC catalog lookup command."
  (interactive)
  (let ((start) (LCCN-pos) (ISBN-string) (temp-string) (k) (temp-syntax-table))
    (x-set-selection 'PRIMARY nil)	;forget current selection
    (if (re-search-forward "^[ \t]*LCCN[ \t]*=[ \t]*\"[?\"]" nil t)
	(progn
	  (setq temp-syntax-table (syntax-table))
	  (with-syntax-table temp-syntax-table
	    (modify-syntax-entry ?\- "word") ; hyphens are word charas
	    (setq LCCN-pos (1- (point)))
	    (modify-syntax-entry ?- "w")
	    (if (re-search-backward "^[ \t]*ISBN[ \t]*=[ \t]*\"\\|^@" nil t)
		(progn
		  (setq start (match-end 0))
		  (if (looking-at "^[ \t]*ISBN *= *\"[?]*\"")
		      (progn
			(re-search-forward "^ *}")
			(error "No ISBN available.")))
		  (if (looking-at "^[ \t]*ISBN")
		      (progn
			(goto-char start)
			(forward-word 1)
			(setq temp-string
			      (concat "s sn:"
				      (buffer-substring start (point)) "\n"))

			(setq k 0)
			(setq ISBN-string "")
			(while (< k (length temp-string))
			  (if (not (string-equal
				    (substring temp-string k (1+ k)) "-"))
			      (setq ISBN-string
				    (concat ISBN-string
					    (substring temp-string k (1+ k)))))
			  (setq k (1+ k)))
			(x-set-selection 'PRIMARY ISBN-string)
			(x-set-cut-buffer ISBN-string)
			(goto-char LCCN-pos)))))))
      (message "[%s]" (if ISBN-string
			  (substring ISBN-string 0 (1- (length ISBN-string)))
			"done")))))


(defun make-oclc-issn-lookup-command ()
  "Make an OCLC Contents1st catalog ISSN lookup command for the word
in which point lies."
  (interactive)
  (x-set-selection 'PRIMARY nil)	;forget current selection
  (let ((ISSN-string) (start))
    (forward-word 1)
    (backward-word 1)
    (setq start (point))
    (forward-word 1)
    (setq ISSN-string
	  (concat "s sn:" (buffer-substring start (point)) "\n"))
    (message (substring ISSN-string 0 (1- (length ISSN-string))))
    (x-set-selection 'PRIMARY ISSN-string)
    (x-set-cut-buffer ISSN-string)
    ))


(defun make-oclc-s-ti-command ()
  "Make an OCLC `s ti:xxxx' command for the title string in the
current BibTeX entry, leaving it in the kill-ring and cut buffer."
  (interactive)
  (re-search-backward "^@")
  (re-search-forward "^}\\|^ *title *= *\"")
  (let ((s-ti-cmd) (start (match-end 0)))
    (beginning-of-line)
    (if (looking-at "^ *title *= *\"")
	(progn
	  (goto-char start)
	  (end-of-line)
	  (backward-word 2)
	  (forward-word 1)
	  (if (< (point) start)
	      (forward-word 1))
	  (setq s-ti-cmd
		(concat "s ti:" (buffer-substring start (point)) "\n"))
	  (x-set-selection 'PRIMARY s-ti-cmd)
	  (x-set-cut-buffer s-ti-cmd)))))


(defun melvyl-isbn ()
  "Extract an ISBN from a University of California MELVYL catalog
entry produced by the

DISPLAY TITLE AUTHOR ISBN PUBLISHER
SET DISPLAY CONTIN

commands, storing it in register g."
  (interactive)
  (re-search-forward "^ISBN:")
  (re-search-forward "[0-9X]+")
  (let ((isbn))
    (setq isbn (buffer-substring (match-beginning 0) (match-end 0)))
    (setq isbn (concat
		(substring isbn 0 1)
		"-"
		(substring isbn 1 4)
		"-"
		(substring isbn 4 9)
		"-"
		(substring isbn 9)))
    (set-register ?g isbn)))


(defun melvyl-sub (old new s)
  "Substitute the first occurrence of OLD by NEW in a copy of the
string S and return it."
  (let ((k 0))
    ;; (debug)
    (while (and (< k (1+ (- (length s) (length old))))
		(not (string-equal old (substring s k (+ k (length old))))))
      (setq k (1+ k)))
    (if (and (< k (1+ (- (length s) (length old))))
	     (string-equal old (substring s k (+ k (length old)))))
	(concat (substring s 0 k) new
		(substring s (+ k (length old)) (length s)))
      s)))


(defun melvyl-update-missing-bib-fields ()
  "Filter a typescript of a melvyl.berkeley.edu library session with
`find ISBN' commands, selecting LCCN and pages values from the output,
and inserting them in the appropriate bibliography entry in the
other window."
  (interactive)
  (let ((isbn) (pages) (lccn) (start) (end))
    (if (search-forward "\nSearch request: FIND ISBN " nil t)
	(progn
	  (setq start (point))
	  (end-of-line)
	  (setq isbn (buffer-substring start (point)))
	  (re-search-forward "^Description: *")
	  (setq start (point))
	  (re-search-forward " p.\\|\n")
	  (setq pages (buffer-substring start (- (point) 3)))
	  (re-search-forward "^Call numbers: ")
	  (beginning-of-line)
	  (forward-char 31)
	  (setq start (point))
	  (end-of-line)
	  (setq lccn (buffer-substring start (point)))
	  (other-window 1)
	  (goto-char (point-min))
	  (search-forward isbn)
	  (search-forward "\",")
	  (forward-line 1)
	  (insert (format "  LCCN =         \"%s\",\n" lccn))
	  (insert (format "  pages =        \"%s\",\n"
			  (melvyl-sub "," " +" pages)))
	  (update-bibdate)
	  (re-search-backward "^@")
	  (recenter 0)
	  (search-forward "ISBN =")
	  (transient-mark-mode t)
	  (beginning-of-line)
	  (set-mark (point))
	  (search-forward "pages =")
	  (forward-line 1)
	  (other-window 1)
	  ))))


(defun merge-bibliography (into-buffer)
  "Merge bibliography entries from the current buffer into another
existing buffer, INTO-BUFFER, if they are not already present there.
Entries with the same citation labels (but possibly different
contents) are left to a human to deal with manually.

Entries are started by an at-sign in column 1, in the form

@Name{citation-label,

and ended by a close curly brace in column 1.

Buffers, rather than windows, are manipulated, so that no confusion
can arise over which is the source, and which is the destination."
  (interactive "bInto buffer:")
  (let ((start) (end) (found-entry) (bibliography-entry) (header)
	(from-buffer) )
    (setq from-buffer (buffer-name))
    (if (equal from-buffer into-buffer)
	(error "Cannot merge into current buffer!"))
    (while (re-search-forward "^@" nil t)
      (setq start (1- (point)))		;found beginning of entry
      (end-of-line)
      (delete-horizontal-space)
      (setq header (buffer-substring start (point))) ;get "@Name{label,"
      (re-search-forward "^}")
      (forward-line 1)
      (setq end (point))		;found end of entry
      (setq bibliography-entry (buffer-substring start end)) ;save entry

      ;; We have now found the entry of the current entry.  Now
      ;; find out whether this entry exists in INTO-BUFFER, and if so,
      ;; move it there.

      (switch-to-buffer into-buffer)
      (goto-char (point-min))
      (setq found-entry (search-forward header nil t))

      (if (not found-entry)		;if entry not found
	  (progn			;then insert it at the end
	    (goto-char (point-max))
	    (delete-blank-lines)
	    (newline)
	    (insert bibliography-entry)
	    (setq bibliography-entry nil)))

      ;; Return to the FROM-BUFFER, and if we copied the entry,
      ;; delete it from FROM-BUFFER.

      (switch-to-buffer from-buffer)
      (if (not found-entry)
	  (progn
	    (delete-region start end)
	    (delete-blank-lines))))))


(defun qr12 ()
  "Do a query-replace of Reg1 with Reg2 in the buffer, then return
point to its original position."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace (get-register ?1) (get-register ?2))))


(defun remove-duplicate-entries ()
  "Search a BibTeX file buffer for entries that are exact duplicates,
and remove the second (and third, fourth, ...) occurrence.  If the
labels match, but the complete entries do not, terminate with an
error to allow manual repair."
  (interactive)

  ;; move to start of current entry in case we are being restarted after a fixup
  (re-search-backward "^@" nil)

  (let ((old-label-start) (old-label-end) (old-start) (old-end)
	(new-end) (new-start) (deletion-count 0)
        (old-case-fold-search case-fold-search))
    (setq case-fold-search nil)		;we require EXACT matches
    ;; Find next entry and its label, and remember their bounds in old-*-*
    (while (re-search-forward "^\\(@[A-Za-z]+{\\)\\([^,]*,\\)" nil t)
      (setq old-start (match-beginning 1))
      ;; NB: label includes trailing comma, to avoid mismatches with other labels
      ;; with the same leading characters
      (setq old-label-start (match-beginning 2))
      (setq old-label-end (match-end 2))
      (re-search-forward "^}")
      (setq old-end (point))

      ;; Search the remainder of the buffer for matching entries
      (while (search-forward (buffer-substring old-start old-label-end) nil t)
	(beginning-of-line)
	(setq new-start (point))
	(re-search-forward "^}")
	(setq new-end (point))

	;; See if the complete entry matches
	(if (string-equal (buffer-substring old-start old-end)
			  (buffer-substring new-start new-end))
	    ;; entries match: delete the new one
	    (progn
	      (delete-region new-start new-end)
	      (delete-blank-lines)
	      (setq deletion-count (1+ deletion-count)))

	  ;; labels match, but entries do not
	  (goto-char old-start)		; position to original
	  (recenter 1)			; move original entry to top of window
	  (push-mark new-start t nil)	; mark duplicate one
	  (setq case-fold-search old-case-fold-search)
	  (error "%d deletions: Entries %s at point and mark have same label, but contents differ!"
		 deletion-count (buffer-substring old-label-start old-label-end))))

      ;; Issue a progress report, and move on to the next entry
      (message "%3d%% %d deletions"
	       (/ (* 100 (- old-end (point-min)))
		  (- (point-max) (point-min)))
	       deletion-count)
      (goto-char old-end))

    ;; Issue a final status message
    (message "%d duplicate entries pseudo-deleted" deletion-count)
    (setq case-fold-search old-case-fold-search)))


(defun sub (regexp replacement the-string)
  "Replace the first occurrence of REGEXP by REPLACEMENT in THE-STRING, and
return a new string with the result."
  (interactive)
  (let ((new-string nil))
    (save-excursion
      (set-buffer (get-buffer-create "*sub*"))
      (insert the-string)
      (goto-char (point-min))
      ;; [12-May-2012] force case-sensitive matching: case-fold-search
      ;; can be clobbered somewhat higher in the call chain, notably,
      ;; in bibtex-first-author-name, resulting in the call
      ;;     (sub "[ ,]+[IVX]+$" "" "Able Baker Xi")
      ;; incorrectly returning "Able Baker" instead of the correct
      ;; "Able Baker Xi"
      (let ((case-fold-search nil))
	(if (re-search-forward regexp nil t)
	    (replace-match replacement nil nil)))
      (setq new-string (buffer-substring (point-min) (point-max))))
    (kill-buffer "*sub*")
    new-string))


(defun title-query-replace (old new)
  "Run a query-replace of OLD by NEW, but in the title fields only, in
a BibTeX file."
  (interactive)
  (let ((end))
    (while (re-search-forward "^[ \t]*title[ \t]*=[ \t]*\"\\|^[ \t]*booktitle[ \t]*=[ \t]*\"" nil t)
      (save-excursion
	(search-forward "\",")
	(setq end (- (point) 2)))
      (narrow-to-region (point) end)
      (query-replace old new)
      (widen))))


(defun title-replace (old new)
  "Run a bibtex-replace-string of OLD by NEW, but in the title fields only, in
a BibTeX file."
  (interactive)
  (let ((end))
    (while (re-search-forward "^[ \t]*title[ \t]*=[ \t]*\"\\|^[ \t]*booktitle[ \t]*=[ \t]*\"" nil t)
      (save-excursion
	(search-forward "\",")
	(setq end (- (point) 2)))
      (narrow-to-region (point) end)
      (bibtex-replace-string old new)
      (widen))))


(defun toggle-case-fold-search ()
  "Toggle case-fold-search between case-sensitive and case-insensitive."
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "searching is case-%ssensitive" (if case-fold-search "IN" "")))


(defun wrap-long-strings ()
  "Wrap long @String{...} entries, lining up the = at column 32."
  (interactive)
  (while (re-search-forward "^@String" nil t)
    (search-forward "=")		;ensure = at column 32
    (backward-char 1)
    (insert-char ?\  (- 32 (current-column)))
    (end-of-line)
    (while (< 79 (current-column))
      (beginning-of-line)
      (forward-char 79)
      (search-backward " ")
      (insert "\n")
      (delete-horizontal-space)
      (insert-char ?\  34)
      (end-of-line))))

;;; ====================================================================
;;;			     Syntax Tables
;;; ====================================================================

(bibtex-make-extended-syntax-table)	;create new table only when
					;this function is loaded
