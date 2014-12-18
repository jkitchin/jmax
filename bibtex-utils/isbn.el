;;; /u/sy/beebe/emacs/isbn.el, Sat May  9 11:11:14 1998
;;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;;;
;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.15",
;;;     date            = "22 November 2004",
;;;     time            = "11:45:21 MST",
;;;     filename        = "isbn.el",
;;;     address         = "University of Utah
;;;                        Department of Mathematics, 110 LCB
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 585 1640, +1 801 581 4148",
;;;     checksum        = "27031 2597 13527 128187",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@ieee.org (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "bibliography, ISBN hyphenation, ISBN
;;;                        validation, ISSN hyphenation, ISSN
;;;                        validation",
;;;     supported       = "yes",
;;;     docstring       = "This file provides GNU Emacs LISP
;;;                        functions check-isbn and check-issn for
;;;                        verifying checksums of ISBN and ISSN
;;;                        entries in BibTeX .bib files, and
;;;                        hyphenate-isbn, that knows how to hyphenate
;;;                        hyphenless ISBNs, such as might be obtained
;;;                        from on-line library catalogs.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by
;;;                        the equivalent of the standard UNIX wc
;;;                        (word count) utility output of lines,
;;;                        words, and characters.  This is produced by
;;;                        Robert Solovay's checksum utility."
;;;  }
;;; ====================================================================

;;; isbn.el --- support for ISBN and ISSN validation and hyphenation

;;; Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999
;;; Free Software Foundation, Inc.

;;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Created: 10 September 1991 (probably earlier)
;;; Version: 1.00
;;; Keywords: checksum validation, ISBN, ISSN

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

(defconst isbn-version "1.15")		;NB: update this at every change
(defconst isbn-date "[22-Nov-2004]") 	;NB: update this at every change
(provide 'isbn)

;;; Change log:
;;; ====================================================================
;;; Version 1.15 [20-Nov-2004]
;;; New source of ISBN range data:
;;;
;;;	http://www.isbn-international.org/en/identifiers/List-of-Ranges.pdf
;;;
;;; Version 1.14 [06-Mar-2002]
;;; Minor update, with hyphenation patterns for Japanese ISBNs (4-xxxx).
;;;
;;; Version 1.13 [29-Aug-2001]
;;; Major update with new ISBN data derived entirely automatically
;;; from data at the International ISBN Agency Web site, in the
;;; files
;;;
;;;	http://www.isbn.spk-berlin.de/html/prefix/pref*.htm
;;;
;;; The first attempt at this, on 24-Nov-2000, uncovered numerous
;;; problems at that site which were reported to the Agency, and have
;;; happily now been corrected.
;;;
;;;
;;; Version 1.12 [26-Oct-1999]
;;; Major update with new data for 1993--1999.  Almost all ISBN ranges
;;; have been expanded or modified, and several countries have been
;;; added.
;;;
;;;
;;; Version 1.11 [25-Apr-1999]
;;; Add how-many argument to hyphenate-isbn, and expand its
;;; documentation string.
;;;
;;;
;;; Version 1.10 [22-Jan-1999]
;;; Extend isbn-in-rangep to convert final x checkdigit in an ISBN to
;;; more standard X.
;;;
;;;
;;; Extend hyphenate-isbn to check the character on either side of a
;;; candidate ISBN, requiring them to be something other than letters,
;;; digits, hyphen, underscore, plus, equals, or slash.  This makes it
;;; less likely that an ISBN embedded in a World-Wide Web URL will be
;;; matched, and (incorrectly) converted.
;;;
;;;
;;; Version 1.09 [09-May-1998]
;;; Add 23 additional countries to ISBN data.  Add (fset...) to
;;; (eval-when-compile ...), and move isbn-digit and issn-digit closer
;;; to start of file to eliminate warnings from byte-compile.
;;; Increase max-specpdl-size from 4000 to 5000.  Add another
;;; copyright year.  Add new keywords in file header.
;;;
;;;
;;; Version 1.08 [16-Mar-1998]
;;; Add additional English-language ISBN ranges from R. R. Bowker's Web
;;; site (URL recorded in the code below).
;;;
;;;
;;; Version 1.07 [22-Oct-1997]
;;; Add (provide 'isbn) feature.
;;;
;;;
;;; Version 1.06 [16-Sep-1997]
;;; Add (eval-when-compile ...) form to increase two compiler limits,
;;; allowing this file to (finally) be byte compiled.
;;;
;;; Add isbn-version and isbn-date constants.
;;;
;;; Add GNU copyright and author information above.
;;;
;;;
;;; Version 1.05 [11-Jul-1994]
;;; Add optional argument suppress-isbn-check to hyphenate-isbn.
;;; Update language comments, and add many new ISBN prefixes in
;;; range 84..99920.
;;;
;;;
;;; Version 1.04 [10-Jul-1994]
;;; Major update with new function hyphenate-isbn.
;;;
;;;
;;; Version 1.03 [10-Sep-1991]
;;; Changes not recorded
;;;
;;;
;;; Version 1.02 [??-???-????]
;;; Changes not recorded
;;;
;;;
;;; Version 1.01 [??-???-????]
;;; Changes not recorded
;;;
;;;
;;; Version 1.00 [??-???-????]
;;; Original version
;;; ====================================================================

(eval-when-compile
  (setq byte-optimize t)
  ;; Successful byte compilation of this function requires enlarging
  ;; these two values beyond their normal defaults
  (setq max-specpdl-size 15000)		;default: 600
  (setq max-lisp-eval-depth 4000)	;default: 200
  (fset 'issn-digit 'isbn-digit)
  nil)

;;; Thanks to Helmut M\"{u}lner, <hmuelner@fiicmds03.tu-graz.ac.at>
;;; in an e-mail message of Thu, 5 Sep 1991 10:59:16 +0200 to
;;; beebe@csc-sun.math.utah.edu for the info on the ISSN checksum:
;;;
;;; >> The last digit of an ISSN is a checksum digit and it is computed
;;; >> (almost) the same as an ISBN checksum:
;;; >>
;;; >>    (sum(k=1:7) digit(k) * (k+2)) mod 11 == digit(8)

(defun check-isbn ()
  "Search the buffer for BibTeX ISBN = \"value\" strings, and
 validate them."
  (interactive)
  (save-excursion
    (while (re-search-forward "ISBN[ \t]*=[ \t]*\"\\([-0-9Xx]+\\)\"" nil t)
      (if (not (isbn-numberp (buffer-substring (match-beginning 1)
                                                 (match-end 1))))
          (progn
            (ding)
            (princ "Invalid ISBN number---entering recursive edit")
            (recursive-edit))))
    (princ "[done]")))


(defun check-issn ()
  "Search the buffer for BibTeX ISSN = \"value\" strings, and
 validate them."
  (interactive)
  (save-excursion
    (while (re-search-forward "ISSN[ \t]*=[ \t]*\"\\([-0-9Xx]+\\)\"" nil t)
      (if (not (issn-numberp (buffer-substring (match-beginning 1)
                                                 (match-end 1))))
          (progn
            (ding)
            (princ "Invalid ISSN number---entering recursive edit")
            (recursive-edit))))
    (princ "[done]")))


(defun hyphenate-isbn (&optional how-many suppress-isbn-check)
  "Supply hyphenation in hyphenless ISBNs, such as might be extracted
from an on-line library catalog, for all ISBNs in the buffer from the
current point to the end.

To be recognized, ISBNs must be surrounded by characters other than
letters, digits, hyphen, and underscore.  These constraints reduce the
likelihood of erroneous conversion of ISBNs embedded in World-Wide Web
URLs, or arbitrary strings of digits with other meanings, such as
catalog order numbers.

An optional first argument, HOW-MANY, specifies a limit on the number
of ISBNs to attempt to convert.  The default of omitted, or nil, means
no limit: execution will continue until no more ISBNs can be
recognized.

ISBN checksums are verified before hyphenating, and a checksum error
terminates execution.  However, an optional non-nil second argument,
SUPPRESS-ISBN-CHECK, suppresses that checksum verification.

An ISBN (International Standard Book Number) is a 10-character value
of the form

	countrygroupnumber-publishernumber-booknumber-checkdigit

where the first three fields are of variable width and contain only
the digits [0-9], and the last single-character field contains [0-9Xx].
Larger country groups or publishers have smaller numbers, and
correspondingly larger booknumbers.

While it is permissible for the hyphens to be omitted, or replaced by
spaces, the hyphenated form is now in widespread use, and is to be
preferred, since it is more readable, and lacks the parsing ambiguity
when the parts are separated by spaces.

Reasonable library catalogs (e.g., U.S. Library of Congress or
University of California MELVYL system) support ISBN searching in any
of these three forms, but a few (notably, the OCLC databases) accept
only the 10-character form without spaces or hyphens.

While there is some regularity in the points where the publishernumber
digit count increases, there are nevertheless variations that must be
dealt with by special-case code.  The ranges below are taken from the
ISBN numeric index registry volume (Publishers' International ISBN
Directory, 19th edition, 1992/1993, Volume 2, Numerical ISBN Section,
R. R. Bowker, New York, 1992, ISBN 3-598-21601-7, and from the same
volume of the 25th edition, 1998/1999, ISBN 3-598-21607-6).

Unrecognized ISBNs raise an error message.  They may represent smaller
countries, or new publisher assignments, or simply be in error."

  (interactive "P")
  (setq how-many (if how-many (prefix-numeric-value how-many) nil))

  (while (and (or (null how-many) (> how-many 0))
	      (re-search-forward
	       "[^0-9A-Za-z_/=+-][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9Xx][^0-9A-Za-z_/=+-]" nil t))
    (if (not (null how-many))
	(setq how-many (1- how-many)))
    (backward-char 11)

    (if (and (null suppress-isbn-check)
	     (not (isbn-numberp (buffer-substring (point) (+ 10 (point))))))
	(error "Invalid ISBN: %s" (buffer-substring (point) (+ 10 (point)))))

    ;; NB: We intentionally include the hyphen that separates the
    ;; countrygroupnumber from the publishernumber, in order to
    ;; improve readability, even though this complicates the
    ;; processing in isbn-in-rangep.  Given the position of that
    ;; hyphen, we automatically compute the required hyphen positions.

    (cond
     ;; The following ISBN range data has been derived automatically from
     ;; the World-Wide Web site of The International ISBN Agency at
     ;;
     ;;         http://www.isbn.spk-berlin.de/html/prefix.htm
     ;;
     ;; shortly before Wed Aug 29 08:30:58 MDT 2001.
     ;;
     ;; There may be a few instances of data containing multiple
     ;; queries (???????); these are due to as-yet-unassigned
     ;; publisher numbers.
     ;; ----------------------------------------------------------------
     ;; Region name:     Australia
     ;; Country code(s): AU
     ;; Language group:  0
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ((isbn-in-rangep "0-00"        "0-19"      ))
     ((isbn-in-rangep "0-200"       "0-699"     ))
     ((isbn-in-rangep "0-7000"      "0-8499"    ))
     ((isbn-in-rangep "0-85000"     "0-89999"   ))
     ((isbn-in-rangep "0-900000"    "0-949999"  ))
     ((isbn-in-rangep "0-9500000"   "0-9999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Canada
     ;; Country code(s): CA
     ;; Language group:  0
     ;; Publisher(s):    7700 - 7799 ; 88500 - 88999 ; 919000 - 919699 ; 9690000 - 9690999
     ((isbn-in-rangep "0-7700"      "0-7799"    ))
     ((isbn-in-rangep "0-88500"     "0-88999"   ))
     ((isbn-in-rangep "0-919000"    "0-919699"  ))
     ((isbn-in-rangep "0-9690000"   "0-9690999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Gibraltar
     ;; Country code(s): GI
     ;; Language group:  0
     ;; Publisher(s):    9583000 - 9583049
     ((isbn-in-rangep "0-9583000"   "0-9583049" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Ireland
     ;; Country code(s): IE
     ;; Language group:  0
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     New Zealand
     ;; Country code(s): NZ
     ;; Language group:  0
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ;; ((isbn-in-rangep "0-00"        "0-19"      ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-200"       "0-699"     ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-7000"      "0-8499"    ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-85000"     "0-89999"   ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-900000"    "0-949999"  ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-9500000"   "0-9999999" ))	; duplicate: see entry for Australia [AU]
     ;; ----------------------------------------------------------------
     ;; Region name:     Puerto Rico
     ;; Country code(s): PR
     ;; Language group:  0
     ;; Publisher(s):    9633400 - 9633499
     ((isbn-in-rangep "0-9633400"   "0-9633499" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     South Africa
     ;; Country code(s): ZA
     ;; Language group:  0
     ;; Publisher(s):    620 - 629 ; 636 - 639 ; 7954 - 7973 ; 7975 - 7999 ; 86483 - 86487 ; 86808 - 86817 ; 86843 - 86852 ; 86873 - 86887 ; 86950 - 86999 ; 907980 - 907999 ; 908352 - 908409 ; 909060 - 909079 ; 909230 - 909239 ; 947000 - 947059 ; 949934 - 949998 ; 9583050 - 9583249 ; 9583750 - 9585149
     ((isbn-in-rangep "0-620"       "0-629"     ))
     ((isbn-in-rangep "0-636"       "0-639"     ))
     ((isbn-in-rangep "0-7954"      "0-7973"    ))
     ((isbn-in-rangep "0-7975"      "0-7999"    ))
     ((isbn-in-rangep "0-86483"     "0-86487"   ))
     ((isbn-in-rangep "0-86808"     "0-86817"   ))
     ((isbn-in-rangep "0-86843"     "0-86852"   ))
     ((isbn-in-rangep "0-86873"     "0-86887"   ))
     ((isbn-in-rangep "0-86950"     "0-86999"   ))
     ((isbn-in-rangep "0-907980"    "0-907999"  ))
     ((isbn-in-rangep "0-908352"    "0-908409"  ))
     ((isbn-in-rangep "0-909060"    "0-909079"  ))
     ((isbn-in-rangep "0-909230"    "0-909239"  ))
     ((isbn-in-rangep "0-947000"    "0-947059"  ))
     ((isbn-in-rangep "0-949934"    "0-949998"  ))
     ((isbn-in-rangep "0-9583050"   "0-9583249" ))
     ((isbn-in-rangep "0-9583750"   "0-9585149" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Swaziland
     ;; Country code(s): SZ
     ;; Language group:  0
     ;; Publisher(s):    ??????? - ???????
     ;; ((isbn-in-rangep "0-???????"   "0-???????" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     United Kingdom
     ;; Country code(s): GB
     ;; Language group:  0
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ;; ((isbn-in-rangep "0-00"        "0-19"      ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-200"       "0-699"     ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-7000"      "0-8499"    ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-85000"     "0-89999"   ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-900000"    "0-949999"  ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-9500000"   "0-9999999" ))	; duplicate: see entry for Australia [AU]
     ;; ----------------------------------------------------------------
     ;; Region name:     United States of America
     ;; Country code(s): US
     ;; Language group:  0
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ;; ((isbn-in-rangep "0-00"        "0-19"      ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-200"       "0-699"     ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-7000"      "0-8499"    ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-85000"     "0-89999"   ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-900000"    "0-949999"  ))	; duplicate: see entry for Australia [AU]
     ;; ((isbn-in-rangep "0-9500000"   "0-9999999" ))	; duplicate: see entry for Australia [AU]
     ;; ----------------------------------------------------------------
     ;; Region name:     Zimbabwe
     ;; Country code(s): ZW
     ;; Language group:  0
     ;; Publisher(s):    7974 ; 86918 - 86929 ; 908300 - 908305 ; 949225 - 949933
     ((isbn-in-rangep "0-7974"      "0-7974"    ))
     ((isbn-in-rangep "0-86918"     "0-86929"   ))
     ((isbn-in-rangep "0-908300"    "0-908305"  ))
     ((isbn-in-rangep "0-949225"    "0-949933"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Australia
     ;; Country code(s): AU
     ;; Language group:  1
     ;; Publisher(s):    74000 - 76999 ; 86250 - 86799 ; 875100 - 877099 ; 920680 - 926429
     ((isbn-in-rangep "1-74000"     "1-76999"   ))
     ((isbn-in-rangep "1-86250"     "1-86799"   ))
     ((isbn-in-rangep "1-875100"    "1-877099"  ))
     ((isbn-in-rangep "1-920680"    "1-926429"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Canada
     ;; Country code(s): CA
     ;; Language group:  1
     ;; Publisher(s):    55000 - 55499
     ((isbn-in-rangep "1-55000"     "1-55499"   ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Gibraltar
     ;; Country code(s): GI
     ;; Language group:  1
     ;; Publisher(s):    919655 - 919679
     ((isbn-in-rangep "1-919655"    "1-919679"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     New Zealand
     ;; Country code(s): NZ
     ;; Language group:  1
     ;; Publisher(s):    86930 - 86979 ; 877130 - 877579
     ((isbn-in-rangep "1-86930"     "1-86979"   ))
     ((isbn-in-rangep "1-877130"    "1-877579"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Puerto Rico
     ;; Country code(s): PR
     ;; Language group:  1
     ;; Publisher(s):    881700 - 881749
     ((isbn-in-rangep "1-881700"    "1-881749"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     South Africa
     ;; Country code(s): ZA
     ;; Language group:  1
     ;; Publisher(s):    86800 - 86929 ; 874800 - 875099 ; 919680 - 920679
     ((isbn-in-rangep "1-86800"     "1-86929"   ))
     ((isbn-in-rangep "1-874800"    "1-875099"  ))
     ((isbn-in-rangep "1-919680"    "1-920679"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     United Kingdom
     ;; Country code(s): GB
     ;; Language group:  1
     ;; Publisher(s):    84000 - 86249 ; 869800 - 874799 ; 897580 - 919679
     ((isbn-in-rangep "1-84000"     "1-86249"   ))
     ((isbn-in-rangep "1-869800"    "1-874799"  ))
     ((isbn-in-rangep "1-897580"    "1-919679"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     United States of America
     ;; Country code(s): US
     ;; Language group:  1
     ;; Publisher(s):    55500 - 59999 ; 877580 - 897579
     ((isbn-in-rangep "1-55500"     "1-59999"   ))
     ((isbn-in-rangep "1-877580"    "1-897579"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Zimbabwe
     ;; Country code(s): ZW
     ;; Language group:  1
     ;; Publisher(s):    77900 - 77919 ; 877100 - 877129
     ((isbn-in-rangep "1-77900"     "1-77919"   ))
     ((isbn-in-rangep "1-877100"    "1-877129"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Belgium
     ;; Country code(s): BE
     ;; Language group:  2
     ;; Publisher(s):    500 - 549 ; 8000 - 8249 ; 87000 - 87949 ; 930000 - 939999 ; 9600000 - 9699999
     ((isbn-in-rangep "2-500"       "2-549"     ))
     ((isbn-in-rangep "2-8000"      "2-8249"    ))
     ((isbn-in-rangep "2-87000"     "2-87949"   ))
     ((isbn-in-rangep "2-930000"    "2-939999"  ))
     ((isbn-in-rangep "2-9600000"   "2-9699999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Canada
     ;; Country code(s): CA
     ;; Language group:  2
     ;; Publisher(s):    550 - 599 ; 7600 - 7653 ; 7655 - 7776 ; 7778 - 7999 ; 89000 - 89999 ; 920000 - 929999 ; 9800000 - 9999999
     ((isbn-in-rangep "2-550"       "2-599"     ))
     ((isbn-in-rangep "2-7600"      "2-7653"    ))
     ((isbn-in-rangep "2-7655"      "2-7776"    ))
     ((isbn-in-rangep "2-7778"      "2-7999"    ))
     ((isbn-in-rangep "2-89000"     "2-89999"   ))
     ((isbn-in-rangep "2-920000"    "2-929999"  ))
     ((isbn-in-rangep "2-9800000"   "2-9999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     France
     ;; Country code(s): FR
     ;; Language group:  2
     ;; Publisher(s):    00 - 19 ; 200 - 399 ; 7000 - 7599 ; 7654 ; 7777 ; 84000 - 86999 ; 900000 - 919768 ; 9500000 - 9599768
     ((isbn-in-rangep "2-00"        "2-19"      ))
     ((isbn-in-rangep "2-200"       "2-399"     ))
     ((isbn-in-rangep "2-7000"      "2-7599"    ))
     ((isbn-in-rangep "2-7654"      "2-7654"    ))
     ((isbn-in-rangep "2-7777"      "2-7777"    ))
     ((isbn-in-rangep "2-84000"     "2-86999"   ))
     ((isbn-in-rangep "2-900000"    "2-919768"  ))
     ((isbn-in-rangep "2-9500000"   "2-9599768" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Luxembourg
     ;; Country code(s): LU
     ;; Language group:  2
     ;; Publisher(s):    495 - 499 ; 87950 - 87999 ; 919769 - 919999 ; 9599769 - 9599999
     ((isbn-in-rangep "2-495"       "2-499"     ))
     ((isbn-in-rangep "2-87950"     "2-87999"   ))
     ((isbn-in-rangep "2-919769"    "2-919999"  ))
     ((isbn-in-rangep "2-9599769"   "2-9599999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Switzerland (French speaking)
     ;; Country code(s): CH
     ;; Language group:  2
     ;; Publisher(s):    600 - 699 ; 8250 - 8399 ; 88000 - 88999 ; 940000 - 949999 ; 9700000 - 9799999
     ((isbn-in-rangep "2-600"       "2-699"     ))
     ((isbn-in-rangep "2-8250"      "2-8399"    ))
     ((isbn-in-rangep "2-88000"     "2-88999"   ))
     ((isbn-in-rangep "2-940000"    "2-949999"  ))
     ((isbn-in-rangep "2-9700000"   "2-9799999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Austria
     ;; Country code(s): AT
     ;; Language group:  3
     ;; Publisher(s):    01 - 02 ; 200 - 249 ; 7000 - 7149 ; 85000 - 85499 ; 900000 - 904999 ; 9500000 - 9519999
     ((isbn-in-rangep "3-01"        "3-02"      ))
     ((isbn-in-rangep "3-200"       "3-249"     ))
     ((isbn-in-rangep "3-7000"      "3-7149"    ))
     ((isbn-in-rangep "3-85000"     "3-85499"   ))
     ((isbn-in-rangep "3-900000"    "3-904999"  ))
     ((isbn-in-rangep "3-9500000"   "3-9519999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Germany
     ;; Country code(s): DE
     ;; Language group:  3
     ;; Publisher(s):    00 ; 05 - 19 ; 320 - 379 ; 400 - 699 ; 7300 - 7499 ; 7600 - 8499 ; 86000 - 86499 ; 87000 - 89999 ; 910000 - 949999 ; 9700000 - 9899999
     ((isbn-in-rangep "3-00"        "3-00"      ))
     ((isbn-in-rangep "3-05"        "3-19"      ))
     ((isbn-in-rangep "3-320"       "3-379"     ))
     ((isbn-in-rangep "3-400"       "3-699"     ))
     ((isbn-in-rangep "3-7300"      "3-7499"    ))
     ((isbn-in-rangep "3-7600"      "3-8499"    ))
     ((isbn-in-rangep "3-86000"     "3-86499"   ))
     ((isbn-in-rangep "3-87000"     "3-89999"   ))
     ((isbn-in-rangep "3-910000"    "3-949999"  ))
     ((isbn-in-rangep "3-9700000"   "3-9899999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Switzerland (German speaking)
     ;; Country code(s): CH
     ;; Language group:  3
     ;; Publisher(s):    030 - 033 ; 0340 - 0369 ; 03700 - 03999 ; 250 - 319 ; 7150 - 7299 ; 85500 - 85999 ; 905000 - 909999 ; 9520000 - 9539999
     ((isbn-in-rangep "3-030"       "3-033"     ))
     ((isbn-in-rangep "3-0340"      "3-0369"    ))
     ((isbn-in-rangep "3-03700"     "3-03999"   ))
     ((isbn-in-rangep "3-250"       "3-319"     ))
     ((isbn-in-rangep "3-7150"      "3-7299"    ))
     ((isbn-in-rangep "3-85500"     "3-85999"   ))
     ((isbn-in-rangep "3-905000"    "3-909999"  ))
     ((isbn-in-rangep "3-9520000"   "3-9539999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Japan
     ;; Country code(s): JP
     ;; Language group:  4
     ;; Publisher(s): 00 - 19 ; 200 - 699 ; 7000 -8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ((isbn-in-rangep "4-00"        "4-19"	))
     ((isbn-in-rangep "4-200"       "4-699"	))
     ((isbn-in-rangep "4-7000"      "4-8499"	))
     ((isbn-in-rangep "4-85000"     "4-89999"	))
     ((isbn-in-rangep "4-900000"    "4-949999"	))
     ((isbn-in-rangep "4-9500000"   "4-9999999"	))
     ;; ----------------------------------------------------------------
     ;; Region name:     Armenia
     ;; Country code(s): AM
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 800000 - 919999
     ((isbn-in-rangep "5-00"        "5-19"      ))
     ((isbn-in-rangep "5-200"       "5-699"     ))
     ((isbn-in-rangep "5-7000"      "5-8499"    ))
     ((isbn-in-rangep "5-85000"     "5-89999"   ))
     ((isbn-in-rangep "5-800000"    "5-919999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Belarus
     ;; Country code(s): BY
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ((isbn-in-rangep "5-900000"    "5-949999"  ))
     ((isbn-in-rangep "5-9500000"   "5-9999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Estonia
     ;; Country code(s): EE
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 9500000 - 9999999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-949999"  ))	; duplicate: see entry for Belarus [BY]
     ;; ((isbn-in-rangep "5-9500000"   "5-9999999" ))	; duplicate: see entry for Belarus [BY]
     ;; ----------------------------------------------------------------
     ;; Region name:     Kazakhstan
     ;; Country code(s): KZ
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ((isbn-in-rangep "5-900000"    "5-919999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Kyrgyzstan
     ;; Country code(s): KG
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-919999"  ))	; duplicate: see entry for Kazakhstan [KZ]
     ;; ----------------------------------------------------------------
     ;; Region name:     Latvia
     ;; Country code(s): LV
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-919999"  ))	; duplicate: see entry for Kazakhstan [KZ]
     ;; ----------------------------------------------------------------
     ;; Region name:     Lithuania
     ;; Country code(s): LT
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-919999"  ))	; duplicate: see entry for Kazakhstan [KZ]
     ;; ----------------------------------------------------------------
     ;; Region name:     Moldova
     ;; Country code(s): MD
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-919999"  ))	; duplicate: see entry for Kazakhstan [KZ]
     ;; ----------------------------------------------------------------
     ;; Region name:     Russian Federation
     ;; Country code(s): RU
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999 ; 9200 - 9299 ; 93000 - 94999 ; 950000 - 979999 ; 9800000 - 9999999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-919999"  ))	; duplicate: see entry for Kazakhstan [KZ]
     ((isbn-in-rangep "5-9200"      "5-9299"    ))
     ((isbn-in-rangep "5-93000"     "5-94999"   ))
     ((isbn-in-rangep "5-950000"    "5-979999"  ))
     ((isbn-in-rangep "5-9800000"   "5-9999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Ukraine
     ;; Country code(s): UA
     ;; Language group:  5
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 919999
     ;; ((isbn-in-rangep "5-00"        "5-19"      ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-200"       "5-699"     ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-7000"      "5-8499"    ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-85000"     "5-89999"   ))	; duplicate: see entry for Armenia [AM]
     ;; ((isbn-in-rangep "5-900000"    "5-919999"  ))	; duplicate: see entry for Kazakhstan [KZ]
     ;; ----------------------------------------------------------------
     ;; Region name:     China
     ;; Country code(s): CN
     ;; Language group:  7
     ;; Publisher(s):    00 - 09 ; 100 - 499 ; 5000 - 7999 ; 80000 - 89999 ; 900000 - 999999
     ((isbn-in-rangep "7-00"        "7-09"      ))
     ((isbn-in-rangep "7-100"       "7-499"     ))
     ((isbn-in-rangep "7-5000"      "7-7999"    ))
     ((isbn-in-rangep "7-80000"     "7-89999"   ))
     ((isbn-in-rangep "7-900000"    "7-999999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Czech Republic
     ;; Country code(s): CZ
     ;; Language group:  80
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 999999
     ((isbn-in-rangep "80-00"       "80-19"     ))
     ((isbn-in-rangep "80-200"      "80-699"    ))
     ((isbn-in-rangep "80-7000"     "80-8499"   ))
     ((isbn-in-rangep "80-85000"    "80-89999"  ))
     ((isbn-in-rangep "80-900000"   "80-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Slovakia
     ;; Country code(s): SK
     ;; Language group:  80
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 999999
     ;; ((isbn-in-rangep "80-00"       "80-19"     ))	; duplicate: see entry for Czech Republic [CZ]
     ;; ((isbn-in-rangep "80-200"      "80-699"    ))	; duplicate: see entry for Czech Republic [CZ]
     ;; ((isbn-in-rangep "80-7000"     "80-8499"   ))	; duplicate: see entry for Czech Republic [CZ]
     ;; ((isbn-in-rangep "80-85000"    "80-89999"  ))	; duplicate: see entry for Czech Republic [CZ]
     ;; ((isbn-in-rangep "80-900000"   "80-999999" ))	; duplicate: see entry for Czech Republic [CZ]
     ;; ----------------------------------------------------------------
     ;; Region name:     India
     ;; Country code(s): IN
     ;; Language group:  81
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Norway
     ;; Country code(s): NO
     ;; Language group:  82
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8999 ; 90000 - 98999 ; 990000 - 999999
     ((isbn-in-rangep "82-00"       "82-19"     ))
     ((isbn-in-rangep "82-200"      "82-699"    ))
     ((isbn-in-rangep "82-7000"     "82-8999"   ))
     ((isbn-in-rangep "82-90000"    "82-98999"  ))
     ((isbn-in-rangep "82-990000"   "82-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Poland
     ;; Country code(s): PL
     ;; Language group:  83
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 999999
     ((isbn-in-rangep "83-00"       "83-19"     ))
     ((isbn-in-rangep "83-200"      "83-699"    ))
     ((isbn-in-rangep "83-7000"     "83-8499"   ))
     ((isbn-in-rangep "83-85000"    "83-89999"  ))
     ((isbn-in-rangep "83-900000"   "83-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Spain
     ;; Country code(s): ES
     ;; Language group:  84
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 949999 ; 95000 - 96999 ; 9700 - 9999
     ((isbn-in-rangep "84-00"       "84-19"     ))
     ((isbn-in-rangep "84-200"      "84-699"    ))
     ((isbn-in-rangep "84-7000"     "84-8499"   ))
     ((isbn-in-rangep "84-85000"    "84-89999"  ))
     ((isbn-in-rangep "84-900000"   "84-949999" ))
     ((isbn-in-rangep "84-95000"    "84-96999"  ))
     ((isbn-in-rangep "84-9700"     "84-9999"   ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Brazil
     ;; Country code(s): BR
     ;; Language group:  85
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 999999
     ((isbn-in-rangep "85-00"       "85-19"     ))
     ((isbn-in-rangep "85-200"      "85-699"    ))
     ((isbn-in-rangep "85-7000"     "85-8499"   ))
     ((isbn-in-rangep "85-85000"    "85-89999"  ))
     ((isbn-in-rangep "85-900000"   "85-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Bosnia and Herzegovina
     ;; Country code(s): BA
     ;; Language group:  86
     ;; Publisher(s):    00 - 29 ; 300 - 699 ; 7000 - 7999 ; 80000 - 89999 ; 900000 - 999999
     ((isbn-in-rangep "86-00"       "86-29"     ))
     ((isbn-in-rangep "86-300"      "86-699"    ))
     ((isbn-in-rangep "86-7000"     "86-7999"   ))
     ((isbn-in-rangep "86-80000"    "86-89999"  ))
     ((isbn-in-rangep "86-900000"   "86-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Croatia
     ;; Country code(s): HR
     ;; Language group:  86
     ;; Publisher(s):    00 - 29 ; 300 - 699 ; 7000 - 7999 ; 80000 - 89999 ; 900000 - 999999
     ;; ((isbn-in-rangep "86-00"       "86-29"     ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-300"      "86-699"    ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-7000"     "86-7999"   ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-80000"    "86-89999"  ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-900000"   "86-999999" ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ----------------------------------------------------------------
     ;; Region name:     Macedonia
     ;; Country code(s): MK
     ;; Language group:  86
     ;; Publisher(s):    00 - 29 ; 300 - 699 ; 7000 - 7999 ; 80000 - 89999 ; 900000 - 999999
     ;; ((isbn-in-rangep "86-00"       "86-29"     ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-300"      "86-699"    ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-7000"     "86-7999"   ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-80000"    "86-89999"  ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-900000"   "86-999999" ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ----------------------------------------------------------------
     ;; Region name:     Slovenia
     ;; Country code(s): SI
     ;; Language group:  86
     ;; Publisher(s):    00 - 29 ; 300 - 699 ; 7000 - 7999 ; 80000 - 89999 ; 900000 - 999999
     ;; ((isbn-in-rangep "86-00"       "86-29"     ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-300"      "86-699"    ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-7000"     "86-7999"   ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-80000"    "86-89999"  ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-900000"   "86-999999" ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ----------------------------------------------------------------
     ;; Region name:     Yugoslavia
     ;; Country code(s): YU
     ;; Language group:  86
     ;; Publisher(s):    00 - 29 ; 300 - 699 ; 7000 - 7999 ; 80000 - 89999 ; 900000 - 999999
     ;; ((isbn-in-rangep "86-00"       "86-29"     ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-300"      "86-699"    ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-7000"     "86-7999"   ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-80000"    "86-89999"  ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ((isbn-in-rangep "86-900000"   "86-999999" ))	; duplicate: see entry for Bosnia and Herzegovina [BA]
     ;; ----------------------------------------------------------------
     ;; Region name:     Denmark
     ;; Country code(s): DK
     ;; Language group:  87
     ;; Publisher(s):    00 - 29 ; 400 - 649 ; 7000 - 7999 ; 85000 - 94999 ; 970000 - 999999
     ((isbn-in-rangep "87-00"       "87-29"     ))
     ((isbn-in-rangep "87-400"      "87-649"    ))
     ((isbn-in-rangep "87-7000"     "87-7999"   ))
     ((isbn-in-rangep "87-85000"    "87-94999"  ))
     ((isbn-in-rangep "87-970000"   "87-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Italy
     ;; Country code(s): IT
     ;; Language group:  88
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Switzerland (Italian speaking)
     ;; Country code(s): CH
     ;; Language group:  88
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 900000 - 999999
     ((isbn-in-rangep "88-00"       "88-19"     ))
     ((isbn-in-rangep "88-200"      "88-699"    ))
     ((isbn-in-rangep "88-7000"     "88-8499"   ))
     ((isbn-in-rangep "88-85000"    "88-89999"  ))
     ((isbn-in-rangep "88-900000"   "88-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Korea (Republic)
     ;; Country code(s): KR
     ;; Language group:  89
     ;; Publisher(s):    00 - 24 ; 250 - 549 ; 5500 - 8499 ; 85000 - 94999 ; 950000 - 999999
     ((isbn-in-rangep "89-00"       "89-24"     ))
     ((isbn-in-rangep "89-250"      "89-549"    ))
     ((isbn-in-rangep "89-5500"     "89-8499"   ))
     ((isbn-in-rangep "89-85000"    "89-94999"  ))
     ((isbn-in-rangep "89-950000"   "89-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Belgium
     ;; Country code(s): BE
     ;; Language group:  90
     ;; Publisher(s):    00 - 19 ; 200 - 499 ; 5000 - 6999 ; 70000 - 79999 ; 800000 - 899999
     ((isbn-in-rangep "90-00"       "90-19"     ))
     ((isbn-in-rangep "90-200"      "90-499"    ))
     ((isbn-in-rangep "90-5000"     "90-6999"   ))
     ((isbn-in-rangep "90-70000"    "90-79999"  ))
     ((isbn-in-rangep "90-800000"   "90-899999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Netherlands
     ;; Country code(s): NL
     ;; Language group:  90
     ;; Publisher(s):    00 - 19 ; 200 - 499 ; 5000 - 6999 ; 70000 - 79999 ; 800000 - 899999
     ;; ((isbn-in-rangep "90-00"       "90-19"     ))	; duplicate: see entry for Belgium [BE]
     ;; ((isbn-in-rangep "90-200"      "90-499"    ))	; duplicate: see entry for Belgium [BE]
     ;; ((isbn-in-rangep "90-5000"     "90-6999"   ))	; duplicate: see entry for Belgium [BE]
     ;; ((isbn-in-rangep "90-70000"    "90-79999"  ))	; duplicate: see entry for Belgium [BE]
     ;; ((isbn-in-rangep "90-800000"   "90-899999" ))	; duplicate: see entry for Belgium [BE]
     ;; ----------------------------------------------------------------
     ;; Region name:     Sweden
     ;; Country code(s): SE
     ;; Language group:  91
     ;; Publisher(s):    0 - 1 ; 20 - 49 ; 500 - 649 ; 7000 - 7999 ; 85000 - 94999 ; 970000 - 999999
     ((isbn-in-rangep "91-0"        "91-1"      ))
     ((isbn-in-rangep "91-20"       "91-49"     ))
     ((isbn-in-rangep "91-500"      "91-649"    ))
     ((isbn-in-rangep "91-7000"     "91-7999"   ))
     ((isbn-in-rangep "91-85000"    "91-94999"  ))
     ((isbn-in-rangep "91-970000"   "91-999999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     International Organizations (Publishers), Unesco
     ;; Country code(s): IP
     ;; Language group:  92
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Argentina
     ;; Country code(s): AR
     ;; Language group:  950
     ;; Publisher(s):    00 - 49 ; 500 - 899 ; 9000 - 9899 ; 99000 - 99999
     ((isbn-in-rangep "950-00"      "950-49"    ))
     ((isbn-in-rangep "950-500"     "950-899"   ))
     ((isbn-in-rangep "950-9000"    "950-9899"  ))
     ((isbn-in-rangep "950-99000"   "950-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Finland
     ;; Country code(s): FI
     ;; Language group:  951
     ;; Publisher(s):    0 - 1 ; 20 - 54 ; 550 - 889 ; 8900 - 9499 ; 95000 - 99999
     ((isbn-in-rangep "951-0"       "951-1"     ))
     ((isbn-in-rangep "951-20"      "951-54"    ))
     ((isbn-in-rangep "951-550"     "951-889"   ))
     ((isbn-in-rangep "951-8900"    "951-9499"  ))
     ((isbn-in-rangep "951-95000"   "951-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Finland
     ;; Country code(s): FI
     ;; Language group:  952
     ;; Publisher(s):    00 - 19 ; 200 - 499 ; 5000 - 8899 ; 89 - 94 ; 9500 - 9899 ; 99000 - 99999
     ((isbn-in-rangep "952-00"      "952-19"    ))
     ((isbn-in-rangep "952-200"     "952-499"   ))
     ((isbn-in-rangep "952-5000"    "952-8899"  ))
     ((isbn-in-rangep "952-89"      "952-94"    ))
     ((isbn-in-rangep "952-9500"    "952-9899"  ))
     ((isbn-in-rangep "952-99000"   "952-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Croatia
     ;; Country code(s): HR
     ;; Language group:  953
     ;; Publisher(s):    0 ; 10 - 14 ; 150 - 599 ; 6000 - 9599 ; 96000 - 99999
     ((isbn-in-rangep "953-0"       "953-0"     ))
     ((isbn-in-rangep "953-10"      "953-14"    ))
     ((isbn-in-rangep "953-150"     "953-599"   ))
     ((isbn-in-rangep "953-6000"    "953-9599"  ))
     ((isbn-in-rangep "953-96000"   "953-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Bulgaria
     ;; Country code(s): BG
     ;; Language group:  954
     ;; Publisher(s):    00 - 29 ; 300 - 799 ; 8000 - 8999 ; 90000 - 94999 ; 9500 - 9999
     ((isbn-in-rangep "954-00"      "954-29"    ))
     ((isbn-in-rangep "954-300"     "954-799"   ))
     ((isbn-in-rangep "954-8000"    "954-8999"  ))
     ((isbn-in-rangep "954-90000"   "954-94999" ))
     ((isbn-in-rangep "954-9500"    "954-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Sri Lanka
     ;; Country code(s): LK
     ;; Language group:  955
     ;; Publisher(s):    0 - 1 ; 20 - 54 ; 550 - 799 ; 8000 - 9499 ; 95000 - 99999
     ((isbn-in-rangep "955-0"       "955-1"     ))
     ((isbn-in-rangep "955-20"      "955-54"    ))
     ((isbn-in-rangep "955-550"     "955-799"   ))
     ((isbn-in-rangep "955-8000"    "955-9499"  ))
     ((isbn-in-rangep "955-95000"   "955-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Chile
     ;; Country code(s): CL
     ;; Language group:  956
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 9999
     ((isbn-in-rangep "956-00"      "956-19"    ))
     ((isbn-in-rangep "956-200"     "956-699"   ))
     ((isbn-in-rangep "956-7000"    "956-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Taiwan, China
     ;; Country code(s): TW
     ;; Language group:  957
     ;; Publisher(s):    00 - 02 ; 0300 - 0499 ; 05 -19 ; 2000 - 2099 ; 21 - 29 ; 30000 - 30999 ; 31 - 43 ; 440 - 819 ; 8200 - 9699 ; 97000 - 99999
     ((isbn-in-rangep "957-00"      "957-02"    ))
     ((isbn-in-rangep "957-0300"    "957-0499"  ))
     ((isbn-in-rangep "957-05"      "957-19"    ))
     ((isbn-in-rangep "957-2000"    "957-2099"  ))
     ((isbn-in-rangep "957-21"      "957-29"    ))
     ((isbn-in-rangep "957-30000"   "957-30999" ))
     ((isbn-in-rangep "957-31"      "957-43"    ))
     ((isbn-in-rangep "957-440"     "957-819"   ))
     ((isbn-in-rangep "957-8200"    "957-9699"  ))
     ((isbn-in-rangep "957-97000"   "957-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Colombia
     ;; Country code(s): CO
     ;; Language group:  958
     ;; Publisher(s):    00 - 59 ; 600 - 799 ; 8000 - 9499 ; 95000 - 99999
     ((isbn-in-rangep "958-00"      "958-59"    ))
     ((isbn-in-rangep "958-600"     "958-799"   ))
     ((isbn-in-rangep "958-8000"    "958-9499"  ))
     ((isbn-in-rangep "958-95000"   "958-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Cuba
     ;; Country code(s): CU
     ;; Language group:  959
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499
     ((isbn-in-rangep "959-00"      "959-19"    ))
     ((isbn-in-rangep "959-200"     "959-699"   ))
     ((isbn-in-rangep "959-7000"    "959-8499"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Greece
     ;; Country code(s): GR
     ;; Language group:  960
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 99999
     ((isbn-in-rangep "960-00"      "960-19"    ))
     ((isbn-in-rangep "960-200"     "960-699"   ))
     ((isbn-in-rangep "960-7000"    "960-8499"  ))
     ((isbn-in-rangep "960-85000"   "960-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Slovenia
     ;; Country code(s): SI
     ;; Language group:  961
     ;; Publisher(s):    00 - 19 ; 200 - 599 ; 6000 - 8999 ; 90000 - 94999
     ((isbn-in-rangep "961-00"      "961-19"    ))
     ((isbn-in-rangep "961-200"     "961-599"   ))
     ((isbn-in-rangep "961-6000"    "961-8999"  ))
     ((isbn-in-rangep "961-90000"   "961-94999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Hong Kong
     ;; Country code(s): HK
     ;; Language group:  962
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 86999 ; 8700 - 8999 ; 900 - 999
     ((isbn-in-rangep "962-00"      "962-19"    ))
     ((isbn-in-rangep "962-200"     "962-699"   ))
     ((isbn-in-rangep "962-7000"    "962-8499"  ))
     ((isbn-in-rangep "962-85000"   "962-86999" ))
     ((isbn-in-rangep "962-8700"    "962-8999"  ))
     ((isbn-in-rangep "962-900"     "962-999"   ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Hungary
     ;; Country code(s): HU
     ;; Language group:  963
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999 ; 9000 - 9999
     ((isbn-in-rangep "963-00"      "963-19"    ))
     ((isbn-in-rangep "963-200"     "963-699"   ))
     ((isbn-in-rangep "963-7000"    "963-8499"  ))
     ((isbn-in-rangep "963-85000"   "963-89999" ))
     ((isbn-in-rangep "963-9000"    "963-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Iran
     ;; Country code(s): IR
     ;; Language group:  964
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Israel
     ;; Country code(s): IL
     ;; Language group:  965
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Ukraine
     ;; Country code(s): UA
     ;; Language group:  966
     ;; Publisher(s):    00 - 29 ; 500 - 699 ; 7000 - 7999 ; 90000 - 99999
     ((isbn-in-rangep "966-00"      "966-29"    ))
     ((isbn-in-rangep "966-500"     "966-699"   ))
     ((isbn-in-rangep "966-7000"    "966-7999"  ))
     ((isbn-in-rangep "966-90000"   "966-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Malaysia
     ;; Country code(s): MY
     ;; Language group:  967
     ;; Publisher(s):    0 - 5 ; 60 - 89 ; 900 - 989 ; 9900 - 9989 ; 99900 - 99999
     ((isbn-in-rangep "967-0"       "967-5"     ))
     ((isbn-in-rangep "967-60"      "967-89"    ))
     ((isbn-in-rangep "967-900"     "967-989"   ))
     ((isbn-in-rangep "967-9900"    "967-9989"  ))
     ((isbn-in-rangep "967-99900"   "967-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Mexico
     ;; Country code(s): MX
     ;; Language group:  968
     ;; Publisher(s):    10 - 39 ; 400 - 499 ; 5000 - 7999 ; 800 - 899
     ((isbn-in-rangep "968-10"      "968-39"    ))
     ((isbn-in-rangep "968-400"     "968-499"   ))
     ((isbn-in-rangep "968-5000"    "968-7999"  ))
     ((isbn-in-rangep "968-800"     "968-899"   ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Pakistan
     ;; Country code(s): PK
     ;; Language group:  969
     ;; Publisher(s):    0 - 1 ; 20 - 39 ; 400 - 799 ; 8000 - 9999
     ((isbn-in-rangep "969-0"       "969-1"     ))
     ((isbn-in-rangep "969-20"      "969-39"    ))
     ((isbn-in-rangep "969-400"     "969-799"   ))
     ((isbn-in-rangep "969-8000"    "969-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Mexico
     ;; Country code(s): MX
     ;; Language group:  970
     ;; Publisher(s):    00 - 59 ; 600 - 899 ; 9000 - 9099 ; 91000 - 99999
     ((isbn-in-rangep "970-00"      "970-59"    ))
     ((isbn-in-rangep "970-600"     "970-899"   ))
     ((isbn-in-rangep "970-9000"    "970-9099"  ))
     ((isbn-in-rangep "970-91000"   "970-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Philippines
     ;; Country code(s): PH
     ;; Language group:  971
     ;; Publisher(s):    00 - 49 ; 500 - 849 ; 8500 - 9099 ; 91000 - 99999
     ((isbn-in-rangep "971-00"      "971-49"    ))
     ((isbn-in-rangep "971-500"     "971-849"   ))
     ((isbn-in-rangep "971-8500"    "971-9099"  ))
     ((isbn-in-rangep "971-91000"   "971-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Portugal
     ;; Country code(s): PT
     ;; Language group:  972
     ;; Publisher(s):    0 - 1 ; 20 - 54 ; 550 - 799 ; 8000 - 9499 ; 95000 - 99999
     ((isbn-in-rangep "972-0"       "972-1"     ))
     ((isbn-in-rangep "972-20"      "972-54"    ))
     ((isbn-in-rangep "972-550"     "972-799"   ))
     ((isbn-in-rangep "972-8000"    "972-9499"  ))
     ((isbn-in-rangep "972-95000"   "972-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Romania
     ;; Country code(s): RO
     ;; Language group:  973
     ;; Publisher(s):    0 - 1 ; 20 - 54 ; 550 - 799 ; 8000 - 8499 ; 85000 - 99999
     ((isbn-in-rangep "973-0"       "973-1"     ))
     ((isbn-in-rangep "973-20"      "973-54"    ))
     ((isbn-in-rangep "973-550"     "973-799"   ))
     ((isbn-in-rangep "973-8000"    "973-8499"  ))
     ((isbn-in-rangep "973-85000"   "973-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Thailand
     ;; Country code(s): TH
     ;; Language group:  974
     ;; Publisher(s):    00 - 19 ; 200 - 699 ; 7000 - 8499 ; 85000 - 89999
     ((isbn-in-rangep "974-00"      "974-19"    ))
     ((isbn-in-rangep "974-200"     "974-699"   ))
     ((isbn-in-rangep "974-7000"    "974-8499"  ))
     ((isbn-in-rangep "974-85000"   "974-89999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Turkey
     ;; Country code(s): TR
     ;; Language group:  975
     ;; Publisher(s):    00 - 29 ; 300 - 599 ; 6000 - 9199 ; 92000 - 97999
     ((isbn-in-rangep "975-00"      "975-29"    ))
     ((isbn-in-rangep "975-300"     "975-599"   ))
     ((isbn-in-rangep "975-6000"    "975-9199"  ))
     ((isbn-in-rangep "975-92000"   "975-97999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Caribbean Community
     ;; Country code(s): Antigua AG, Bahamas BS, Barbados BB, Belize BZ, Cayman Islands KY, Dominica DM, Grenada GD, Guyana GY, Jamaica JM, Montserrat MS, St.  Kitts-Nevis KN, St.  Lucia LC, St.  Vincent VC, Trinidad and Tobago TT, Virgin Islands (Br.) VG
     ;; Language group:  976
     ;; Publisher(s):    0 - 3 ; 40 - 59 ; 600 - 799 ; 8000 - 9499 ; 95000 - 99999
     ((isbn-in-rangep "976-0"       "976-3"     ))
     ((isbn-in-rangep "976-40"      "976-59"    ))
     ((isbn-in-rangep "976-600"     "976-799"   ))
     ((isbn-in-rangep "976-8000"    "976-9499"  ))
     ((isbn-in-rangep "976-95000"   "976-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Egypt
     ;; Country code(s): EG
     ;; Language group:  977
     ;; Publisher(s):    00 - 19 ; 200 - 499 ; 5000 - 6999 ; 70000 - 99999
     ((isbn-in-rangep "977-00"      "977-19"    ))
     ((isbn-in-rangep "977-200"     "977-499"   ))
     ((isbn-in-rangep "977-5000"    "977-6999"  ))
     ((isbn-in-rangep "977-70000"   "977-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Nigeria
     ;; Country code(s): NG
     ;; Language group:  978
     ;; Publisher(s):    000 - 199 ; 2000 - 2999 ; 30000 - 79999 ; 8000 - 8999 ; 900 - 999
     ((isbn-in-rangep "978-000"     "978-199"   ))
     ((isbn-in-rangep "978-2000"    "978-2999"  ))
     ((isbn-in-rangep "978-30000"   "978-79999" ))
     ((isbn-in-rangep "978-8000"    "978-8999"  ))
     ((isbn-in-rangep "978-900"     "978-999"   ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Indonesia
     ;; Country code(s): ID
     ;; Language group:  979
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Venezuela
     ;; Country code(s): VE
     ;; Language group:  980
     ;; Publisher(s):    00 - 19 ; 200 - 599 ; 6000 - 9999
     ((isbn-in-rangep "980-00"      "980-19"    ))
     ((isbn-in-rangep "980-200"     "980-599"   ))
     ((isbn-in-rangep "980-6000"    "980-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Singapore
     ;; Country code(s): SG
     ;; Language group:  981
     ;; Publisher(s):    00 - 19 ; 200 - 299 ; 3000 - 9999
     ((isbn-in-rangep "981-00"      "981-19"    ))
     ((isbn-in-rangep "981-200"     "981-299"   ))
     ((isbn-in-rangep "981-3000"    "981-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     South Pacific
     ;; Country code(s): Cook Islands CK, Fiji FJ, Kiribati KI, Nauru NR, New Caledonia NC, Niue NU, Samoa WS, Solomon Islands SB, Tokelau TK, Tonga TO, Tuvalu TV, Vanuatu VU
     ;; Language group:  982
     ;; Publisher(s):    00 - 09 ; 100 - 699 ; 70 - 89 ; 9000 - 9999
     ((isbn-in-rangep "982-00"      "982-09"    ))
     ((isbn-in-rangep "982-100"     "982-699"   ))
     ((isbn-in-rangep "982-70"      "982-89"    ))
     ((isbn-in-rangep "982-9000"    "982-9999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Malaysia
     ;; Country code(s): MY
     ;; Language group:  983
     ;; Publisher(s):    00 - 01 ; 020 - 199 ; 2000 - 3999 ; 40000 - 49999 ; 50 - 79 ; 800 - 899 ; 9000 - 9899 ; 99000 - 99999
     ((isbn-in-rangep "983-00"      "983-01"    ))
     ((isbn-in-rangep "983-020"     "983-199"   ))
     ((isbn-in-rangep "983-2000"    "983-3999"  ))
     ((isbn-in-rangep "983-40000"   "983-49999" ))
     ((isbn-in-rangep "983-50"      "983-79"    ))
     ((isbn-in-rangep "983-800"     "983-899"   ))
     ((isbn-in-rangep "983-9000"    "983-9899"  ))
     ((isbn-in-rangep "983-99000"   "983-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Bangladesh
     ;; Country code(s): BD
     ;; Language group:  984
     ;; Publisher(s):    00 - 39 ; 400 - 799 ; 8000 - 8999 ; 90000 - 99999
     ((isbn-in-rangep "984-00"      "984-39"    ))
     ((isbn-in-rangep "984-400"     "984-799"   ))
     ((isbn-in-rangep "984-8000"    "984-8999"  ))
     ((isbn-in-rangep "984-90000"   "984-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Belarus
     ;; Country code(s): BY
     ;; Language group:  985
     ;; Publisher(s):    00 - 39 ; 400 - 599 ; 6000 - 8999 ; 90000 - 99999
     ((isbn-in-rangep "985-00"      "985-39"    ))
     ((isbn-in-rangep "985-400"     "985-599"   ))
     ((isbn-in-rangep "985-6000"    "985-8999"  ))
     ((isbn-in-rangep "985-90000"   "985-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Taiwan, China
     ;; Country code(s): TW
     ;; Language group:  986
     ;; Publisher(s):    00 - 19 ; 200 - 559 ; 5600 - 7999 ; 8000 - 99999
     ((isbn-in-rangep "986-00"      "986-19"    ))
     ((isbn-in-rangep "986-200"     "986-559"   ))
     ((isbn-in-rangep "986-5600"    "986-7999"  ))
     ((isbn-in-rangep "986-8000"    "986-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Argentina
     ;; Country code(s): AR
     ;; Language group:  987
     ;; Publisher(s):    00 - 09 ; 1000 - 1999 ; 20000 - 29999 ; 30 - 49 ; 500 - 899 ; 9000 - 9499 ; 95000 - 99999
     ((isbn-in-rangep "987-00"      "987-09"    ))
     ((isbn-in-rangep "987-1000"    "987-1999"  ))
     ((isbn-in-rangep "987-20000"   "987-29999" ))
     ((isbn-in-rangep "987-30"      "987-49"    ))
     ((isbn-in-rangep "987-500"     "987-899"   ))
     ((isbn-in-rangep "987-9000"    "987-9499"  ))
     ((isbn-in-rangep "987-95000"   "987-99999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Kosova (soon operational)
     ;; Country code(s): YU
     ;; Language group:  9951
     ;; Publisher(s):    ??????? - ???????
     ;; ((isbn-in-rangep "9951-???????"  "9951-???????"))
     ;; ----------------------------------------------------------------
     ;; Region name:     Azerbaijan
     ;; Country code(s): AZ
     ;; Language group:  9952
     ;; Publisher(s):    0 ; 20 - 29 ; 400 - 599 ; 8000 - 9999
     ((isbn-in-rangep "9952-0"      "9952-0"    ))
     ((isbn-in-rangep "9952-20"     "9952-29"   ))
     ((isbn-in-rangep "9952-400"    "9952-599"  ))
     ((isbn-in-rangep "9952-8000"   "9952-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Lebanon
     ;; Country code(s): LB
     ;; Language group:  9953
     ;; Publisher(s):    0 ; 10-39 ; 400-899 ; 9000-9999
     ((isbn-in-rangep "9953-0"      "9953-0"    ))
     ((isbn-in-rangep "9953-10"     "9953-39"   ))
     ((isbn-in-rangep "9953-400"    "9953-899"  ))
     ((isbn-in-rangep "9953-9000"   "9953-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Morocco
     ;; Country code(s): MA
     ;; Language group:  9954
     ;; Publisher(s):    0 - 1 ; 20 - 39 ; 400 - 799 ; 8000 - 9999
     ((isbn-in-rangep "9954-0"      "9954-1"    ))
     ((isbn-in-rangep "9954-20"     "9954-39"   ))
     ((isbn-in-rangep "9954-400"    "9954-799"  ))
     ((isbn-in-rangep "9954-8000"   "9954-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Lithuania
     ;; Country code(s): LT
     ;; Language group:  9955
     ;; Publisher(s):    00 - 39 ; 400 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9955-00"     "9955-39"   ))
     ((isbn-in-rangep "9955-400"    "9955-899"  ))
     ((isbn-in-rangep "9955-9000"   "9955-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Cameroon
     ;; Country code(s): CM
     ;; Language group:  9956
     ;; Publisher(s):    0 ; 10 - 39 ; 400 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9956-0"      "9956-0"    ))
     ((isbn-in-rangep "9956-10"     "9956-39"   ))
     ((isbn-in-rangep "9956-400"    "9956-899"  ))
     ((isbn-in-rangep "9956-9000"   "9956-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Jordan
     ;; Country code(s): JO
     ;; Language group:  9957
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Bosnia and Herzegovina
     ;; Country code(s): BA
     ;; Language group:  9958
     ;; Publisher(s):    0 ; 10 - 49 ; 500 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9958-0"      "9958-0"    ))
     ((isbn-in-rangep "9958-10"     "9958-49"   ))
     ((isbn-in-rangep "9958-500"    "9958-899"  ))
     ((isbn-in-rangep "9958-9000"   "9958-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Libya
     ;; Country code(s): LY
     ;; Language group:  9959
     ;; Publisher(s):    0 - 1 ; 20 - 79 ; 800 - 949 ; 9500 - 9999
     ((isbn-in-rangep "9959-0"      "9959-1"    ))
     ((isbn-in-rangep "9959-20"     "9959-79"   ))
     ((isbn-in-rangep "9959-800"    "9959-949"  ))
     ((isbn-in-rangep "9959-9500"   "9959-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Saudi Arabia
     ;; Country code(s): SA
     ;; Language group:  9960
     ;; Publisher(s):    00 - 59 ; 600 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9960-00"     "9960-59"   ))
     ((isbn-in-rangep "9960-600"    "9960-899"  ))
     ((isbn-in-rangep "9960-9000"   "9960-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Algeria
     ;; Country code(s): DZ
     ;; Language group:  9961
     ;; Publisher(s):    0 - 4 ; 50 - 79 ; 800 - 949 ; 9500 - 9999
     ((isbn-in-rangep "9961-0"      "9961-4"    ))
     ((isbn-in-rangep "9961-50"     "9961-79"   ))
     ((isbn-in-rangep "9961-800"    "9961-949"  ))
     ((isbn-in-rangep "9961-9500"   "9961-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Panama
     ;; Country code(s): PA
     ;; Language group:  9962
     ;; Publisher(s):    00 - 59 ; 600 - 849 ; 8500 - 9999
     ((isbn-in-rangep "9962-00"     "9962-59"   ))
     ((isbn-in-rangep "9962-600"    "9962-849"  ))
     ((isbn-in-rangep "9962-8500"   "9962-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Cyprus
     ;; Country code(s): CY
     ;; Language group:  9963
     ;; Publisher(s):    0 - 2 ; 30 - 54 ; 550 - 749 ; 7500 - 9999
     ((isbn-in-rangep "9963-0"      "9963-2"    ))
     ((isbn-in-rangep "9963-30"     "9963-54"   ))
     ((isbn-in-rangep "9963-550"    "9963-749"  ))
     ((isbn-in-rangep "9963-7500"   "9963-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Ghana
     ;; Country code(s): GH
     ;; Language group:  9964
     ;; Publisher(s):    0 - 6 ; 70 - 94 ; 950 - 999
     ((isbn-in-rangep "9964-0"      "9964-6"    ))
     ((isbn-in-rangep "9964-70"     "9964-94"   ))
     ((isbn-in-rangep "9964-950"    "9964-999"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Kazakhstan
     ;; Country code(s): KZ
     ;; Language group:  9965
     ;; Publisher(s):    00 - 39 ; 400 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9965-00"     "9965-39"   ))
     ((isbn-in-rangep "9965-400"    "9965-899"  ))
     ((isbn-in-rangep "9965-9000"   "9965-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Kenya
     ;; Country code(s): KE
     ;; Language group:  9966
     ;; Publisher(s):    00 - 69 ; 800 - 959 ; 9600 - 9999
     ((isbn-in-rangep "9966-00"     "9966-69"   ))
     ((isbn-in-rangep "9966-800"    "9966-959"  ))
     ((isbn-in-rangep "9966-9600"   "9966-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Kyrgyzstan
     ;; Country code(s): KG
     ;; Language group:  9967
     ;; Publisher(s):    00 - 39 ; 400 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9967-00"     "9967-39"   ))
     ((isbn-in-rangep "9967-400"    "9967-899"  ))
     ((isbn-in-rangep "9967-9000"   "9967-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Costa Rica
     ;; Country code(s): CR
     ;; Language group:  9968
     ;; Publisher(s):    0 ; 10 - 69 ; 700 - 969 ; 9700 - 9999
     ((isbn-in-rangep "9968-0"      "9968-0"    ))
     ((isbn-in-rangep "9968-10"     "9968-69"   ))
     ((isbn-in-rangep "9968-700"    "9968-969"  ))
     ((isbn-in-rangep "9968-9700"   "9968-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Uganda
     ;; Country code(s): UG
     ;; Language group:  9970
     ;; Publisher(s):    00 - 39 ; 400 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9970-00"     "9970-39"   ))
     ((isbn-in-rangep "9970-400"    "9970-899"  ))
     ((isbn-in-rangep "9970-9000"   "9970-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Singapore
     ;; Country code(s): SG
     ;; Language group:  9971
     ;; Publisher(s):    0 - 5 ; 60 - 89 ; 900 - 989 ; 9900 - 9999
     ((isbn-in-rangep "9971-0"      "9971-5"    ))
     ((isbn-in-rangep "9971-60"     "9971-89"   ))
     ((isbn-in-rangep "9971-900"    "9971-989"  ))
     ((isbn-in-rangep "9971-9900"   "9971-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Peru
     ;; Country code(s): PE
     ;; Language group:  9972
     ;; Publisher(s):    00 - 09 ; 1 - 3 ; 40 - 59 ; 600 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9972-00"     "9972-09"   ))
     ((isbn-in-rangep "9972-1"      "9972-3"    ))
     ((isbn-in-rangep "9972-40"     "9972-59"   ))
     ((isbn-in-rangep "9972-600"    "9972-899"  ))
     ((isbn-in-rangep "9972-9000"   "9972-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Tunisia
     ;; Country code(s): TN
     ;; Language group:  9973
     ;; Publisher(s):    0 ; 10 - 69 ; 700 - 969 ; 9700 - 9999
     ((isbn-in-rangep "9973-0"      "9973-0"    ))
     ((isbn-in-rangep "9973-10"     "9973-69"   ))
     ((isbn-in-rangep "9973-700"    "9973-969"  ))
     ((isbn-in-rangep "9973-9700"   "9973-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Uruguay
     ;; Country code(s): UY
     ;; Language group:  9974
     ;; Publisher(s):    0 - 2 ; 30 - 54 ; 550 - 749 ; 7500 - 9999
     ((isbn-in-rangep "9974-0"      "9974-2"    ))
     ((isbn-in-rangep "9974-30"     "9974-54"   ))
     ((isbn-in-rangep "9974-550"    "9974-749"  ))
     ((isbn-in-rangep "9974-7500"   "9974-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Moldova
     ;; Country code(s): MD
     ;; Language group:  9975
     ;; Publisher(s):    0 - 4 ; 50 - 89 ; 900 - 949 ; 9500 - 9999
     ((isbn-in-rangep "9975-0"      "9975-4"    ))
     ((isbn-in-rangep "9975-50"     "9975-89"   ))
     ((isbn-in-rangep "9975-900"    "9975-949"  ))
     ((isbn-in-rangep "9975-9500"   "9975-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Tanzania
     ;; Country code(s): TZ
     ;; Language group:  9976
     ;; Publisher(s):    0 - 5 ; 60 - 89 ; 900 - 989 ; 9990 - 9999
     ((isbn-in-rangep "9976-0"      "9976-5"    ))
     ((isbn-in-rangep "9976-60"     "9976-89"   ))
     ((isbn-in-rangep "9976-900"    "9976-989"  ))
     ((isbn-in-rangep "9976-9990"   "9976-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Costa Rica
     ;; Country code(s): CR
     ;; Language group:  9977
     ;; Publisher(s):    00 - 89 ; 900 - 989 ; 9900 - 9999
     ((isbn-in-rangep "9977-00"     "9977-89"   ))
     ((isbn-in-rangep "9977-900"    "9977-989"  ))
     ((isbn-in-rangep "9977-9900"   "9977-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Ecuador
     ;; Country code(s): EC
     ;; Language group:  9978
     ;; Publisher(s):    00 - 94 ; 950 - 989 ; 9900 - 9999
     ((isbn-in-rangep "9978-00"     "9978-94"   ))
     ((isbn-in-rangep "9978-950"    "9978-989"  ))
     ((isbn-in-rangep "9978-9900"   "9978-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Iceland
     ;; Country code(s): IS
     ;; Language group:  9979
     ;; Publisher(s):
     ;; ----------------------------------------------------------------
     ;; Region name:     Papua New Guinea
     ;; Country code(s): PG
     ;; Language group:  9980
     ;; Publisher(s):    0 - 3 ; 40 - 89 ; 900 - 989
     ((isbn-in-rangep "9980-0"      "9980-3"    ))
     ((isbn-in-rangep "9980-40"     "9980-89"   ))
     ((isbn-in-rangep "9980-900"    "9980-989"  ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Morocco
     ;; Country code(s): MA
     ;; Language group:  9981
     ;; Publisher(s):    00 - 09 ; 100 - 159 ; 1600 - 1999 ; 20 - 79 ; 800 - 949 ; 9500 - 9999
     ((isbn-in-rangep "9981-00"     "9981-09"   ))
     ((isbn-in-rangep "9981-100"    "9981-159"  ))
     ((isbn-in-rangep "9981-1600"   "9981-1999" ))
     ((isbn-in-rangep "9981-20"     "9981-79"   ))
     ((isbn-in-rangep "9981-800"    "9981-949"  ))
     ((isbn-in-rangep "9981-9500"   "9981-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Zambia
     ;; Country code(s): ZM
     ;; Language group:  9982
     ;; Publisher(s):    00 - 79 ; 800 - 889 ; 9900 - 9999
     ((isbn-in-rangep "9982-00"     "9982-79"   ))
     ((isbn-in-rangep "9982-800"    "9982-889"  ))
     ((isbn-in-rangep "9982-9900"   "9982-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Gambia
     ;; Country code(s): GM
     ;; Language group:  9983
     ;; Publisher(s):    80 - 94 ; 950 - 989 ; 9900 - 9999
     ((isbn-in-rangep "9983-80"     "9983-94"   ))
     ((isbn-in-rangep "9983-950"    "9983-989"  ))
     ((isbn-in-rangep "9983-9900"   "9983-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Latvia
     ;; Country code(s): LV
     ;; Language group:  9984
     ;; Publisher(s):    00 - 49 ; 500 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9984-00"     "9984-49"   ))
     ((isbn-in-rangep "9984-500"    "9984-899"  ))
     ((isbn-in-rangep "9984-9000"   "9984-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Estonia
     ;; Country code(s): EE
     ;; Language group:  9985
     ;; Publisher(s):    0 - 4 ; 50 - 79 ; 800 - 899 ; 9000 - 9999
     ((isbn-in-rangep "9985-0"      "9985-4"    ))
     ((isbn-in-rangep "9985-50"     "9985-79"   ))
     ((isbn-in-rangep "9985-800"    "9985-899"  ))
     ((isbn-in-rangep "9985-9000"   "9985-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Lithuania
     ;; Country code(s): LT
     ;; Language group:  9986
     ;; Publisher(s):    00 - 39 ; 400 - 899 ; 9000 - 9399 ; 940 - 969 ; 97 - 99
     ((isbn-in-rangep "9986-00"     "9986-39"   ))
     ((isbn-in-rangep "9986-400"    "9986-899"  ))
     ((isbn-in-rangep "9986-9000"   "9986-9399" ))
     ((isbn-in-rangep "9986-940"    "9986-969"  ))
     ((isbn-in-rangep "9986-97"     "9986-99"   ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Tanzania
     ;; Country code(s): TZ
     ;; Language group:  9987
     ;; Publisher(s):    00 - 39 ; 400 - 879 ; 8800 - 9999
     ((isbn-in-rangep "9987-00"     "9987-39"   ))
     ((isbn-in-rangep "9987-400"    "9987-879"  ))
     ((isbn-in-rangep "9987-8800"   "9987-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Ghana
     ;; Country code(s): GH
     ;; Language group:  9988
     ;; Publisher(s):    0 - 2 ; 30 - 54 ; 550 - 749 ; 7500 - 9999
     ((isbn-in-rangep "9988-0"      "9988-2"    ))
     ((isbn-in-rangep "9988-30"     "9988-54"   ))
     ((isbn-in-rangep "9988-550"    "9988-749"  ))
     ((isbn-in-rangep "9988-7500"   "9988-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Macedonia
     ;; Country code(s): MK
     ;; Language group:  9989
     ;; Publisher(s):    0 - 2 ; 30 - 59 ; 600 - 849 ; 8500 - 9999
     ((isbn-in-rangep "9989-0"      "9989-2"    ))
     ((isbn-in-rangep "9989-30"     "9989-59"   ))
     ((isbn-in-rangep "9989-600"    "9989-849"  ))
     ((isbn-in-rangep "9989-8500"   "9989-9999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Bahrain
     ;; Country code(s): BH
     ;; Language group:  99901
     ;; Publisher(s):    00 - 49 ; 500 - 999
     ((isbn-in-rangep "99901-00"    "99901-49"  ))
     ((isbn-in-rangep "99901-500"   "99901-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Mauritius
     ;; Country code(s): MU
     ;; Language group:  99903
     ;; Publisher(s):    0 - 1 ; 20 - 89 ; 900 -999
     ((isbn-in-rangep "99903-0"     "99903-1"   ))
     ((isbn-in-rangep "99903-20"    "99903-89"  ))
     ((isbn-in-rangep "99903-900"   "99903-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Netherlands Antilles
     ;; Country code(s): AN
     ;; Language group:  99904
     ;; Publisher(s):    0 - 5 ; 60 - 89 ; 900 - 999
     ((isbn-in-rangep "99904-0"     "99904-5"   ))
     ((isbn-in-rangep "99904-60"    "99904-89"  ))
     ((isbn-in-rangep "99904-900"   "99904-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Bolivia
     ;; Country code(s): BO
     ;; Language group:  99905
     ;; Publisher(s):    0 - 3 ; 40 - 79 ; 800 - 999
     ((isbn-in-rangep "99905-0"     "99905-3"   ))
     ((isbn-in-rangep "99905-40"    "99905-79"  ))
     ((isbn-in-rangep "99905-800"   "99905-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Kuwait
     ;; Country code(s): KW
     ;; Language group:  99906
     ;; Publisher(s):    0 - 2 ; 30 - 59 ; 600 - 999
     ((isbn-in-rangep "99906-0"     "99906-2"   ))
     ((isbn-in-rangep "99906-30"    "99906-59"  ))
     ((isbn-in-rangep "99906-600"   "99906-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Malawi
     ;; Country code(s): MW
     ;; Language group:  99908
     ;; Publisher(s):    0 ; 10 - 89 ; 900 - 999
     ((isbn-in-rangep "99908-0"     "99908-0"   ))
     ((isbn-in-rangep "99908-10"    "99908-89"  ))
     ((isbn-in-rangep "99908-900"   "99908-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Malta
     ;; Country code(s): MT
     ;; Language group:  99909
     ;; Publisher(s):    0 - 3 ; 40 - 94 ; 950 - 999
     ((isbn-in-rangep "99909-0"     "99909-3"   ))
     ((isbn-in-rangep "99909-40"    "99909-94"  ))
     ((isbn-in-rangep "99909-950"   "99909-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Sierra Leone
     ;; Country code(s): SL
     ;; Language group:  99910
     ;; Publisher(s):    0 - 2 ; 30 - 89 ; 900 - 999
     ((isbn-in-rangep "99910-0"     "99910-2"   ))
     ((isbn-in-rangep "99910-30"    "99910-89"  ))
     ((isbn-in-rangep "99910-900"   "99910-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Lesotho
     ;; Country code(s): LS
     ;; Language group:  99911
     ;; Publisher(s):    00 - 59 ; 600 - 999
     ((isbn-in-rangep "99911-00"    "99911-59"  ))
     ((isbn-in-rangep "99911-600"   "99911-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Botswana
     ;; Country code(s): BW
     ;; Language group:  99912
     ;; Publisher(s):    0 - 5 ; 60 - 89 ; 900 - 999
     ((isbn-in-rangep "99912-0"     "99912-5"   ))
     ((isbn-in-rangep "99912-60"    "99912-89"  ))
     ((isbn-in-rangep "99912-900"   "99912-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Andorra
     ;; Country code(s): AD
     ;; Language group:  99913
     ;; Publisher(s):    ??????? - ???????
     ;; ((isbn-in-rangep "99913-???????"  "99913-???????"))
     ;; ----------------------------------------------------------------
     ;; Region name:     Suriname
     ;; Country code(s): SR
     ;; Language group:  99914
     ;; Publisher(s):    0 - 4 ; 50 - 89 ; 900 - 949
     ((isbn-in-rangep "99914-0"     "99914-4"   ))
     ((isbn-in-rangep "99914-50"    "99914-89"  ))
     ((isbn-in-rangep "99914-900"   "99914-949" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Maldives
     ;; Country code(s): MV
     ;; Language group:  99915
     ;; Publisher(s):    0 - 4 ; 50 - 79 ; 800 - 999
     ((isbn-in-rangep "99915-0"     "99915-4"   ))
     ((isbn-in-rangep "99915-50"    "99915-79"  ))
     ((isbn-in-rangep "99915-800"   "99915-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Namibia
     ;; Country code(s): NA
     ;; Language group:  99916
     ;; Publisher(s):    0 - 2 ; 30 - 69 ; 700 - 999
     ((isbn-in-rangep "99916-0"     "99916-2"   ))
     ((isbn-in-rangep "99916-30"    "99916-69"  ))
     ((isbn-in-rangep "99916-700"   "99916-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Brunei Darussalam
     ;; Country code(s): BN
     ;; Language group:  99917
     ;; Publisher(s):    0 - 2 ; 30 - 89 ; 900 - 999
     ((isbn-in-rangep "99917-0"     "99917-2"   ))
     ((isbn-in-rangep "99917-30"    "99917-89"  ))
     ((isbn-in-rangep "99917-900"   "99917-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Faroe Islands
     ;; Country code(s): FO
     ;; Language group:  99918
     ;; Publisher(s):    0 - 3 ; 40 - 89 ; 900 - 999
     ((isbn-in-rangep "99918-0"     "99918-3"   ))
     ((isbn-in-rangep "99918-40"    "99918-89"  ))
     ((isbn-in-rangep "99918-900"   "99918-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Benin
     ;; Country code(s): BJ
     ;; Language group:  99919
     ;; Publisher(s):    0 - 2 ; 40 - 69 ; 900 - 999
     ((isbn-in-rangep "99919-0"     "99919-2"   ))
     ((isbn-in-rangep "99919-40"    "99919-69"  ))
     ((isbn-in-rangep "99919-900"   "99919-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Andorra
     ;; Country code(s): AD
     ;; Language group:  99920
     ;; Publisher(s):    0 - 4 ; 50 - 89 ; 900 - 999
     ((isbn-in-rangep "99920-0"     "99920-4"   ))
     ((isbn-in-rangep "99920-50"    "99920-89"  ))
     ((isbn-in-rangep "99920-900"   "99920-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Qatar
     ;; Country code(s): QA
     ;; Language group:  99921
     ;; Publisher(s):    0 - 1 ; 20 - 69 ; 700 - 999
     ((isbn-in-rangep "99921-0"     "99921-1"   ))
     ((isbn-in-rangep "99921-20"    "99921-69"  ))
     ((isbn-in-rangep "99921-700"   "99921-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Guatemala
     ;; Country code(s): GT
     ;; Language group:  99922
     ;; Publisher(s):    0 - 4 ; 50 - 79 ; 800 - 900 ; ??????? - ???????
     ((isbn-in-rangep "99922-0"     "99922-4"   ))
     ((isbn-in-rangep "99922-50"    "99922-79"  ))
     ((isbn-in-rangep "99922-800"   "99922-900" ))
     ;; ((isbn-in-rangep "99922-???????"  "99922-???????"))
     ;; ----------------------------------------------------------------
     ;; Region name:     El Salvador
     ;; Country code(s): SV
     ;; Language group:  99923
     ;; Publisher(s):    0 - 1 ; 20 - 79 ; 800 - 999
     ((isbn-in-rangep "99923-0"     "99923-1"   ))
     ((isbn-in-rangep "99923-20"    "99923-79"  ))
     ((isbn-in-rangep "99923-800"   "99923-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Nicaragua
     ;; Country code(s): NI
     ;; Language group:  99924
     ;; Publisher(s):    0 - 2 ; 30 - 79 ; 800 - 900
     ((isbn-in-rangep "99924-0"     "99924-2"   ))
     ((isbn-in-rangep "99924-30"    "99924-79"  ))
     ((isbn-in-rangep "99924-800"   "99924-900" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Paraguay
     ;; Country code(s): PY
     ;; Language group:  99925
     ;; Publisher(s):    0 - 3 ; 40 - 79 ; 800 - 999
     ((isbn-in-rangep "99925-0"     "99925-3"   ))
     ((isbn-in-rangep "99925-40"    "99925-79"  ))
     ((isbn-in-rangep "99925-800"   "99925-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Honduras
     ;; Country code(s): HN
     ;; Language group:  99926
     ;; Publisher(s):    0 ; 10 - 59 ; 600 - 999
     ((isbn-in-rangep "99926-0"     "99926-0"   ))
     ((isbn-in-rangep "99926-10"    "99926-59"  ))
     ((isbn-in-rangep "99926-600"   "99926-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Albania
     ;; Country code(s): AL
     ;; Language group:  99927
     ;; Publisher(s):    0 - 2 ; 30 - 59 ; 600 - 999
     ((isbn-in-rangep "99927-0"     "99927-2"   ))
     ((isbn-in-rangep "99927-30"    "99927-59"  ))
     ((isbn-in-rangep "99927-600"   "99927-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Georgia
     ;; Country code(s): GE
     ;; Language group:  99928
     ;; Publisher(s):    0 ; 10 - 79 ; 800 - 999
     ((isbn-in-rangep "99928-0"     "99928-0"   ))
     ((isbn-in-rangep "99928-10"    "99928-79"  ))
     ((isbn-in-rangep "99928-800"   "99928-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Mongolia
     ;; Country code(s): MN
     ;; Language group:  99929
     ;; Publisher(s):    ??????? - ???????
     ;; ((isbn-in-rangep "99929-???????"  "99929-???????"))
     ;; ----------------------------------------------------------------
     ;; Region name:     Armenia
     ;; Country code(s): AM
     ;; Language group:  99930
     ;; Publisher(s):    0 - 4 ; 50 - 79 ; 800 - 999
     ((isbn-in-rangep "99930-0"     "99930-4"   ))
     ((isbn-in-rangep "99930-50"    "99930-79"  ))
     ((isbn-in-rangep "99930-800"   "99930-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Seychelles
     ;; Country code(s): SC
     ;; Language group:  99931
     ;; Publisher(s):    0 - 4 ; 50 - 79 ; 800 - 999
     ((isbn-in-rangep "99931-0"     "99931-4"   ))
     ((isbn-in-rangep "99931-50"    "99931-79"  ))
     ((isbn-in-rangep "99931-800"   "99931-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Malta
     ;; Country code(s): MT
     ;; Language group:  99932
     ;; Publisher(s):    0 ; 10 - 59 ; 600 -999
     ((isbn-in-rangep "99932-0"     "99932-0"   ))
     ((isbn-in-rangep "99932-10"    "99932-59"  ))
     ((isbn-in-rangep "99932-600"   "99932-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Nepal
     ;; Country code(s): NP
     ;; Language group:  99933
     ;; Publisher(s):    ??????? - ???????
     ;; ((isbn-in-rangep "99933-???????"  "99933-???????"))
     ;; ----------------------------------------------------------------
     ;; Region name:     Dominican Republic
     ;; Country code(s): DO
     ;; Language group:  99934
     ;; Publisher(s):    0 - 1 ; 20 - 79 ; 800 - 999
     ((isbn-in-rangep "99934-0"     "99934-1"   ))
     ((isbn-in-rangep "99934-20"    "99934-79"  ))
     ((isbn-in-rangep "99934-800"   "99934-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Haiti
     ;; Country code(s): HT
     ;; Language group:  99935
     ;; Publisher(s):    0 -2 ; 30 - 59 ; 600 - 999
     ((isbn-in-rangep "99935-0"     "99935-2"   ))
     ((isbn-in-rangep "99935-30"    "99935-59"  ))
     ((isbn-in-rangep "99935-600"   "99935-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Bhutan
     ;; Country code(s): BT
     ;; Language group:  99936
     ;; Publisher(s):    0 ; 10 - 59 ; 600 - 999
     ((isbn-in-rangep "99936-0"     "99936-0"   ))
     ((isbn-in-rangep "99936-10"    "99936-59"  ))
     ((isbn-in-rangep "99936-600"   "99936-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Macau
     ;; Country code(s): MO
     ;; Language group:  99937
     ;; Publisher(s):    0 - 1 ; 20 - 59 ; 600 - 999
     ((isbn-in-rangep "99937-0"     "99937-1"   ))
     ((isbn-in-rangep "99937-20"    "99937-59"  ))
     ((isbn-in-rangep "99937-600"   "99937-999" ))
     ;; ----------------------------------------------------------------
     ;; Region name:     Srpska (Republic of)
     ;; Country code(s): BA
     ;; Language group:  99938
     ;; Publisher(s):    0 - 2 ; 30 - 59 ; 600 - 999
     ((isbn-in-rangep "99938-0"     "99938-2"   ))
     ((isbn-in-rangep "99938-30"    "99938-59"  ))
     ((isbn-in-rangep "99938-600"   "99938-999" ))

     ;; From http://www.bowker.com/standards/home/isbn/international/hyphenation-instructions.html
     ;; The publisher prefix ranges in the English group (U.S., U.K.,
     ;; Canada, Australia, New Zealand, etc) are as follows:
     ;;
     ;;      Group                   If Number Ranges         Insert Hyphens
     ;;      Identifier "0"           are Between              After
     ;;      -------------------------------------------------------------------
     ;;      00.......19                00-19             1st  3rd  9th digit
     ;;      200......699               20-69              "   4th       "
     ;;      7000.....8499              70-84              "   5th       "
     ;;      85000....89999             85-89              "   6th       "
     ;;      900000...949999            90-94              "   7th       "
     ;;      9500000..9999999           95-99              "   8th       "
     ;;
     ;;
     ;;
     ;;      Group                   If Number Ranges        Insert Hyphens
     ;;      Identifier "1"           are Between             After
     ;;      -------------------------------------------------------------------
     ;;      55000....86979           5500-8697           1st  6th  9th digit
     ;;      869800...998999          8698-9989            "   7th       "
     ;;      9990000..9999999         9990-9999            "   8th       "
     ;;
     ;;
     ;;
     ;;        The following table gives the range distribution of the group identifiers:
     ;;
     ;;                                 0 - 7
     ;;                                80 - 94
     ;;                               950 - 995
     ;;                              9960 - 9989
     ;;                             99900 - 99999
     ;;
     ;; These ranges are already partially covered by ranges above, but
     ;; we include them here for completeness, unless they are identical
     ;; to one above, in which case, they are commented out.  I have yet
     ;; to find comparable data for other language groups.

     ;; ((isbn-in-rangep "0-00"		"0-19"))
     ;; ((isbn-in-rangep "0-200"		"0-699"))
     ;; ((isbn-in-rangep "0-7000"	"0-8499"))
     ;; ((isbn-in-rangep "0-85000"		"0-89999"))
     ;; ((isbn-in-rangep "0-900000"	"0-949999"))
     ;; ((isbn-in-rangep "0-9500000"	"0-9999999"))

     ((isbn-in-rangep "1-55000"		"1-86979"))
     ((isbn-in-rangep "1-869800"	"1-998999"))
     ((isbn-in-rangep "1-9990000"	"1-9999999"))

     ;; Updates on Mon Nov 22 11:22:53 2004 from data in
     ;;		http://www.isbn-international.org/en/identifiers/List-of-Ranges.pdf
     ;; That file contains only bare ranges, some incomplete; there is
     ;; no additional commentary about the associated languages and regions.
     ;; For that reason, we include it as a complete block here, rather
     ;; than trying to merge it into the documented ranges above.
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "1-00"	     "1-09"	))
     ((isbn-in-rangep "1-100"        "1-399"	))
     ((isbn-in-rangep "1-4000"       "1-5499"	))
     ((isbn-in-rangep "1-55000"      "1-86979"	))
     ((isbn-in-rangep "1-869800"     "1-998999"	))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "2-00"        "2-19"	))
     ((isbn-in-rangep "2-200"       "2-349"	))
     ((isbn-in-rangep "2-400"       "2-699"	))
     ((isbn-in-rangep "2-35000"     "2-39999"	))
     ((isbn-in-rangep "2-7000"      "2-8399"	))
     ((isbn-in-rangep "2-84000"     "2-89999"	))
     ((isbn-in-rangep "2-900000"    "2-949999"	))
     ((isbn-in-rangep "2-9500000"   "2-9999999"	))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "3-00"	    "3-02"	))
     ((isbn-in-rangep "3-030"	    "3-033"	))
     ((isbn-in-rangep "3-0340"	    "3-0369"	))
     ((isbn-in-rangep "3-03700"	    "3-03999"	))
     ((isbn-in-rangep "3-04"	    "3-19"	))
     ((isbn-in-rangep "3-200"	    "3-699"	))
     ((isbn-in-rangep "3-7000"	    "3-8499"	))
     ((isbn-in-rangep "3-85000"	    "3-89999"	))
     ((isbn-in-rangep "3-900000"    "3-949999"	))
     ((isbn-in-rangep "3-9500000"   "3-9999999"	))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "4-00"	    "4-19"	))
     ((isbn-in-rangep "4-200"	    "4-699"	))
     ((isbn-in-rangep "4-7000"	    "4-8499"	))
     ((isbn-in-rangep "4-85000"	    "4-89999"	))
     ((isbn-in-rangep "4-900000"    "4-949999"	))
     ((isbn-in-rangep "4-9500000"   "4-9999999"	))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "5-00"	    "5-19"	))
     ((isbn-in-rangep "5-200"	    "5-699"	))
     ((isbn-in-rangep "5-7000"	    "5-8499"	))
     ((isbn-in-rangep "5-85000"	    "5-89999"	))
     ((isbn-in-rangep "5-900000"    "5-909999"	))
     ((isbn-in-rangep "5-91000"	    "5-91999"	))
     ((isbn-in-rangep "5-9200"	    "5-9299"	))
     ((isbn-in-rangep "5-93000"	    "5-94999"	))
     ((isbn-in-rangep "5-9500"	    "5-9799"	))
     ((isbn-in-rangep "5-98000"	    "5-98999"	))
     ((isbn-in-rangep "5-9900000"   "5-9999999"	))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "7-00"	    "7-09"	))
     ((isbn-in-rangep "7-100"	    "7-499"	))
     ((isbn-in-rangep "7-5000"	    "7-7999"	))
     ((isbn-in-rangep "7-80000"	    "7-89999"	))
     ((isbn-in-rangep "7-900000"    "7-999999"	))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "80-00"	    "80-19"	))
     ((isbn-in-rangep "80-200"	    "80-699"	))
     ((isbn-in-rangep "80-7000"	    "80-8499"	))
     ((isbn-in-rangep "80-85000"    "80-89999"	))
     ((isbn-in-rangep "80-900000"   "80-999999"	))
     ((isbn-in-rangep "81-00"	    "81-19"	))
     ((isbn-in-rangep "81-200"	    "81-699"	))
     ((isbn-in-rangep "81-7000"	    "81-8499"	))
     ((isbn-in-rangep "81-85000"    "81-89999"	))
     ((isbn-in-rangep "81-900000"   "81-999999"	))
     ((isbn-in-rangep "82-00"	    "82-19"	))
     ((isbn-in-rangep "82-200"	    "82-699"	))
     ((isbn-in-rangep "82-7000"	    "82-8999"	))
     ((isbn-in-rangep "82-90000"    "82-98999"	))
     ((isbn-in-rangep "82-990000"   "82-999999"	))
     ((isbn-in-rangep "83-00"	    "83-19"	))
     ((isbn-in-rangep "83-200"	    "83-599"	))
     ((isbn-in-rangep "83-60000"    "83-69999"	))
     ((isbn-in-rangep "83-7000"	    "83-8499"	))
     ((isbn-in-rangep "83-85000"    "83-89999"	))
     ((isbn-in-rangep "83-900000"   "83-999999"	))
     ((isbn-in-rangep "84-00"	    "84-19"	))
     ((isbn-in-rangep "84-200"	    "84-699"	))
     ((isbn-in-rangep "84-7000"	    "84-8499"	))
     ((isbn-in-rangep "84-85000"    "84-89999"	))
     ((isbn-in-rangep "84-9000"	    "84-9199"	))
     ((isbn-in-rangep "84-920000"   "84-923999"	))
     ((isbn-in-rangep "84-92400"    "84-92999"	))
     ((isbn-in-rangep "84-930000"   "84-949999"	))
     ((isbn-in-rangep "84-95000"    "84-96999"	))
     ((isbn-in-rangep "84-9700"	    "84-9999"	))
     ((isbn-in-rangep "85-00"	    "85-19"	))
     ((isbn-in-rangep "85-200"	    "85-699"	))
     ((isbn-in-rangep "85-7000"	    "85-8499"	))
     ((isbn-in-rangep "85-85000"    "85-89999"  ))
     ((isbn-in-rangep "85-900000"   "85-979999" ))
     ((isbn-in-rangep "85-98000"    "85-99999"  ))
     ((isbn-in-rangep "86-00"       "86-29"     ))
     ((isbn-in-rangep "86-300"      "86-699"    ))
     ((isbn-in-rangep "86-7000"     "86-7999"   ))
     ((isbn-in-rangep "86-80000"    "86-89999"  ))
     ((isbn-in-rangep "86-900000"   "86-999999" ))
     ((isbn-in-rangep "87-00"       "87-29"     ))
     ((isbn-in-rangep "87-400"      "87-649"    ))
     ((isbn-in-rangep "87-7000"     "87-7999"   ))
     ((isbn-in-rangep "87-85000"    "87-94999"  ))
     ((isbn-in-rangep "87-970000"   "87-999999" ))
     ((isbn-in-rangep "88-00"       "88-19"     ))
     ((isbn-in-rangep "88-200"      "88-599"    ))
     ((isbn-in-rangep "88-6000"     "88-8499"   ))
     ((isbn-in-rangep "88-85000"    "88-89999"  ))
     ((isbn-in-rangep "88-900000"   "88-999999" ))
     ((isbn-in-rangep "89-00"       "89-24"     ))
     ((isbn-in-rangep "89-250"      "89-549"    ))
     ((isbn-in-rangep "89-5500"     "89-8499"   ))
     ((isbn-in-rangep "89-85000"    "89-94999"  ))
     ((isbn-in-rangep "89-950000"   "89-999999" ))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "90-00"       "90-19"     ))
     ((isbn-in-rangep "90-200"      "90-499"    ))
     ((isbn-in-rangep "90-5000"     "90-6999"   ))
     ((isbn-in-rangep "90-70000"    "90-79999"  ))
     ((isbn-in-rangep "90-800000"   "90-849999" ))
     ((isbn-in-rangep "90-8500"     "90-8999"   ))
     ((isbn-in-rangep "90-900000"   "90-909999" ))
     ((isbn-in-rangep "90-940000"   "90-949999" ))
     ((isbn-in-rangep "91-0"        "91-1"      ))
     ((isbn-in-rangep "91-20"       "91-49"     ))
     ((isbn-in-rangep "91-500"      "91-649"    ))
     ((isbn-in-rangep "91-7000"     "91-7999"   ))
     ((isbn-in-rangep "91-85000"    "91-94999"  ))
     ((isbn-in-rangep "91-970000"   "91-999999" ))
     ((isbn-in-rangep "92-0"        "92-5"      ))
     ((isbn-in-rangep "92-60"       "92-79"     ))
     ((isbn-in-rangep "92-800"      "92-899"    ))
     ((isbn-in-rangep "92-9000"     "92-9499"   ))
     ((isbn-in-rangep "92-95000"    "92-98999"  ))
     ((isbn-in-rangep "92-990000"   "92-999999" ))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "950-00"      "950-49"    ))
     ((isbn-in-rangep "950-500"     "950-899"   ))
     ((isbn-in-rangep "950-9000"    "950-9899"  ))
     ((isbn-in-rangep "950-99000"   "950-99999" ))
     ((isbn-in-rangep "951-0"       "951-1"     ))
     ((isbn-in-rangep "951-20"      "951-54"    ))
     ((isbn-in-rangep "951-550"     "951-889"   ))
     ((isbn-in-rangep "951-8900"    "951-9499"  ))
     ((isbn-in-rangep "951-95000"   "951-99999" ))
     ((isbn-in-rangep "952-00"      "952-19"    ))
     ((isbn-in-rangep "952-200"     "952-499"   ))
     ((isbn-in-rangep "952-5000"    "952-5999"  ))
     ((isbn-in-rangep "952-60"      "952-65"    ))
     ((isbn-in-rangep "952-6600"    "952-6699"  ))
     ((isbn-in-rangep "952-67000"   "952-69999" ))
     ((isbn-in-rangep "952-7000"    "952-7999"  ))
     ((isbn-in-rangep "952-89"      "952-94"    ))
     ((isbn-in-rangep "952-9500"    "952-9899"  ))
     ((isbn-in-rangep "952-99000"   "952-99999" ))
     ((isbn-in-rangep "953-0"       "953-0"     ))
     ((isbn-in-rangep "953-10"      "953-14"    ))
     ((isbn-in-rangep "953-150"     "953-599"   ))
     ((isbn-in-rangep "953-6000"    "953-9499"  ))
     ((isbn-in-rangep "953-95000"   "953-99999" ))
     ((isbn-in-rangep "954-00"      "954-29"    ))
     ((isbn-in-rangep "954-300"     "954-799"   ))
     ((isbn-in-rangep "954-8000"    "954-8999"  ))
     ((isbn-in-rangep "954-90000"   "954-92999" ))
     ((isbn-in-rangep "954-9300"    "954-9999"  ))
     ;; 955 0 ??
     ((isbn-in-rangep "955-1000"    "955-1999"  ))
     ((isbn-in-rangep "955-20"      "955-54"    ))
     ((isbn-in-rangep "955-550"     "955-799"   ))
     ((isbn-in-rangep "955-8000"    "955-9499"  ))
     ((isbn-in-rangep "967-9900"    "967-9989"  ))
     ((isbn-in-rangep "967-99900"   "967-99999" ))
     ((isbn-in-rangep "968-01"      "968-39"    ))
     ((isbn-in-rangep "968-400"     "968-499"   ))
     ((isbn-in-rangep "968-5000"    "968-7999"  ))
     ((isbn-in-rangep "968-800"     "968-899"   ))
     ((isbn-in-rangep "969-0"       "969-1"     ))
     ((isbn-in-rangep "969-20"      "969-39"    ))
     ((isbn-in-rangep "969-400"     "969-799"   ))
     ((isbn-in-rangep "969-8000"    "969-9999"  ))
     ((isbn-in-rangep "970-01"      "970-59"    ))
     ((isbn-in-rangep "970-600"     "970-899"   ))
     ((isbn-in-rangep "970-9000"    "970-9099"  ))
     ((isbn-in-rangep "970-91000"   "970-96999" ))
     ((isbn-in-rangep "970-9700"    "970-9999"  ))
     ((isbn-in-rangep "971-000"     "971-019"   ))
     ((isbn-in-rangep "971-02"      "971-02"    ))
     ((isbn-in-rangep "971-0300"    "971-0599"  ))
     ((isbn-in-rangep "971-06"      "971-09"    ))
     ((isbn-in-rangep "971-10"      "971-49"    ))
     ((isbn-in-rangep "971-500"     "971-849"   ))
     ((isbn-in-rangep "971-8500"    "971-9099"  ))
     ((isbn-in-rangep "971-91000"   "971-99999" ))
     ((isbn-in-rangep "972-0"       "972-1"     ))
     ((isbn-in-rangep "972-20"      "972-54"    ))
     ((isbn-in-rangep "972-550"     "972-799"   ))
     ((isbn-in-rangep "972-8000"    "972-9499"  ))
     ((isbn-in-rangep "972-95000"   "972-99999" ))
     ((isbn-in-rangep "973-0"       "973-1"     ))
     ((isbn-in-rangep "973-20"      "973-54"    ))
     ((isbn-in-rangep "973-550"     "973-769"   ))
     ((isbn-in-rangep "973-7700"    "973-8499"  ))
     ((isbn-in-rangep "973-85000"   "973-89999" ))
     ((isbn-in-rangep "973-9000"    "973-9499"  ))
     ((isbn-in-rangep "973-95000"   "973-99999" ))
     ((isbn-in-rangep "974-00"      "974-19"    ))
     ((isbn-in-rangep "974-200"     "974-699"   ))
     ((isbn-in-rangep "974-7000"    "974-8499"  ))
     ((isbn-in-rangep "974-85000"   "974-89999" ))
     ((isbn-in-rangep "974-90000"   "974-94999" ))
     ((isbn-in-rangep "974-9500"    "974-9999"  ))
     ((isbn-in-rangep "975-00"      "975-24"    ))
     ((isbn-in-rangep "975-250"     "975-599"   ))
     ((isbn-in-rangep "975-6000"    "975-9199"  ))
     ((isbn-in-rangep "975-92000"   "975-98999" ))
     ((isbn-in-rangep "976-0"       "976-3"     ))
     ((isbn-in-rangep "976-40"      "976-59"    ))
     ((isbn-in-rangep "976-600"     "976-799"   ))
     ((isbn-in-rangep "976-8000"    "976-9499"  ))
     ((isbn-in-rangep "976-95000"   "976-99999" ))
     ((isbn-in-rangep "977-00"      "977-19"    ))
     ((isbn-in-rangep "977-200"     "977-499"   ))
     ((isbn-in-rangep "977-5000"    "977-6999"  ))
     ((isbn-in-rangep "977-700"     "977-999"   ))
     ((isbn-in-rangep "978-000"     "978-199"   ))
     ((isbn-in-rangep "978-2000"    "978-2999"  ))
     ((isbn-in-rangep "978-30000"   "978-79999" ))
     ((isbn-in-rangep "978-8000"    "978-8999"  ))
     ((isbn-in-rangep "978-900"     "978-999"   ))
     ((isbn-in-rangep "979-0"       "979-0"     ))
     ((isbn-in-rangep "979-20"      "979-29"    ))
     ((isbn-in-rangep "979-3000"    "979-3999"  ))
     ((isbn-in-rangep "979-400"     "979-799"   ))
     ((isbn-in-rangep "979-8000"    "979-9499"  ))
     ((isbn-in-rangep "979-95000"   "979-99999" ))
     ((isbn-in-rangep "980-00"      "980-19"    ))
     ((isbn-in-rangep "980-200"     "980-599"   ))
     ((isbn-in-rangep "980-6000"    "980-9999"  ))
     ((isbn-in-rangep "981-00"      "981-19"    ))
     ((isbn-in-rangep "981-200"     "981-299"   ))
     ((isbn-in-rangep "981-3000"    "981-9999"  ))
     ((isbn-in-rangep "982-00"      "982-09"    ))
     ((isbn-in-rangep "982-100"     "982-699"   ))
     ((isbn-in-rangep "982-70"      "982-89"    ))
     ((isbn-in-rangep "982-9000"    "982-9999"  ))
     ((isbn-in-rangep "983-00"      "983-01"    ))
     ((isbn-in-rangep "983-020"     "983-199"   ))
     ((isbn-in-rangep "983-2000"    "983-3999"  ))
     ((isbn-in-rangep "983-40000"   "983-49999" ))
     ((isbn-in-rangep "983-50"      "983-79"    ))
     ((isbn-in-rangep "983-800"     "983-899"   ))
     ((isbn-in-rangep "983-9000"    "983-9899"  ))
     ((isbn-in-rangep "983-99000"   "983-99999" ))
     ((isbn-in-rangep "984-00"      "984-39"    ))
     ((isbn-in-rangep "984-400"     "984-799"   ))
     ((isbn-in-rangep "984-8000"    "984-8999"  ))
     ((isbn-in-rangep "984-90000"   "984-99999" ))
     ((isbn-in-rangep "985-00"      "985-39"    ))
     ((isbn-in-rangep "985-400"     "985-599"   ))
     ((isbn-in-rangep "985-6000"    "985-8999"  ))
     ((isbn-in-rangep "985-90000"   "985-99999" ))
     ((isbn-in-rangep "986-00"      "986-11"    ))
     ((isbn-in-rangep "986-120"     "986-559"   ))
     ((isbn-in-rangep "986-5600"    "986-7999"  ))
     ((isbn-in-rangep "986-80000"   "986-99999" ))
     ((isbn-in-rangep "987-00"      "987-09"    ))
     ((isbn-in-rangep "987-1000"    "987-1999"  ))
     ((isbn-in-rangep "987-20000"   "987-29999" ))
     ((isbn-in-rangep "987-30"      "987-49"    ))
     ((isbn-in-rangep "987-500"     "987-899"   ))
     ((isbn-in-rangep "987-9000"    "987-9499"  ))
     ((isbn-in-rangep "987-95000"   "987-99999" ))
     ((isbn-in-rangep "988-00"      "988-19"    ))
     ((isbn-in-rangep "988-200"     "988-799"   ))
     ((isbn-in-rangep "988-8000"    "988-9699"  ))
     ((isbn-in-rangep "988-97000"   "988-99999" ))
     ((isbn-in-rangep "989-0"       "989-1"     ))
     ((isbn-in-rangep "989-20"      "989-54"    ))
     ((isbn-in-rangep "989-550"     "989-799"   ))
     ((isbn-in-rangep "989-8000"    "989-9499"  ))
     ((isbn-in-rangep "989-95000"   "989-99999" ))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "9945-00"     "9945-39"   ))
     ((isbn-in-rangep "9945-400"    "9945-849"  ))
     ((isbn-in-rangep "9945-8500"   "9945-9999" ))
     ((isbn-in-rangep "9946-0"      "9946-1"    ))
     ((isbn-in-rangep "9946-20"     "9946-39"   ))
     ((isbn-in-rangep "9946-400"    "9946-899"  ))
     ((isbn-in-rangep "9946-9000"   "9946-9999" ))
     ((isbn-in-rangep "9947-0"      "9947-1"    ))
     ((isbn-in-rangep "9947-20"     "9947-79"   ))
     ((isbn-in-rangep "9947-800"    "9947-999"  ))
     ((isbn-in-rangep "9948-00"     "9948-39"   ))
     ((isbn-in-rangep "9948-400"    "9948-849"  ))
     ((isbn-in-rangep "9948-8500"   "9948-9999" ))
     ((isbn-in-rangep "9949-0"      "9949-0"    ))
     ((isbn-in-rangep "9949-10"     "9949-39"   ))
     ((isbn-in-rangep "9949-400"    "9949-899"  ))
     ((isbn-in-rangep "9949-9000"   "9949-9999" ))
     ;; 9950 ?? ??
     ((isbn-in-rangep "9951-00"     "9951-39"   ))
     ((isbn-in-rangep "9951-400"    "9951-849"  ))
     ((isbn-in-rangep "9951-8500"   "9951-9999" ))
     ((isbn-in-rangep "9952-0"      "9952-1"    ))
     ((isbn-in-rangep "9952-20"     "9952-39"   ))
     ((isbn-in-rangep "9952-400"    "9952-799"  ))
     ((isbn-in-rangep "9952-8000"   "9952-9999" ))
     ((isbn-in-rangep "9953-0"      "9953-0"    ))
     ((isbn-in-rangep "9953-10"     "9953-39"   ))
     ((isbn-in-rangep "9953-400"    "9953-599"  ))
     ((isbn-in-rangep "9953-60"     "9953-89"   ))
     ((isbn-in-rangep "9953-9000"   "9953-9999" ))
     ((isbn-in-rangep "9954-0"      "9954-1"    ))
     ((isbn-in-rangep "9954-20"     "9954-39"   ))
     ((isbn-in-rangep "9954-400"    "9954-799"  ))
     ((isbn-in-rangep "9954-8000"   "9954-9999" ))
     ((isbn-in-rangep "9955-00"     "9955-39"   ))
     ((isbn-in-rangep "9955-400"    "9955-929"  ))
     ((isbn-in-rangep "9955-9300"   "9955-9999" ))
     ((isbn-in-rangep "9956-0"      "9956-0"    ))
     ((isbn-in-rangep "9956-10"     "9956-39"   ))
     ((isbn-in-rangep "9956-400"    "9956-899"  ))
     ((isbn-in-rangep "9956-9000"   "9956-9999" ))
     ((isbn-in-rangep "9957-00"     "9957-39"   ))
     ((isbn-in-rangep "9957-400"    "9957-849"  ))
     ((isbn-in-rangep "9957-8500"   "9957-9999" ))
     ((isbn-in-rangep "9958-0"      "9958-0"    ))
     ((isbn-in-rangep "9958-10"     "9958-49"   ))
     ((isbn-in-rangep "9958-500"    "9958-899"  ))
     ((isbn-in-rangep "9958-9000"   "9958-9999" ))
     ((isbn-in-rangep "9959-0"      "9959-1"    ))
     ((isbn-in-rangep "9959-20"     "9959-79"   ))
     ((isbn-in-rangep "9959-800"    "9959-949"  ))
     ((isbn-in-rangep "9959-9500"   "9959-9999" ))
     ((isbn-in-rangep "9960-00"     "9960-59"   ))
     ((isbn-in-rangep "9960-600"    "9960-899"  ))
     ((isbn-in-rangep "9960-9000"   "9960-9999" ))
     ((isbn-in-rangep "9961-0"      "9961-2"    ))
     ((isbn-in-rangep "9961-30"     "9961-69"   ))
     ((isbn-in-rangep "9961-700"    "9961-949"  ))
     ((isbn-in-rangep "9961-9500"   "9961-9999" ))
     ((isbn-in-rangep "9962-00"     "9962-54"   ))
     ((isbn-in-rangep "9962-5500"   "9962-5599" ))
     ((isbn-in-rangep "9962-56"     "9962-59"   ))
     ((isbn-in-rangep "9962-600"    "9962-849"  ))
     ((isbn-in-rangep "9962-8500"   "9962-9999" ))
     ((isbn-in-rangep "9963-0"      "9963-2"    ))
     ((isbn-in-rangep "9963-30"     "9963-54"   ))
     ((isbn-in-rangep "9963-550"    "9963-749"  ))
     ((isbn-in-rangep "9963-7500"   "9963-9999" ))
     ((isbn-in-rangep "9964-0"      "9964-6"    ))
     ((isbn-in-rangep "9964-70"     "9964-94"   ))
     ((isbn-in-rangep "9964-950"    "9964-999"  ))
     ((isbn-in-rangep "9965-00"     "9965-39"   ))
     ((isbn-in-rangep "9965-400"    "9965-899"  ))
     ((isbn-in-rangep "9965-9000"   "9965-9999" ))
     ((isbn-in-rangep "9966-00"     "9966-69"   ))
     ((isbn-in-rangep "9966-7000"   "9966-7499" ))
     ((isbn-in-rangep "9966-750"    "9966-959"  ))
     ((isbn-in-rangep "9966-9600"   "9966-9999" ))
     ((isbn-in-rangep "9967-00"     "9967-39"   ))
     ((isbn-in-rangep "9967-400"    "9967-899"  ))
     ((isbn-in-rangep "9967-9000"   "9967-9999" ))
     ((isbn-in-rangep "9968-00"     "9968-49"   ))
     ((isbn-in-rangep "9968-500"    "9968-939"  ))
     ((isbn-in-rangep "9968-9400"   "9968-9999" ))
     ((isbn-in-rangep "9970-00"     "9970-39"   ))
     ((isbn-in-rangep "9970-400"    "9970-899"  ))
     ((isbn-in-rangep "9970-9000"   "9970-9999" ))
     ((isbn-in-rangep "9971-0"      "9971-5"    ))
     ((isbn-in-rangep "9971-60"     "9971-89"   ))
     ((isbn-in-rangep "9971-900"    "9971-989"  ))
     ((isbn-in-rangep "9971-9900"   "9971-9999" ))
     ((isbn-in-rangep "9972-00"     "9972-09"   ))
     ;; 9972 1 ??
     ((isbn-in-rangep "9972-200"    "9972-249"  ))
     ((isbn-in-rangep "9972-2500"   "9972-2999" ))
     ((isbn-in-rangep "9972-30"     "9972-59"   ))
     ((isbn-in-rangep "9972-600"    "9972-899"  ))
     ((isbn-in-rangep "9972-9000"   "9972-9999" ))
     ((isbn-in-rangep "9973-0"      "9973-0"    ))
     ((isbn-in-rangep "9973-10"     "9973-69"   ))
     ((isbn-in-rangep "9973-700"    "9973-969"  ))
     ((isbn-in-rangep "9973-9700"   "9973-9999" ))
     ((isbn-in-rangep "9974-0"      "9974-2"    ))
     ((isbn-in-rangep "9974-30"     "9974-54"   ))
     ((isbn-in-rangep "9974-550"    "9974-749"  ))
     ((isbn-in-rangep "9974-7500"   "9974-9499" ))
     ((isbn-in-rangep "9974-95"     "9974-99"   ))
     ((isbn-in-rangep "9975-0"      "9975-4"    ))
     ((isbn-in-rangep "9975-50"     "9975-89"   ))
     ((isbn-in-rangep "9975-900"    "9975-949"  ))
     ((isbn-in-rangep "9975-9500"   "9975-9999" ))
     ((isbn-in-rangep "9976-0"      "9976-5"    ))
     ((isbn-in-rangep "9976-60"     "9976-89"   ))
     ((isbn-in-rangep "9976-900"    "9976-989"  ))
     ((isbn-in-rangep "9976-9990"   "9976-9999" ))
     ((isbn-in-rangep "9977-00"     "9977-89"   ))
     ((isbn-in-rangep "9977-900"    "9977-989"  ))
     ((isbn-in-rangep "9977-9900"   "9977-9999" ))
     ((isbn-in-rangep "9978-00"     "9978-29"   ))
     ((isbn-in-rangep "9978-300"    "9978-399"  ))
     ((isbn-in-rangep "9978-40"     "9978-94"   ))
     ((isbn-in-rangep "9978-950"    "9978-989"  ))
     ((isbn-in-rangep "9978-9900"   "9978-9999" ))
     ((isbn-in-rangep "9979-0"      "9979-4"    ))
     ((isbn-in-rangep "9979-50"     "9979-75"   ))
     ((isbn-in-rangep "9979-760"    "9979-899"  ))
     ((isbn-in-rangep "9979-9000"   "9979-9999" ))
     ((isbn-in-rangep "9980-0"      "9980-3"    ))
     ((isbn-in-rangep "9980-40"     "9980-89"   ))
     ((isbn-in-rangep "9980-900"    "9980-989"  ))
     ((isbn-in-rangep "9980-9900"   "9980-9999" ))
     ((isbn-in-rangep "9981-00"     "9981-09"   ))
     ((isbn-in-rangep "9981-100"    "9981-159"  ))
     ((isbn-in-rangep "9981-1600"   "9981-1999" ))
     ((isbn-in-rangep "9981-20"     "9981-79"   ))
     ((isbn-in-rangep "9981-800"    "9981-949"  ))
     ((isbn-in-rangep "9981-9500"   "9981-9999" ))
     ((isbn-in-rangep "9982-00"     "9982-79"   ))
     ((isbn-in-rangep "9982-800"    "9982-889"  ))
     ((isbn-in-rangep "9982-9900"   "9982-9999" ))
     ((isbn-in-rangep "9983-80"     "9983-94"   ))
     ((isbn-in-rangep "9983-950"    "9983-989"  ))
     ((isbn-in-rangep "9983-9900"   "9983-9999" ))
     ((isbn-in-rangep "9984-00"     "9984-49"   ))
     ((isbn-in-rangep "9984-500"    "9984-899"  ))
     ((isbn-in-rangep "9984-9000"   "9984-9999" ))
     ((isbn-in-rangep "9985-0"      "9985-4"    ))
     ((isbn-in-rangep "9985-50"     "9985-79"   ))
     ((isbn-in-rangep "9985-800"    "9985-899"  ))
     ((isbn-in-rangep "9985-9000"   "9985-9999" ))
     ((isbn-in-rangep "9986-00"     "9986-39"   ))
     ((isbn-in-rangep "9986-400"    "9986-899"  ))
     ((isbn-in-rangep "9986-9000"   "9986-9399" ))
     ((isbn-in-rangep "9986-940"    "9986-969"  ))
     ((isbn-in-rangep "9986-97"     "9986-99"   ))
     ((isbn-in-rangep "9987-00"     "9987-39"   ))
     ((isbn-in-rangep "9987-400"    "9987-879"  ))
     ((isbn-in-rangep "9987-8800"   "9987-9999" ))
     ((isbn-in-rangep "9988-0"      "9988-2"    ))
     ((isbn-in-rangep "9988-30"     "9988-54"   ))
     ((isbn-in-rangep "9988-550"    "9988-749"  ))
     ((isbn-in-rangep "9988-7500"   "9988-9999" ))
     ((isbn-in-rangep "9989-0"      "9989-0"    ))
     ((isbn-in-rangep "9989-100"    "9989-199"  ))
     ((isbn-in-rangep "9989-2000"   "9989-2999" ))
     ((isbn-in-rangep "9989-30"     "9989-59"   ))
     ((isbn-in-rangep "9989-600"    "9989-949"  ))
     ((isbn-in-rangep "9989-9500"   "9989-9999" ))
     ;; ----------------------------------------------------------------
     ((isbn-in-rangep "99901-00"    "99901-49"  ))
     ((isbn-in-rangep "99901-500"   "99901-799" ))
     ((isbn-in-rangep "99901-80"    "99901-99"  ))
     ((isbn-in-rangep "99903-0"     "99903-1"   ))
     ((isbn-in-rangep "99903-20"    "99903-89"  ))
     ((isbn-in-rangep "99903-900"   "99903-999" ))
     ((isbn-in-rangep "99904-0"     "99904-5"   ))
     ((isbn-in-rangep "99904-60"    "99904-89"  ))
     ((isbn-in-rangep "99904-900"   "99904-999" ))
     ((isbn-in-rangep "99905-0"     "99905-3"   ))
     ((isbn-in-rangep "99905-40"    "99905-79"  ))
     ((isbn-in-rangep "99905-800"   "99905-999" ))
     ((isbn-in-rangep "99906-0"     "99906-2"   ))
     ((isbn-in-rangep "99906-30"    "99906-59"  ))
     ((isbn-in-rangep "99906-600"   "99906-999" ))
     ((isbn-in-rangep "99908-0"     "99908-0"   ))
     ((isbn-in-rangep "99908-10"    "99908-89"  ))
     ((isbn-in-rangep "99908-900"   "99908-999" ))
     ((isbn-in-rangep "99909-0"     "99909-3"   ))
     ((isbn-in-rangep "99909-40"    "99909-94"  ))
     ((isbn-in-rangep "99909-950"   "99909-999" ))
     ((isbn-in-rangep "99910-0"     "99910-2"   ))
     ((isbn-in-rangep "99910-30"    "99910-89"  ))
     ((isbn-in-rangep "99910-900"   "99910-999" ))
     ((isbn-in-rangep "99911-00"    "99911-59"  ))
     ((isbn-in-rangep "99911-600"   "99911-999" ))
     ((isbn-in-rangep "99912-0"     "99912-4"   ))
     ((isbn-in-rangep "99912-500"   "99912-599" ))
     ((isbn-in-rangep "99912-60"    "99912-89"  ))
     ((isbn-in-rangep "99912-900"   "99912-999" ))
     ((isbn-in-rangep "99913-0"     "99913-2"   ))
     ((isbn-in-rangep "99913-30"    "99913-35"  ))
     ((isbn-in-rangep "99913-600"   "99913-604" ))
     ((isbn-in-rangep "99914-0"     "99914-4"   ))
     ((isbn-in-rangep "99914-50"    "99914-89"  ))
     ((isbn-in-rangep "99914-900"   "99914-949" ))
     ((isbn-in-rangep "99915-0"     "99915-4"   ))
     ((isbn-in-rangep "99915-50"    "99915-79"  ))
     ((isbn-in-rangep "99915-800"   "99915-999" ))
     ((isbn-in-rangep "99916-0"     "99916-2"   ))
     ((isbn-in-rangep "99916-30"    "99916-69"  ))
     ((isbn-in-rangep "99916-700"   "99916-999" ))
     ((isbn-in-rangep "99917-0"     "99917-2"   ))
     ((isbn-in-rangep "99917-30"    "99917-89"  ))
     ((isbn-in-rangep "99917-900"   "99917-999" ))
     ((isbn-in-rangep "99918-0"     "99918-3"   ))
     ((isbn-in-rangep "99918-40"    "99918-89"  ))
     ((isbn-in-rangep "99918-900"   "99918-999" ))
     ((isbn-in-rangep "99919-0"     "99919-2"   ))
     ((isbn-in-rangep "99919-40"    "99919-69"  ))
     ((isbn-in-rangep "99919-900"   "99919-999" ))
     ((isbn-in-rangep "99920-0"     "99920-4"   ))
     ((isbn-in-rangep "99920-50"    "99920-89"  ))
     ((isbn-in-rangep "99920-900"   "99920-999" ))
     ((isbn-in-rangep "99921-0"     "99921-1"   ))
     ((isbn-in-rangep "99921-20"    "99921-69"  ))
     ((isbn-in-rangep "99921-700"   "99921-799" ))
     ((isbn-in-rangep "99921-8"     "99921-8"   ))
     ((isbn-in-rangep "99921-90"    "99921-99"  ))
     ((isbn-in-rangep "99922-0"     "99922-3"   ))
     ((isbn-in-rangep "99922-40"    "99922-69"  ))
     ((isbn-in-rangep "99922-700"   "99922-999" ))
     ((isbn-in-rangep "99923-0"     "99923-1"   ))
     ((isbn-in-rangep "99923-20"    "99923-79"  ))
     ((isbn-in-rangep "99923-800"   "99923-999" ))
     ((isbn-in-rangep "99924-0"     "99924-2"   ))
     ((isbn-in-rangep "99924-30"    "99924-79"  ))
     ((isbn-in-rangep "99924-800"   "99924-900" ))
     ((isbn-in-rangep "99925-0"     "99925-3"   ))
     ((isbn-in-rangep "99925-40"    "99925-79"  ))
     ((isbn-in-rangep "99925-800"   "99925-999" ))
     ((isbn-in-rangep "99926-0"     "99926-0"   ))
     ((isbn-in-rangep "99926-10"    "99926-59"  ))
     ((isbn-in-rangep "99926-600"   "99926-999" ))
     ((isbn-in-rangep "99927-0"     "99927-2"   ))
     ((isbn-in-rangep "99927-30"    "99927-59"  ))
     ((isbn-in-rangep "99927-600"   "99927-999" ))
     ((isbn-in-rangep "99928-0"     "99928-0"   ))
     ((isbn-in-rangep "99928-10"    "99928-79"  ))
     ((isbn-in-rangep "99928-800"   "99928-999" ))
     ;; 99929 ?? ??
     ((isbn-in-rangep "99930-0"     "99930-4"   ))
     ((isbn-in-rangep "99930-50"    "99930-79"  ))
     ((isbn-in-rangep "99930-800"   "99930-999" ))
     ((isbn-in-rangep "99931-0"     "99931-4"   ))
     ((isbn-in-rangep "99931-50"    "99931-79"  ))
     ((isbn-in-rangep "99931-800"   "99931-999" ))
     ((isbn-in-rangep "99932-0"     "99932-0"   ))
     ((isbn-in-rangep "99932-10"    "99932-59"  ))
     ((isbn-in-rangep "99932-600"   "99932-699" ))
     ((isbn-in-rangep "99932-7"     "99932-7"   ))
     ((isbn-in-rangep "99932-80"    "99932-99"  ))
     ((isbn-in-rangep "99933-0"     "99933-2"   ))
     ((isbn-in-rangep "99933-30"    "99933-59"  ))
     ((isbn-in-rangep "99933-600"   "99933-999" ))
     ((isbn-in-rangep "99934-0"     "99934-1"   ))
     ((isbn-in-rangep "99934-20"    "99934-79"  ))
     ((isbn-in-rangep "99934-800"   "99934-999" ))
     ((isbn-in-rangep "99935-0"     "99935-2"   ))
     ((isbn-in-rangep "99935-30"    "99935-59"  ))
     ((isbn-in-rangep "99935-600"   "99935-799" ))
     ;; 99935 8 ??
     ((isbn-in-rangep "99935-90"    "99935-99"  ))
     ((isbn-in-rangep "99936-0"     "99936-0"   ))
     ((isbn-in-rangep "99936-10"    "99936-59"  ))
     ((isbn-in-rangep "99936-600"   "99936-999" ))
     ((isbn-in-rangep "99937-0"     "99937-1"   ))
     ((isbn-in-rangep "99937-20"    "99937-59"  ))
     ((isbn-in-rangep "99937-600"   "99937-999" ))
     ((isbn-in-rangep "99938-0"     "99938-2"   ))
     ((isbn-in-rangep "99938-30"    "99938-59"  ))
     ((isbn-in-rangep "99938-600"   "99938-999" ))
     ((isbn-in-rangep "99939-0"     "99939-5"   ))
     ((isbn-in-rangep "99939-60"    "99939-89"  ))
     ((isbn-in-rangep "99939-900"   "99939-999" ))
     ((isbn-in-rangep "99940-0"     "99940-0"   ))
     ((isbn-in-rangep "99940-10"    "99940-69"  ))
     ((isbn-in-rangep "99940-700"   "99940-999" ))
     ((isbn-in-rangep "99941-0"     "99941-2"   ))
     ((isbn-in-rangep "99941-30"    "99941-89"  ))
     ((isbn-in-rangep "99941-900"   "99941-999" ))
     ((isbn-in-rangep "99942-0"     "99942-4"   ))
     ((isbn-in-rangep "99942-50"    "99942-79"  ))
     ((isbn-in-rangep "99942-800"   "99942-999" ))
     ((isbn-in-rangep "99943-0"     "99943-2"   ))
     ((isbn-in-rangep "99943-30"    "99943-59"  ))
     ((isbn-in-rangep "99943-600"   "99943-999" ))
     ((isbn-in-rangep "99944-0"     "99944-4"   ))
     ((isbn-in-rangep "99944-50"    "99944-79"  ))
     ((isbn-in-rangep "99944-800"   "99944-999" ))
     ((isbn-in-rangep "99946-0"     "99946-2"   ))
     ((isbn-in-rangep "99946-30"    "99946-59"  ))
     ((isbn-in-rangep "99946-600"   "99946-999" ))

     (t
      (error "Cannot hyphenate ISBN %s"
	     (buffer-substring (point) (+ 10 (point))))))))


(defun isbn-digit (c)           ;private internal function
  "Return the value of the ISBN digit c (in 0..10), or nil if c is invalid."
  (cond
   ((and (>= c ?0) (<= c ?9)) (- c ?0))
   ((or (= c ?X) (= c ?x)) 10)))

(fset 'issn-digit 'isbn-digit)          ;ISSN uses subset of ISBN digits


(defun isbn-numberp (isbn)            ;private internal function
  "Given an ISBN number as a string argument, return t if it
is valid, according to the formula
        (sum(k=1:9) digit(k) * k) mod 11 == digit(10)
where X as a digit has value 10, and hyphens are ignored, and
nil otherwise."
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
    result))


(defun issn-numberp (issn)            ;private internal function
  "Given an ISSN number as a string argument, return t if it
is valid, according to the formula
    (sum(k=1:7) digit(k) * (k+2)) mod 11 == digit(8)
where X as a digit has value 10, and hyphens are ignored, and
nil otherwise."
  (let ((sum 0) (k 0) (c 0) (pos 1) (result nil))
    (while (< k (length issn))
      (setq c (issn-digit (aref issn k)))
      (setq k (1+ k))
      (cond
       ((and c (= pos 8))
        (setq result (= (mod sum 11) c) pos (1+ pos)))
       ((and c (< pos 8))
        (setq sum (+ sum (* c (+ pos 2))) pos (1+ pos)))
       ))
    result))


(defun isbn-in-rangep (start-prefix end-prefix)
  "Check whether the 10-digit ISBN starting at point lies within
the range of START-PREFIX and END-PREFIX (inclusive), where the
prefixes consist of a hyphen-separated countrygroupnumber and
publishernumber, and if so, insert hyphens in the buffer at the points
indicated by the hyphen in start-prefix, and advance past the end of
the ISBN before returning.  If there is no match, point is left
unchanged.  Return T on success, and NIL on failure."
  (let ((reduced-start-prefix (isbn-squeeze-hyphens start-prefix))
	(reduced-end-prefix (isbn-squeeze-hyphens end-prefix)))
    (if (and
	 (string-less-or-equalp
	  reduced-start-prefix
	  (buffer-substring (point) (+ (length reduced-start-prefix) (point))))
	 (string-less-or-equalp
	  (buffer-substring (point) (+ (length reduced-end-prefix) (point)))
	  reduced-end-prefix))
	(and
	 (mapcar 'isbn-insert-hyphen
		 (make-isbn-digit-group-sizes start-prefix))
	 (if (looking-at "x")
	     (progn			;convert final x to X in ISBN
	       (delete-char 1)
	       (insert-char ?\X 1))
	   (forward-char 1)
	   t))
      nil)))


(defun isbn-insert-hyphen (count)
  "Move forward COUNT characters and insert a hyphen."
  (forward-char count)
  (insert "-"))


(defun isbn-squeeze-hyphens (isbn)
  "Return a copy of ISBN with hyphens removed."
  (let ((new-isbn ""))
    (mapcar '(lambda (s)
	       (setq new-isbn
		     (concat new-isbn (if (= s ?-) ""
					(char-to-string s))))) isbn)
    new-isbn))


(defun make-isbn-digit-group-sizes (isbn)
  "Return a 3-element vector of digit-group-sizes for the ISBN prefix,
countrygroupnumber-publishernumber.  These define the size of the four
parts of an ISBN."
  (let ((digit-group-sizes (make-vector 3 1))
	(k 0)
	(m 0)
	(n (length isbn)))
    (while (< k n)
      (if (= ?- (aref isbn k))
	  (progn
	    (aset digit-group-sizes 0 m)
	    (setq m 0))
	(setq m (1+ m)))
      (setq k (1+ k)))
    (aset digit-group-sizes 1 m)
    (aset digit-group-sizes 2
	  (- 9 (aref digit-group-sizes 0) (aref digit-group-sizes 1)))
    digit-group-sizes))


(defun string-less-or-equalp (s1 s2)
  "Return t if S1 is lexicographically less than or equal to S2, and nil
otherwise."
  (or (string-equal s1 s2)
      (string-lessp s1 s2)))

;;; isbn.el ends here
