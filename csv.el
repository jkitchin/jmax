;;; csv.el --- Functions for reading and parsing CSV files.

;; Copyright (C) 2001-2006 by Ulf Jasper

;; Author:     Ulf Jasper <ulf.jasper@web.de>
;; Filename:   csv.el
;; Created:    August 19 2001
;; Keywords:   util
;; CVS:        $Id: csv.el,v 1.6 2006-11-28 16:52:48 ulf Exp $
;; Time-stamp: "28. November 2006, 17:52:15 (ulf)"

(defconst csv-version 2.0 "Version number of csv.el.")

;; ======================================================================

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;; ======================================================================

;;; Commentary:

;; csv.el provides functions for reading and parsing CSV (Comma Separated
;; Value) files.  It follows the format as defined in RFC 4180 "Common
;; Format and MIME Type for CSV Files" (http://tools.ietf.org/html/rfc4180).

;; Main routine is `csv-parse-buffer' which takes a buffer containing a
;; CSV file and converts its contents into list form.  The first line of
;; the CSV file can be interpreted as a list of keys.  In this case

;; Key1,Key 2,"Key3"
;; Value1a,Value1b,"Value1c"
;; Value2a,Value2b,"Very, very long Value
;; 2c"

;; gets translated into a list of alists:

;; ((("Key1" . "Value1a") ("Key 2" . "Value1b") ("Key3" . "Value1c"))
;;  (("Key1" . "Value2a") ("Key 2" . "Value2b") ("Key3" . "Very, very long Value\n2c")))

;; If the first line of the CSV file shall NOT be interpreted as a list of
;; key names the result is a list of lists:

;; (("Key1" "Key 2" "Key3")
;;  ("Value1a" "Value1b" "Value1c")
;;  ("Value2a" "Value2b" "Very, very long Value\n2c"))

;; The function `csv-insert-contents' demonstrates how to use
;; `csv-parse-buffer'.

;; ======================================================================

;;; History:

;; 2.0 (2006-11-28)
;;     Follow CSV definition of RFC 4180.
;;     - Allow for empty first field in a line.
;;     - Allow for missing newline in last line.
;;     - csv-(un)quoted-entry-regexp now defines actual entry only
;;       (without leading and trailing separators)
;;     - Changed csv-parse-buffer argument list!
;;     Thanks to Adrian Aichner.

;; 1.2 (2004-11-06)
;;     Introduced csv-(un)?quoted-entry-regexp, as suggested by John
;;     Sturdy.  Tested on Emacs 21.3.50.

;; 1.1 (2001-08-30)
;;     First.first version.
;;     automatically add missing entries for lines that are too short.
;;     Tested on Emacs 20.7.1 and XEmacs 21.1.12.

;; 1.0 (2001-08-20)
;;     First version

;; ======================================================================

;;; Code:

(defvar csv-quoted-entry-regexp
  "\"\\(\\([^\"]\\|\n\\|\"\"\\)*\\)\""
  "Regexp defining a quoted entry in a CSV file.

Modifying this expression allows for automatically modifying CSV
entries while they are being read.  The actual CSV entry is read
from the matching part.  The following expression for example
strips off leading and trailing whitespaces:

[\\t ]*\"\\(\\([^\"]\\|\\n\\|\"\"\\)*\\)\"[\\t ]*")

(defvar csv-unquoted-entry-regexp
  "\\([^,\n]*\\)"
  "Regexp defining an unquoted entry in a CSV file.

Modifying this expression allows for automatically modifying CSV
entries while they are being read.  The actual CSV entry is read
from the matching part.  The following expression for example
strips off leading and trailing whitespaces:

[\\t ]*\\(\[^,\\n\]*\\)\[\\t \]*")

(defun csv-parse-buffer (first-line-contains-keys
                         &optional buffer coding-system)
  "Parse a buffer containing CSV data, return data as a list of alists.

The first line in the buffer is interpreteted as a header line
if FIRST-LINE-CONTAINS-KEYS is non-nil.
If BUFFER is non-nil it gives the buffer to be parsed.  If it is
nil the current buffer is parsed.
CODING-SYSTEM gives the coding-system for reading the buffer."
  (interactive)
  (let ((result nil)
	(keylist nil)
	(go-ahead t)
	(coding-system-for-read coding-system))
    (save-window-excursion
      (if buffer
	  (switch-to-buffer buffer)
	(setq buffer (current-buffer)))
      (goto-char (point-min))
      ;; get the header line
      (when first-line-contains-keys
        (setq keylist (csv--read-line))
        (forward-line 1))
      ;; get all the content lines
      (while go-ahead
	(setq result (cons (csv--read-line keylist) result))
	(forward-line 1)
	(if (eobp)
	    (setq go-ahead nil))))
    (setq result (reverse result))
    result))

(defun csv--read-line (&optional keylist)
  "Parse a single CSV line.
If KEYLIST is not nil an alist is returned, using the keys from the keylist.
Otherwise just the list of entries is returned."
  (let ((line-contents nil)
	(match1 "")
	(match2 "")
	(match "")
	(matchstart-q 0)
	(matchstart-u 0)
	(matchend-q 0)
	(matchend-u 0)
	(index 0)
	(go-ahead t)
        (q-regexp-0 (concat "^" csv-quoted-entry-regexp "\\(,\\|,?$\\)"))
        (q-regexp-1 (concat "," csv-quoted-entry-regexp "\\(,\\|,?$\\)"))
        (u-regexp-0 (concat "^" csv-unquoted-entry-regexp "\\(,\\|,?$\\)"))
        (u-regexp-1 (concat "," csv-unquoted-entry-regexp "\\(,\\|,?$\\)")))
    (beginning-of-line)
    (setq line-contents nil)
    (while go-ahead
      (setq matchstart-q nil
	    matchstart-u nil)
      ;; try for quoted entry
      (save-excursion
	(when (re-search-forward (if (> index 0) q-regexp-1 q-regexp-0) nil t)
	  (setq matchstart-q (match-beginning 0))
	  (setq matchend-q (+ 1 (match-end 1)))
	  (setq match1 (match-string 1))))
      ;; try unquoted
      (save-excursion
	(when (re-search-forward (if (> index 0) u-regexp-1 u-regexp-0) nil t)
	  (setq matchstart-u (match-beginning 0))
	  (setq matchend-u (match-end 1))
	  (setq match2 (match-string 1))))
      ;; check whether quoted or unquoted fits better
      (setq match nil)
      (if matchstart-q
	  (if matchstart-u
	      (if (<= matchstart-q matchstart-u)
		  (progn
		    (setq match match1)
		    (goto-char matchend-q))
		(setq match match2)
		(goto-char matchend-u))
	    (setq match match1)
	    (goto-char matchend-q))
	(when matchstart-u
	  (setq match match2)
	  (goto-char matchend-u)))
      ;; check whether we found something
      (if (not match)
	  (setq go-ahead nil)
	(if (not keylist)
	    (setq line-contents (cons match line-contents))
	  (let ((key (nth index keylist)))
	    (setq line-contents (cons (cons key match) line-contents))))
	)
      (setq index (+ 1 index))
      (if (eolp) (setq go-ahead nil)))
    ;; fill up
    (while (< index (length keylist))
      (let ((key (nth index keylist)))
	(setq line-contents (cons (cons key "") line-contents)))
      (setq index (+ 1 index)))
    ;; finally reverse result -- for readability
    (reverse line-contents)))

(defun csv-insert-contents (contentlist)
  "Insert the contents of a CSV file -- sample for using `csv-parse-buffer'.
CONTENTLIST gives a list of alists as returned by `csv-parse-buffer'."
  (interactive)
  (mapc (lambda (line)
          (mapc (lambda (i)
                  (if (listp i)
                      (insert (format "\"%s\" = \"%s\"\n" (car i) (cdr i)))
                    (insert (format "\"%s\"\n" i))))
                  line)
          (insert "-----\n"))
        contentlist))

(defun csv-demo (first-line-contains-keys)
  "CSV demo routine."
  (interactive "P")
  (let* ((b (current-buffer))
	 (tb (get-buffer-create "*csv*")))
    (switch-to-buffer-other-window tb)
    (erase-buffer)
    (goto-char (point-min))
    (csv-insert-contents (csv-parse-buffer first-line-contains-keys b))
    (switch-to-buffer tb)))

(defun csv-run-testsuite ()
  "Run a suite of CSV parser tests."
  (interactive)
  (message "Running CSV parser tests...")
  (csv-run-test nil
                "a,b,c"
                '(("a" "b" "c")))
  (csv-run-test nil
                "a,b,c\n,e,f"
                '(("a" "b" "c") ("" "e" "f")))
  (csv-run-test t
                "A,B,C\na,b,c"
                '((("A" . "a") ("B" . "b") ("C" . "c"))))
  (csv-run-test t
                "A,C,A\na,b,c"
                '((("A" . "a") ("C" . "b") ("A" . "c"))))

  ;; from the doc
  (csv-run-test t
                "Key1,Key 2,\"Key3\"\nValue1a,Value1b,\"Value1c\"\nValue2a,Value2b,\"Very, very long Value\n2c\""
                '((("Key1" . "Value1a") ("Key 2" . "Value1b") ("Key3" . "Value1c"))
                  (("Key1" . "Value2a") ("Key 2" . "Value2b") ("Key3" . "Very, very long Value\n2c"))))

  (csv-run-test nil
                "Key1,Key 2,\"Key3\"\nValue1a,Value1b,\"Value1c\"\nValue2a,Value2b,\"Very, very long Value\n2c\""
                '(("Key1" "Key 2" "Key3")
                  ("Value1a" "Value1b" "Value1c")
                  ("Value2a" "Value2b" "Very, very long Value\n2c")))

  ;; Examples from RFC 4180
  ;; 2.1
  (csv-run-test nil
                "aaa,bbb,ccc\nzzz,yyy,xxx\n"
                '(("aaa" "bbb" "ccc") ("zzz" "yyy" "xxx")))
  ;; 2.2
  (csv-run-test nil
                "aaa,bbb,ccc\nzzz,yyy,xxx"
                '(("aaa" "bbb" "ccc") ("zzz" "yyy" "xxx")))
  ;; 2.3
  (csv-run-test t
                "field_name,field_name,field_name\naaa,bbb,ccc\nzzz,yyy,xxx"
                '((("field_name" . "aaa") ("field_name" . "bbb") ("field_name" . "ccc"))
                  (("field_name" . "zzz") ("field_name" . "yyy") ("field_name" . "xxx"))))
  (csv-run-test t
                "field_name1,field_name2,field_name3\naaa,bbb,ccc\nzzz,yyy,xxx"
                '((("field_name1" . "aaa") ("field_name2" . "bbb") ("field_name3" . "ccc"))
                  (("field_name1" . "zzz") ("field_name2" . "yyy") ("field_name3" . "xxx"))))
  ;; 2.4
  (csv-run-test nil
                "aaa,bbb,ccc"
                '(("aaa" "bbb" "ccc")))
  ;; 2.5
  (csv-run-test nil
                "\"aaa\",\"bbb\",\"ccc\"\nzzz,yyy,xxx\n"
                '(("aaa" "bbb" "ccc") ("zzz" "yyy" "xxx")))
  ;; 2.6
  (csv-run-test nil
                "\"aaa\",\"b\nbb\",\"ccc\"\nzzz,yyy,xxx\n"
                '(("aaa" "b\nbb" "ccc") ("zzz" "yyy" "xxx")))
  ;; 2.7
  (csv-run-test nil
                "\"aaa\",\"b\"\"bb\",\"ccc\"\n"
                '(("aaa" "b\"\"bb" "ccc")))
  (message "Running CSV parser tests... OK"))

(defun csv-run-test (first-line-contains-keys csv-string expected-result)
  "Run a single CSV parser test.
Argument FIRST-LINE-CONTAINS-KEYS argument for csv parser.
Argument CSV-STRING the CSV input string.
Argument EXPECTED-RESULT the expected resulting list."
  (let ((b (get-buffer-create " *csv test*"))
        (result nil))
    (set-buffer b)
    (erase-buffer)
    (insert csv-string)
    (goto-char (point-min))
    (setq result (csv-parse-buffer first-line-contains-keys b))
    (unless (equal expected-result result)
      (error "Error while parsing csv string %s:\nFound: %s\nExpected: %s"
             csv-string result expected-result))))

(provide 'csv)
;;; csv.el ends here
