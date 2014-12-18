;;; -*-emacs-lisp-*-
;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.54",
;;;     date            = "10 December 2011",
;;;     time            = "16:04:02 MDT",
;;;     filename        = "clsc.el",
;;;     address         = "Department of Mathematics
;;;                        University of Utah
;;;                        Salt Lake City, UT 84112
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 581 4148",
;;;     checksum        = "55703 1240 5684 47693",
;;;     email           = "beebe@math.utah.edu (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "emacs, lisp",
;;;     license         = "public domain",
;;;     supported       = "yes",
;;;     docstring       = "This file is a collection of miscellaneous
;;;                        GNU Emacs functions.  It began as a
;;;                        translation of a number of TOPS-20 Emacs
;;;                        functions which were written in TECO, but has
;;;                        since been augmented by many more functions
;;;                        that did not exist in TOPS-20 Emacs.  Think
;;;                        of this file as a grab-bag of useful stuff.
;;;
;;;                        The TECO code, clsc.emacs, was developed from
;;;                        4-May-1981 to 22-Jun-1990, and the
;;;                        translation to Emacs Lisp began on 2-Sep-1986
;;;                        under VAX VMS.  The peculiar name, clsc, is
;;;                        an abbreviation of "College of Science
;;;                        Computer", which was the name of our
;;;                        institution from its founding in 1978 to
;;;                        1986, when it became the "Center for
;;;                        Scientific Computing".
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

;;; clsc.el --- miscellaneous handy GNU Emacs editing support

;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Created: 02-Sep-1986
;; Version: 1.00
;; Keywords:

;; Copyright (C) 1986--1998 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;***********************************************************************
; This file contains functions developed at the University of Utah
; College of Science.  Many have been translated from the CLSC.EMACS
; library developed for TOPS-20 EMACS by Nelson H. F. Beebe
;
; Please keep these functions in alphabetical order, ground (C-M-q), and
; separated by TWO blank lines (byte-compile-file terminates at CTL-L, so
; page marks (and therefore sort-pages) cannot be used, sigh...)
;***********************************************************************

;;; Change log:

;; [1.54] Sat Dec 10 16:03:16 2011
;; Suppress load of clsc19 and clsc20 libraries for Emacs versions 20,
;; 21, ...
;;
;; [1.53] Sun May 27 09:58:28 2001
;; Update several functions to use new private functions
;; clsc-replace-regexp and clsc-replace-string to avoid spurious
;; progress messages.
;;
;; [1.52] Tue Apr 21 09:22:55 1998
;; Update date-edit to use mail-host-address when available, and
;; add a call to (provide ...).
;;
;; [1.51] Wed Feb 25 16:05:18 1998
;; Update to support new auxiliary libraries clsc19.el and clsc20.el.
;; They contain functions related to occur mode (after, before,
;; show-long-lines) that were previously in this file.  The radically
;; different implementation of occur in emacs 20 precipitated this
;; split.
;;
;; [1.50] Sat Mar  2 10:22:59 1996
;; Add end-of-buffer check in main search loops in functions after and
;; before, to eliminate an infinite-loop condition when search for an
;; empty pattern (i.e. a paragraph break).  These functions are more
;; than 10 years old, which shows how long bugs can remain hidden before
;; biting you!
;;
;; [1.49] Mon Jan 15 15:14:08 1996
;; Add code to strip NULs in fix-typescript.
;;
;; /u/sy/beebe/emacs/clsc.el, Tue Sep 14 09:09:37 1993
;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;; [1.48] Add new variables occur-highlight-time, occur-match-beginning,
;; and occur-match-end.  Replace functions after, before, and
;; show-long-lines with new versions that support occur-mode
;; positioning.  Replace occur mode's occur-mode-goto-occurrence with
;; private version, and add new functions
;; occur-mode-extended-goto-occurrence and occur-mode-mouse-goto to
;; support quick mouse-click gotos.  Replace next-doubled-word by
;; version that marks the word by highlighting with emacs version 19 or
;; later.
;;
;; /u/sy/beebe/emacs/clsc.el, Mon Sep  6 18:28:51 1993
;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;; [1.47] Fix error in show-one-mouse-binding: princ was erroneously
;; changed to message by edit 1.46.
;;
;; /u/sy/beebe/emacs/clsc.el, Mon Jun 14 10:38:10 1993
;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;; [1.46] Update for Emacs 19.  Revise comment structure to match the
;; standards described in info node elisp -> tips -> library headers.
;; Reformat functions according to guidelines in info node elisp ->
;; tips -> style tips.  Revise list-library to include library name
;; in buffer name so that multiple lists can be visible at once.
;; Change message+beep to error function call.  Fix end-of-buffer
;; error in next-doubled-word and get-next-word.  Modify
;; down-comment-line and up-comment-line to strip empty comments that
;; they pass over.
;;
;; /u/sy/beebe/emacs/clsc.el, Sun Feb 21 10:59:04 1993
;; Edit by Nelson H. F. Beebe <beebe@sunrise>
;; [1.45] Extend fix-typescript to remove X window title sequences
;;
;; /u/sy/beebe/emacs/clsc.el, Fri Nov 20 12:00:55 1992
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; [1.44] Correct typo in make-word-list documentation.
;;
;; /u/sy/beebe/emacs/clsc.el, Fri Nov 13 05:19:43 1992
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; [1.43] Rename show-registers output buffer to *Registers*
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Nov 11 09:55:41 1992
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; [1.42] Add show-registers command.
;;
;; /u/sy/beebe/emacs/clsc.el, Sat Jul 25 08:57:07 1992
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; [1.41] Suppress "Mark set" message from one push-mark call.
;;
;; /u/sy/beebe/emacs/clsc.el, Mon Jun 29 19:20:54 1992
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; [1.40] Add internal-cts-* functions to simplify function today, and
;;	 extend today to optionally generate an ISO-style date.
;;
;; /u/sy/beebe/emacs/clsc.el, Tue May 26 11:04:14 1992
;; Edit by Nelson H. F. Beebe <beebe@sandy.math.utah.edu>
;; [1.39] Add (let...) bindings for free variables discovered by new
;;	 byte-compile-file.  However, mouse-button-names is intentionally
;;	 left as a free variable shared between initialize-mouse-bindings
;;	 and show-mouse-bindings.
;;	 Fix error in list-library that arose because of changed
;;	 M-x occur output.  Update list-library to output separator
;;	 lines between function descriptions, and to sort the output
;;	 by function name.
;;
;; /home/csc-sun/a/sy/beebe/emacs/clsc.el, Wed Dec 18 18:22:41 1991
;; Edit by Nelson H. F. Beebe <beebe@jeeves.math.utah.edu>
;; [1.38] Revise mouse bindings code to initialize mouse-button-names
;;        in a function rather than a defconst, so this library can be
;;        preloaded in a dumped emacs.
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Dec 18 10:47:58 1991
;; Edit by Nelson H. F. Beebe <beebe@jeeves.math.utah.edu>
;; [1.37] add show-mouse-bindings and its support code
;;
;; /u/sy/beebe/emacs/clsc.el, Thu Oct 31 10:59:46 1991
;; Edit by Nelson H. F. Beebe <beebe@jeeves.math.utah.edu>
;; [1.36] add sort-tokens and sort-tokens-regexp (adapted from
;; sftran3-sort-tokens in sftran3.el)
;;
;; /u/sy/beebe/emacs/clsc.el, Fri Oct 25 13:54:46 1991
;; Edit by Nelson H. F. Beebe <beebe@jeeves.math.utah.edu>
;; [1.35] Add check for existence of x-store-cut-buffer so code works
;;        under emacs with no X support, and under epoch.
;;        Add clsc-version variable.
;;
;; /u/beebe/emacs/clsc.el, Tue Aug 27 19:06:21 1991
;; Edit by Nelson H. F. Beebe <beebe@rios.math.utah.edu>
;; [1.34] Add now function
;;
;; /u/sy/beebe/emacs/clsc.el, Tue Jan 22 13:05:02 1991
;; Edit by Nelson H.F. Beebe <beebe@plot79>
;; [1.33] Add today function
;;
;; /u/sy/beebe/emacs/clsc.el, Sat Nov 24 10:28:50 1990
;; Edit by Nelson H.F. Beebe <beebe@magna.math.utah.edu>
;; [1.32] Modify date-edit to recognize signatures of UNIX shell scripts
;;        and PostScript
;;
;; /u/sy/beebe/emacs/clsc.el, Thu Jul 26 08:50:00 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.31] Modify date-edit to insert " at " instead of "@" in bibtex-mode
;;
;; /u/sy/beebe/emacs/clsc.el, Tue Jul 17 14:50:24 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.30] Fix author to allow suppression of comment delimiters.
;;
;; /u/sy/beebe/emacs/clsc.el, Sat Jul 14 12:40:24 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.29] Add *cut-to-x* and copy-region-as-kill for better X window
;;        support
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Jun 27 10:31:19 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.28] Add quote-region
;;
;; /u/sy/beebe/emacs/clsc.el, Thu Jun 14 16:58:23 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.27] Make fix-typescript strip trailing white space
;;
;; /u/sy/beebe/emacs/clsc.el, Mon Apr 30 18:50:49 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.26] Make fix-typescript strip bare assorted control characters
;;
;; /u/sy/beebe/emacs/clsc.el, Thu Mar  8 08:28:09 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.25] Make date-edit check for Emacs mode line, and insert after
;;        that line, if it exists
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Jan 24 22:25:04 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.24] Add leading-untabify to complement leading-tabify
;;
;; /u/sy/beebe/emacs/clsc.el, Sat Jan  6 10:37:48 1990
;; Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
;; [1.23] Change date and redate to handle 4-digit years.  The next
;;        century is only a decade away.  Generalize redate to support
;;        different delimiters, and give date optional arguments to set
;;        the delimiters.
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Sep 20 14:39:06 1989
;; Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
;; [1.22] Add fix-typescript
;;
;; /u/sy/beebe/emacs/clsc.el, Wed May  3 09:31:17 1989
;; Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
;; [1.21] Add author
;;
;; /u/sy/beebe/emacs/clsc.el, Sat Apr 29 21:27:55 1989
;; Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
;; [1.20] Add redate
;;
;; /u/sy/beebe/emacs/clsc.el, Fri Apr 28 08:19:57 1989
;; Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
;; [1.19] Add down-comment-line and up-comment-line (borrowed from
;;        TOPS-20 Emacs)
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Apr 12 18:04:14 1989
;; Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
;; [1.18]
;;
;; /u/sy/beebe/emacs/clsc.el, Wed Mar 15 09:33:45 1989
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.17] Added unix and dos functions for line terminator conversions
;;        in support of editing on Sun 386i with both UNIX and DOS files
;;
;; /u/sy/beebe/emacs/clsc.el, Tue Mar 14 13:37:51 1989
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.16] Changed incorrect interactive "p" to correct interactive "P"
;;
;; /home/csc-sun/u/sy/beebe/emacs/clsc.el, Mon Jan  9 18:51:54 1989
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.16] Added leading-tabify
;;
;; /usr/csc-sun/u/sy/beebe/emacs/clsc.el, Fri Mar 18 18:59:45 1988
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.15] Added show-long-lines
;;
;; /usr/csc-sun/u/sy/beebe/emacs/clsc.el, Fri Mar 18 17:18:32 1988
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.14] Modified date-edit to output 2-line header to avoid excessively
;;        long line
;;
;; /usr/csc-sun/u/sy/beebe/emacs/clsc.el, Tue Feb  9 17:17:47 1988
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.13] Rewrote date function with cleaner more compact code
;;
;; /usr/csc-sun/u/sy/beebe/emacs/clsc.el, Fri Feb  5 11:41:58 1988
;; Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
;; [1.12] Rewrote show-columns to use momentary-string-display instead of
;;        putting output in a temporary buffer.
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Wed Sep 10 12:51:15 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.11]
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Wed Sep 10 12:48:41 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.10] Added list-library
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Mon Sep  8 14:20:25 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.09] Added trim-lines and trim-prefix
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Mon Sep  8 13:43:21 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.08] Added make-word-list and unique-lines (and helper function
;;        get-next-line)
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Mon Sep  8 12:55:04 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.07] Added next-doubled-word (and helper function get-next-word)
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Sat Sep  6 18:37:45 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.06] Added show-columns
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Sat Sep  6 17:40:15 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.05] Added after and before (added from occur in replace.el)
;;
;; SYS$LIBROOT:[EMACS.LISP]CLSC.EL, Thu Sep  4 10:32:45 1986
;; Edit by BEEBEN (at VAX-VMS)
;; [1.04] Added user and system names to date-edit
;;
;; SYS$LIBROOT:[DISTRIB_GNUEMACS.LISP]CLSC.EL Wed Sep  3 19:53:49 1986
;; [1.03] Added blank-trim-lines
;;
;; SYS$LIBROOT:[DISTRIB_GNUEMACS.LISP]CLSC.EL Wed Sep  3 15:33:12 1986
;; [1.02] Added libgen
;;
;; SYS$LIBROOT:[DISTRIB_GNUEMACS.LISP]CLSC.EL Wed Sep  3 14:46:49 1986
;; [1.01] Added check-line-length
;;
;; SYS$OPRROOT:[BEEBEN]DATE_EDIT.EL Tue Sep  2 12:42:38 1986
;; [1.00] Added date-edit, my very first Emacs-Lisp function!


;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file may be part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'clsc)

(defconst clsc-version "1.52 [21-Apr-1998]"
  "Version number of the clsc.el library, a collection of useful editing
functions dating back to TOPS-20 Emacs that began in 1981.")

; Standard bindings from TOPS-20 Emacs (normally unset in GNU Emacs)
(global-set-key "\en" 'down-comment-line)
(global-set-key "\ep" 'up-comment-line)


(defvar occur-highlight-time 2
  "*Number of seconds to highlight matches from *LaTeX Labels* selections when
occur-mode-extended-goto-occurrence (on \\[occur-mode-extended-goto-occurrence])
is invoked.  If you make it zero, highlighting is suppressed.  If you make it
large, highlighting will remain until you input something.")


(defvar quote-region-prefix ">> "
  "String to insert in front of lines quoted by quote-region.")
(global-set-key "\C-c>" 'quote-region)


;; [10-Dec-2011] old code suppressed:
;; (if (string-lessp (substring emacs-version 0 2) "20")
;;    (require 'clsc19)
;;  (require 'clsc20))

(cond
 ((string-lessp (substring emacs-version 0 2) "20")
  (require 'clsc19))
 ((string-equal (substring emacs-version 0 2) "20")
    (require 'clsc20))
 ;; The after-xxx and before-xxx functions have not yet been
 ;; adapted to work with the new implementations of the occur-xxx
 ;; family in the standard Emacs file replace.el for Emacs versions
 ;; after 20, and if loaded here, they break the occur function.
 ;; We therefore suppress their loading for versions 21, 23, 23, 24, ...
 (t nil)
)


(defun author (&optional comment-suppress-marks)
  "Insert an author credit according to the current comment syntax.
With an argument, the comment delimiters are suppressed.  If the
variable author-string is not defined, no action is taken."
  (interactive "P")
  (setq comment-suppress-marks (not comment-suppress-marks))
  (if (and (boundp 'author-string) (stringp author-string))
      (let ((bol 0)
            (eol 0)
            (c-start (if (and comment-suppress-marks (stringp comment-start))
                         comment-start ""))
            (c-end (if (and comment-suppress-marks (stringp comment-end))
                       comment-end "")))
        (insert c-start "Author:" c-end "\n")
        (while (< eol (length author-string))
          (if (string= (substring author-string eol (1+ eol)) "\n")
              (progn
                (insert c-start "\t"
                        (substring author-string bol eol) c-end "\n")
                (setq eol (1+ eol))
                (setq bol eol))
            (setq eol (1+ eol)))))))


(defun blank-trim-lines ()
  "Trim trailing whitespace (blanks and tabs) from all lines in the buffer."
  (interactive)
  (let ((old-max (point-max)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1)))
    (if (null executing-macro)
        (if (= old-max (point-max))
            (message "[no changes]")
          (message (format "%d blanks trimmed" (- old-max (point-max))))))))


(defun check-line-length (&optional arg)
  "Find the next line longer than arg (default = 72) characters.
Leave point at end-of-line.  If no more long lines are found, point is
restored to its value at entry."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 72))
  (let ((start (point)))
    (if (eolp)
	(forward-line 1))
    (beginning-of-line)
    (while (< (point) (point-max))
      (end-of-line)
      (if (> (current-column) arg)
	  (error (format "Line length = %d" (current-column))))
      (forward-line 1))
    (message "[done]")
    (goto-char start)))


;;; From:         Per Mildner
;;;               <MIZAR.DOCS.UU.SE%perm%UCBVAX.BERKELEY.EDU@VM.TCS.Tulane.EDU>
;;; Subject:      Kill & Yank between emacs'es under X
;;; To:           Unix-emacs Redistribution <info-emacs@SCIENCE.UTAH.EDU>
;;; Date:         Fri, 17 Nov 89 06:46:15 GMT
;;; added *cut-to-x* and copy-region-as-kill

(if (string-equal (substring emacs-version 0 2) "18")
    (defvar *cut-to-x* t
      "*If non-Nil cutting will go to X cut buffer too."))

(if (string-equal (substring emacs-version 0 2) "18")
    (defun copy-region-as-kill (beg end)
      "Save the region as if killed, but don't kill it.  If the variable
*cut-to-x* is non-Nil, it will move the region to the X cut buffer too."
      (interactive "r")
      (if (eq last-command 'kill-region)
	  (kill-append (buffer-substring beg end) (< end beg))
	(setq kill-ring (cons (buffer-substring beg end) kill-ring))
	(if (> (length kill-ring) kill-ring-max)
	    (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
      ;;[25-Oct-1991] check that x-store-cut-buffer is known -- emacs 18.xx
      ;; has it when compiled with X window support, but not otherwise, and
      ;; epoch (prototype for Emacs 19) does not have it at all.  epoch
      ;; already handles this correctly, so the top of the kill ring is
      ;; copied to the X11 cut buffer.
      (if (and *cut-to-x* (eq window-system 'x) (fboundp 'x-store-cut-buffer))
	  (x-store-cut-buffer (car kill-ring)))
      (setq this-command 'kill-region)
      (setq kill-ring-yank-pointer kill-ring)))


(defun clsc-replace-regexp (regexp to-string)
  "Work much like replace-regexp, except be silent about it."
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))


(defun clsc-replace-string (from-string to-string)
  "Work much like replace-string, except be silent about it."
  (while (re-search-forward from-string nil t)
    (replace-match to-string nil t)))


(defun date (&optional open-char close-char)
  "Insert current date at point in format [18-Aug-1990], leaving point after
the insertion.  The optional arguments can specify alternatives to the
open and close bracket delimiters."
  (interactive)
  (let ((cts (current-time-string)))	;get "Tue Feb  9 17:06:20 1988"
                                        ;     0123456789.123456789.1234
    (insert (if open-char open-char "[")
	    (if (equal (substring cts 8 9) " ")
		"0"
	      (substring cts 8 9))
	    (substring cts 9 10)	;get "dd"
	    "-" (substring cts 4 7) "-"	;get "-mon-"
	    (substring cts 20 24)	;get "yyyy"
	    (if close-char close-char "]")))) ;end with "[dd-mon-yyyy]"


(defun date-edit ()
  "Insert a first-line comment containing the current file name, the
current time, the user login name (and full name, if different), and
system name.  Mark is left at the start of the following line so you
can easily insert further comments there.  If the first line contains
an Emacs -*-modename-*- string, or a UNIX shell-script signature, or
a PostScript signature, the comment is inserted after that line.
"
  (interactive)
  (let ((old-point (point)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((eolpos))
        (end-of-line)
        (setq eolpos (point))
        (goto-char (point-min))
        (if (re-search-forward "-\\*-.*-\\*-\\|^#!\\|^%!" eolpos t)
            (progn
              (beginning-of-line)
              (cond
               ((string-equal "#!" (buffer-substring 1 3))
                (setq comment-start "# "))
               ((string-equal "%!" (buffer-substring 1 3))
                (setq comment-start "% ")))
              (forward-line 1))
          (goto-char (point-min))))
      (if (stringp comment-start)
          (insert comment-start))
      (if (stringp buffer-file-name)
          (insert buffer-file-name))
      (insert ", " (current-time-string))
      (if (stringp comment-end)
          (insert comment-end))
      (insert "\n")
      (if (stringp comment-start)
          (insert comment-start))
      (insert "Edit by " (user-full-name)
              " <")
      (if (not (equal (user-login-name) (user-full-name)))
	  (insert (user-login-name)))
      (insert (if (string-equal mode-name "BibTeX") " at " "@")
	      (or (and (boundp 'mail-host-address) mail-host-address)
		  (system-name))
	      ">")
      (if (stringp comment-end)
	  (insert comment-end))
      (insert "\n")
      (push-mark (point) t)
      (goto-char old-point))))


(defun detab-fortran ()
  "Detab DEC Fortran source file in buffer.  A leading tab followed by
a non-zero digit puts that digit into column 6 with 5 leading blanks.
A leading tab followed by a character which is not a non-zero digit is
replaced by 6 blanks, putting that character into column 7.  Embedded
tabs are expanded by untabify to blank fill through the next column
which is a multiple of 8."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\t" nil t)
    (if (looking-at "[1-9]")
        (progn
          (delete-char -1)
          (insert "     "))
      (progn
        (delete-char -1)
        (insert "      "))))
  (untabify (point-min) (point-max))
  (goto-char (point-min)))


(defun dos ()
  "Convert UNIX LF line terminators to DOS/TOPS-20-style CR LF terminators."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (clsc-replace-string "\n" "\r\n")))


(defun down-comment-line ()
  "Move to start of next line's comment.  Equivalent to \\[next-line]
followed by \\[indent-for-comment], except that if point follows the
comment-start, and the rest of the line is empty, then that empty
comment is deleted.  That way, repeated invocations of
down-comment-line will not leave a trail of empty comments."
  (interactive)
  (backward-char (length comment-start))
  (if (looking-at (concat (regexp-quote comment-start) "[ \t]*$"))
      (progn			;delete empty comment on current line
	(kill-line nil)
	(delete-horizontal-space)))
  (next-line 1)
  (indent-for-comment))


(defun fix-typescript ()
  "Remove ANSI-style escape sequences and backspace corrections from the
buffer, reduce CR LF sequences to LF, and trim trailing space.  This
cleans up script and PHOTO logs."
  (interactive)
  (save-excursion
    (goto-char (point-min))		;strip X window title escape sequences
    (clsc-replace-regexp "\033\\]0;[^\007]*\007" "\n")
    (goto-char (point-min))		;strip ANSI terminal escape sequences
    (clsc-replace-regexp "\033\\[[^a-zA-Z]*[a-zA-Z]" "")
    (goto-char (point-min))		;change CR LF to LF
    (clsc-replace-string "\r\n" "\n")
    (goto-char (point-min))		;remove overstrike pairs
    (while (re-search-forward "[^\010]\010" nil t)
      (delete-char -2)
      (forward-char -1)))
    (goto-char (point-min))             ;strip control characters
    (clsc-replace-regexp "[\000]" "")
					;NB: use \000 instead of real NUL
					;here to avoid dataloss by
					;checksum program!
    (goto-char (point-min))             ;strip trailing whitespace
    (clsc-replace-regexp "[ \t]+$" "")
    (goto-char (1- (point-max)))
    (if (not (looking-at "\n"))		;typescripts often lack final
	(progn				;newline, so put one in if
	  (goto-char (point-max))	;needed
	  (newline)))
    (goto-char (point-min)))


(defun get-next-line ()
  "Return the next line in the buffer, leaving point following it.
Nil is returned at end-of-buffer."
  (let (start)
    (beginning-of-line)
    (setq start (point))
    (forward-line 1)
    (if (equal start (point))
        nil
      (buffer-substring start (point)))))


(defun get-next-word ()
  "Return the next ``word'' in the buffer, where a word is defined by
forward-word according to the syntax-table settings.  Point is left
following the word.  At end-of-buffer, nil is returned and point is
unchanged."
  (let (start end)
    (if (eobp)
        nil
      (progn
        (setq start (point))
        (forward-word 1)
        (setq end (point))
        (forward-word -1)
	(if (< (point) start)		;then already past last word
	    (progn
	      (goto-char (point-max))
	       nil)
	  (setq start (point))
	  (goto-char end)
	  (buffer-substring start end))))))


;;; Of the four mouse libraries in GNU Emacs 18.57 (bg-mouse, sun-mouse,
;;; sup-mouse, and x-mouse), strangely only sun-mouse provides a way to
;;; discover the mouse bindings.  The function show-mouse-bindings below
;;; rectifies that deficiency for x-mouse.  We must take care, however,
;;; to avoid referencing things that are undefined if we are not running
;;; under the X Window System

(defun initialize-mouse-bindings ()
  "Initialize an internal table of mouse button names."
  (if (and (not (boundp 'mouse-button-names))
	   (boundp 'window-system)
	   (equal window-system 'x)
	   (string-equal (substring emacs-version 0 2) "18")
	   (require 'x-mouse))
      (setq mouse-button-names
	'(
	  ("right"              x-button-right)
	  ("middle"             x-button-middle)
	  ("left"               x-button-left)
	  ("right-up"           x-button-right-up)
	  ("middle-up"          x-button-middle-up)
	  ("left-up"            x-button-left-up)
	  ("s-right"            x-button-s-right)
	  ("s-middle"           x-button-s-middle)
	  ("s-left"             x-button-s-left)
	  ("s-right-up"         x-button-s-right-up)
	  ("s-middle-up"        x-button-s-middle-up)
	  ("s-left-up"          x-button-s-left-up)
	  ("m-right"            x-button-m-right)
	  ("m-middle"           x-button-m-middle)
	  ("m-left"             x-button-m-left)
	  ("m-right-up"         x-button-m-right-up)
	  ("m-middle-up"        x-button-m-middle-up)
	  ("m-left-up"          x-button-m-left-up)
	  ("c-right"            x-button-c-right)
	  ("c-middle"           x-button-c-middle)
	  ("c-left"             x-button-c-left)
	  ("c-right-up"         x-button-c-right-up)
	  ("c-middle-up"        x-button-c-middle-up)
	  ("c-left-up"          x-button-c-left-up)
	  ("m-s-right"          x-button-m-s-right)
	  ("m-s-middle"         x-button-m-s-middle)
	  ("m-s-left"           x-button-m-s-left)
	  ("m-s-right-up"       x-button-m-s-right-up)
	  ("m-s-middle-up"      x-button-m-s-middle-up)
	  ("m-s-left-up"        x-button-m-s-left-up)
	  ("c-s-right"          x-button-c-s-right)
	  ("c-s-middle"         x-button-c-s-middle)
	  ("c-s-left"           x-button-c-s-left)
	  ("c-s-right-up"       x-button-c-s-right-up)
	  ("c-s-middle-up"      x-button-c-s-middle-up)
	  ("c-s-left-up"        x-button-c-s-left-up)
	  ("c-m-right"          x-button-c-m-right)
	  ("c-m-middle"         x-button-c-m-middle)
	  ("c-m-left"           x-button-c-m-left)
	  ("c-m-right-up"       x-button-c-m-right-up)
	  ("c-m-middle-up"      x-button-c-m-middle-up)
	  ("c-m-left-up"        x-button-c-m-left-up)
	  ("c-m-s-right"        x-button-c-m-s-right)
	  ("c-m-s-middle"       x-button-c-m-s-middle)
	  ("c-m-s-left"         x-button-c-m-s-left)
	  ("c-m-s-right-up"     x-button-c-m-s-right-up)
	  ("c-m-s-middle-up"    x-button-c-m-s-middle-up)
	  ("c-m-s-left-up"      x-button-c-m-s-left-up)
	  ))))


(defun internal-cts-fullmonth (cts)
  "Return the full month name from a current-time-string of the form
 \"Tue Feb  9 17:06:20 1988\"."
  (cond
   ((equal (substring cts 4 7) "Jan") "January")
   ((equal (substring cts 4 7) "Feb") "February")
   ((equal (substring cts 4 7) "Mar") "March")
   ((equal (substring cts 4 7) "Apr") "April")
   ((equal (substring cts 4 7) "May") "May")
   ((equal (substring cts 4 7) "Jun") "June")
   ((equal (substring cts 4 7) "Jul") "July")
   ((equal (substring cts 4 7) "Aug") "August")
   ((equal (substring cts 4 7) "Sep") "September")
   ((equal (substring cts 4 7) "Oct") "October")
   ((equal (substring cts 4 7) "Nov") "November")
   ((equal (substring cts 4 7) "Dec") "December")
   (t "???")
   ))


(defun internal-cts-month (cts)
  "Return the 3-letter month abbreviation from a current-time-string of the form
 \"Tue Feb  9 17:06:20 1988\"."
  (substring cts 4 7))


(defun internal-cts-monthday (cts)
  "Return the two digit month day from a current-time-string of the form
 \"Tue Feb  9 17:06:20 1988\"."
  (concat
   (if (equal (substring cts 8 9) " ")
       "0"
     (substring cts 8 9))		;first digit
   (substring cts 9 10)))		;second digit


(defun internal-cts-year (cts)
  "Return the year from a current-time-string of the form
 \"Tue Feb  9 17:06:20 1988\"."
  (substring cts 20 24))


(defun leading-tabify (start end)
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs when this can be
done without changing the column they end at.  The variable
tab-width controls the action.  This version converts only
leading spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (re-search-forward "^[ \t][ \t][ \t]*" nil t)
        (let ((column (current-column))
              (indent-tabs-mode t))
          (delete-region (match-beginning 0) (point))
          (indent-to column))))))


(defun leading-untabify (&optional tab-width)
  "Replace each leading tab in region by ARG (default 8) spaces.  Embedded tabs
are not touched."
  (interactive "P")
  (setq tab-width (if tab-width (prefix-numeric-value tab-width) 8))
  (goto-char (point-min))
  (let ((blanks (make-string tab-width ?\ )))
    (while (re-search-forward "^\t" (point-max) t)
      (backward-char 1)
      (while (looking-at "\t")
        (delete-char 1)
        (insert blanks))))
  (goto-char (point-min)))


(defun libgen (&optional filename)
  "Date-edit, save, compile a .el file (default = current buffer filename)
and then load it."
  (interactive)
  (progn
    (date-edit)
    (save-buffer)
    (setq filename buffer-file-name))
  (byte-compile-file filename)
  (load filename))


;;;NB: Once sort-pages is working, we should prefix each function
;;;    with a FF, then run sort-pages, strip the FF's, and deliver
;;;    an alphabetical list of functions in the library.  This could
;;;    be made conditional upon the use of a prefix argument.

(defun list-library (library)
  "List the function names, key bindings, and documentation of the
library file defined by ARG in a buffer *List Library*.  Because this
function is somewhat slow, a progress report is displayed in the echo
area."
  (interactive "FLibrary file name: ")
  (let* ((fname) (start) (tempbuf (concat "*List Library: " library "*")))
    (save-excursion
      (with-output-to-temp-buffer tempbuf
        (find-file-read-only library)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (occur "^\050defun")
	  (set-buffer "*Occur*")
          (while (re-search-forward ":\050defun[ \t]*" nil t)
            (progn
              (setq start (point))
              (re-search-forward "[ \n\t\050]" nil t)
              (setq fname (buffer-substring start (- (point) 1)))
              (if (null executing-macro)
                  (message (format "%3d percent done"
                                   (/ (* 100 (- (point) (point-min)))
                                      (- (point-max) (point-min))))))
              (let ((fval (intern fname)))
		(princ "\f------------------------------------")
		(princ "------------------------------------\n\n")
		(prin1 fval)
		(princ ":\tInvoke by: ")
		(princ (substitute-command-keys (concat "\\[" fname "]")))
		(princ "\n\n")
		(if (documentation fval)
		    (princ (documentation fval))
		  (princ "not documented"))
		(princ "\n\n")))))))
    (set-buffer tempbuf)
    (message "%s" "sorting functions")
    (sort-pages nil (point-min) (point-max))
    (goto-char (point-min))
    (clsc-replace-string "\f------------------------------------"
			 "------------------------------------")
    (goto-char (1- (point-max)))
    (if (looking-at "\f")
	(delete-char 1))
    (insert "------------------------------------"
	    "------------------------------------\n")
    (goto-char (point-min))
    (message "[done]")))


(defun make-word-list ()
  "Convert the entire buffer to a list of newline-separated ``words''
in a new buffer *Word List*, where a word is defined by forward-word
according to the syntax-table settings.  You can apply sort-lines and
unique-lines to this to obtain a list of all the unique words in a
document."
  (interactive)
  (let (word)
    (with-output-to-temp-buffer "*Word List*"
      (save-excursion
        (goto-char (point-min))
        (while (setq word (get-next-word))
          (princ (format "%s\n" word)))))))


(defun next-doubled-word (&optional arg)
  "Advance to the next doubled word in the buffer, leaving point after
the second word of that pair.  Return the word, if found, or nil if
not In emacs version 18 or earlier, the doubled word is also displayed
in the echo area.  In version 19 or later, it is highlighted in the
buffer.

Besides letter transposition and misspelling, word doubling is one of
the commonest errors in text entry."
  (interactive)
  (let (last-word this-word (looping t))
    (setq last-word (get-next-word))
    (while looping
      (progn
        (setq looping (and last-word (< (point) (point-max))))
        (if looping
            (progn
              (setq this-word (get-next-word))
              (setq looping (not (equal last-word this-word)))
              (setq last-word this-word)))))
    (if (null executing-macro)
        (if this-word
	    (progn
	      (if (and
		   (fboundp 'transient-mark-mode)
		   (> occur-highlight-time 0))
		  (let ((transient-mark-mode))
		    (push-mark (- (point) (length this-word)))
		    (transient-mark-mode 1)
		    (sit-for occur-highlight-time)
		    (pop-mark))
		(error (concat "Doubled word [" this-word "]")))))
      (message "No more doubled words"))
    this-word))


(defun now ()
  "Insert the current time in the buffer"
  (interactive)
  (insert (current-time-string)))

(defun quote-region ()
  "Insert '>> ' in front of each line in the region,
 usually for mail quoting.  The inserted string can be customized by
 setting it from the variable quote-region-prefix."
  (interactive)
  (narrow-to-region (mark) (point))
  (goto-char (point-min))
  (insert "\n" quote-region-prefix "...\n")
  (while (< (point) (point-max))
    (insert quote-region-prefix)
    (forward-line 1))
  (if (not (looking-at "$"))
      (newline))
  (insert quote-region-prefix "...\n\n")
  (widen))


(defun redate ()
  "Change the date [dd-mon-yyyy] closest to start of buffer to the
current date.  Angle brackets, square brackets, braces, or parentheses
may delineate the date field, and are preserved."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "[[(<{][ 0-3][0-9]-\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)-[0-9][0-9]+[])>}]" nil t)
      (progn
        (let ((delimiter (buffer-substring (1- (point)) (point))))
          (delete-char (- (match-beginning 0) (match-end 0)))
          (cond
           ((equal delimiter ")")
            (date ?\( ?\)))
           ((equal delimiter "]")
            (date ?\[ ?\]))
           ((equal delimiter "}")
            (date ?\{ ?\}))
           ((equal delimiter ">")
            (date ?\< ?\>))
           (t
            (date ?\[ ?\])))))))


(defun show-columns ()
  "Show a numbered column display above the current line.  With ARG,
column display begins at current column, instead of at left margin."
  (interactive)
  (let* ((leading-blanks
          (if (null current-prefix-arg) 0 (current-column)))
         (column-display (concat (make-string leading-blanks ?\ )
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789.")))
    (save-excursion
      (forward-line -1)
      (momentary-string-display
       (substring column-display
                  0 (min (1- (window-width)) (length column-display)))
       (point)))))


(defun show-mouse-bindings ()
  "Display the current mouse bindings in a temporary buffer.  If no
bindings are available, just issue an error message."
  (interactive)
  (initialize-mouse-bindings)
  (cond
   ((string-equal (substring emacs-version 0 2) "19")
    ;; In Emacs version 19, mouse bindings are stored in key maps, so
    ;; we simply the required extract lines from the output of
    ;; describe-bindings.
    (describe-bindings)
    (save-excursion
      (switch-to-buffer "*Help*")
      (goto-char (point-min))
      (keep-lines "mouse-")))
   ((boundp 'mouse-button-names)
    (with-output-to-temp-buffer "*Mouse Bindings*"
      (princ "Mouse Bindings\n\n")
      (princ
       "WARNING: window manager bindings may obscure local bindings\n\n")
      (mapcar 'show-one-mouse-binding mouse-button-names)))
   (t
    (error "No mouse bindings available"))))


(defun show-one-mouse-binding (element)
  "Display one mouse binding in the current buffer, unless the binding
is nil or to x-mouse-ignore.

ELEMENT is a list of the form (\"button-name\" x-button-xyz), where
x-button-xyz is a string whose first character is an index into
mouse-map."
  (let ((button-function (aref mouse-map (aref (eval (car (cdr element))) 0))))
    (if (not (or (eq button-function nil)
            (eq button-function 'x-mouse-ignore)))
      (princ (format "%-15s %s\n" (car element) button-function)))))


(defun show-registers ()
  "Display in a temporary buffer what is contained in all registers
that have been set."
  ;; this code is adapted from view-register in register.el
  (interactive)
  (let ((char 0)
	(val))
    (with-output-to-temp-buffer "*Registers*"
      (while (< char 256)
	(setq val (get-register char))
	(if (not (null val))
	    (progn
	      (princ "Register ")
	      (princ (single-key-description char))
	      (princ " contains ")
	      (if (integerp val)
		  (princ val)
		(if (markerp val)
		    (progn
		      (princ "a buffer position:\nbuffer ")
		      (princ (buffer-name (marker-buffer val)))
		      (princ ", position ")
		      (princ (+ 0 val)))
		  (if (consp val)
		      (progn
			(princ "the rectangle:\n")
			(while val
			  (princ (car val))
			  (terpri)
			  (setq val (cdr val))))
		    (princ "the string:\n")
		    (princ val))))
	      (princ "\n\n")))
	(setq char (1+ char))))))


(defvar sort-tokens-regexp nil
  "*Regular expression matching a token for the function sort-tokens.")


(setq sort-tokens-regexp
  "[A-Za-z0-9_$]+[ ]*([^)]*)\\|[A-Za-z0-9_$]+[ ]*\\[[^]]*\\]\\|[A-Za-z0-9$_]+")


(defun sort-tokens (&optional reverse)
  "Sort tokens (words) between point and mark.  The syntax of a token
is defined by a regular expression in the variable sort-token-regexp.

The default value of sort-token-regexp is letters, digits, underscore,
and dollar, optionally followed by a parenthesized or bracketed list.
The lists may not contain parentheses or brackets.  This handles words
in natural language text, as well as scalar and simple array
identifiers in common programming languages.

With a prefix argument, or if the optional argument REVERSE is non-nil,
sort in reverse order."
  (interactive)
  (save-restriction
    (narrow-to-region (mark) (point))
    (sort-regexp-fields (or (not (null reverse))
                            (not (null current-prefix-arg)))
                        sort-tokens-regexp "\\&" (point-min) (point-max))
    (goto-char (point-min))
    (widen)))


(defun today (&optional open-char close-char ISO-style)
  "Insert current date at point in format 22 January 1991, leaving point after
the insertion.  The optional arguments can specify alternatives to the
open and close bracket delimiters.  With a prefix argument, or a
non-nil fourth argument, the date is inserted in ISO form 1991 January 22."
  (interactive)
  (let ((cts (current-time-string)))	;get "Tue Feb  9 17:06:20 1988"
                                        ;     0123456789.123456789.1234
    (if (or ISO-style (not (null current-prefix-arg)))
	(insert (if open-char open-char "")
		(internal-cts-year cts)
		" "
		(internal-cts-fullmonth cts)
		" "
		(internal-cts-monthday cts)
		(if close-char close-char ""))
      (insert (if open-char open-char "")
	      (internal-cts-monthday cts)
	      " "
	      (internal-cts-fullmonth cts)
	      " "
	      (internal-cts-year cts)
	      (if close-char close-char ""))))) ;end with "ss mon yyyy"


(defun trim-lines (&optional column-limit)
  "Trim all characters past column ARG (default = 72) from all lines in
the entire buffer.  Any trailing whitespace is also strimmed.  Columns
are numbered 1,2,3,...  This is useful for removing line sequence
numbers in FORTRAN programs and IBM-style fixed-length record
line-numbered files."
  (interactive "P")
  (setq column-limit (if column-limit (prefix-numeric-value column-limit) 72))
  (if (< column-limit 1)
      (error "Cannot trim lines to nothingness!"))
  (let ((old-max (point-max)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (end-of-line)
        (if (> (current-column) column-limit)
            (delete-char (- column-limit (current-column))))
        (delete-horizontal-space)
        (forward-line 1)))
    (if (null executing-macro)
        (if (= old-max (point-max))
            (message "[no changes]")
          (message (format "%d characters trimmed" (- old-max (point-max))))))))


(defun trim-prefix (&optional column-limit)
  "Trim all characters up to, and including, column ARG (default = 5)
from all lines in the entire buffer.  Columns are numbered 1,2,3,...
This is useful for removing prefix-style line sequence numbers."
  (interactive "P")
  (setq column-limit (if column-limit (prefix-numeric-value
                                       column-limit) 5))
  (let (trim-column (old-max (point-max)))
    (if (> column-limit 0)
        (save-excursion
          (goto-char (point-min))
          (while (< (point) (point-max))
            (end-of-line)
            (setq trim-column (min (current-column) column-limit))
            (beginning-of-line)
            (delete-char trim-column)
            (forward-line 1))))
    (if (null executing-macro)
        (if (= old-max (point-max))
            (message "[no changes]")
          (message (format "%d characters trimmed" (- old-max (point-max))))))))


(fset 'flush-duplicate-lines 'unique-lines)


(defun unique-lines ()
  "Delete adjacent duplicate lines in the buffer from point to
end-of-buffer, leaving only the unique lines."
  (interactive)
  (let (last-line this-line)
    (setq last-line (get-next-line))
    (while last-line
      (setq this-line (get-next-line))
      (if (equal last-line this-line)
          (kill-line -1))
      (setq last-line this-line))))


(defun unix ()
  "Convert CR LF line terminators to UNIX-style LF terminators."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (clsc-replace-string "\r\n" "\n")))


(defun up-comment-line ()
  "Move to start of previous line's comment.  Equivalent to \\[previous-line]
followed by \\[indent-for-comment], except that if point follows the
comment-start, and the rest of the line is empty, then that empty
comment is deleted.  That way, repeated invocations of up-comment-line
will not leave a trail of empty comments."
  (interactive)
  (backward-char (length comment-start))
  (if (looking-at (concat (regexp-quote comment-start) "[ \t]*$"))
      (progn			;delete empty comment on current line
	(kill-line nil)
	(delete-horizontal-space)))
  (previous-line 1)
  (indent-for-comment))
