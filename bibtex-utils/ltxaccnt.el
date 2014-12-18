;;; /u/sy/beebe/emacs/ltxaccnt.el, Sat Jun 12 16:37:18 1993
;;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;;;
;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "0.20",
;;;     date            = "12 December 2011",
;;;     time            = "14:39:11 MDT",
;;;     filename        = "ltxaccnt.el",
;;;     address         = "University of Utah
;;;                        Department of Mathematics, 110 LCB
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 581 4148",
;;;     checksum        = "39809 997 4161 36351",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@computer.org (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "LaTeX; accent",
;;;     license         = "public domain",
;;;     supported       = "yes",
;;;     docstring       = "This GNU Emacs package provides support for input
;;;                        of accented letters in LaTeX (and other TeX-based
;;;                        document formatting systems), and also for
;;;                        ordinary text.  For that reason, it is provided
;;;                        as a standalone file that can be loaded
;;;                        independently of other macro packages.
;;;
;;;                        The general idea is that since accents are
;;;                        awkward to type on many keyboards, the Japanese
;;;                        style of input that offers a circular list of
;;;                        possibilities bound to a single toggle key is a
;;;                        reasonable model to follow.  When the function
;;;                        LaTeX-accent-toggle is invoked by a key press,
;;;                        the characters preceding point are searched for
;;;                        in a list of sublists stored in the variable
;;;                        LaTeX-accent-table.  Each sublist contains a set
;;;                        of related accented characters, and each
;;;                        invocation of LaTeX-accent-toggle by a key press
;;;                        will replace the longest matching preceding
;;;                        string with the next one in the sublist.  When
;;;                        the end of the sublist is reached, processing
;;;                        continues from the beginning, so the sublist is
;;;                        effectively circular.  Thus, if you press the
;;;                        toggle key once too many, you can keep pressing
;;;                        it until you come back around to the one you
;;;                        wanted, or use you can use the Emacs undo command
;;;                        to revert to the accent you missed.
;;;
;;;                        Generally, the toggle key binding will be on a
;;;                        function key; the default chosen here is F1.
;;;
;;;                        Although the accent tables encoded here are
;;;                        suitable for TeX and its various macro packages
;;;                        (AmSLaTeX, AmSTeX, LAmSTeX, LaTeX, SLiTeX, ...),
;;;                        suitable redefinition of the tables will make
;;;                        this scheme work for any accent representation,
;;;                        including characters in the range 128..256.  In
;;;                        order to use the upper-half of a 256-character
;;;                        set, you must be using Emacs 19 or later, and you
;;;                        must have fonts available to represent those
;;;                        characters.
;;;
;;;                        TeX supports 14 different accents, most of which
;;;                        are quite rare.  In order to keep the list of
;;;                        possibilities short, you probably want to
;;;                        customize the value of LaTeX-accent-table to the
;;;                        language(s) you need most frequently.  Customized
;;;                        variables for several European languages are
;;;                        provided below, and you can choose them simply by
;;;                        setting LaTeX-accent-table to one of them by,
;;;                        e.g.
;;;
;;;                        (setq LaTeX-accent-table LaTeX-Danish-accent-table)
;;;
;;;                        or better, by using the language table augmented
;;;                        by user-definable additions:
;;;
;;;                        (setq LaTeX-accent-table
;;;                              (append LaTeX-Danish-accent-table
;;;                                      LaTeX-extra-accent-table))
;;;
;;;                        You can of course select mixtures of languages,
;;;                        like this:
;;;
;;;                        (setq LaTeX-accent-table
;;;                              (append LaTeX-French-accent-table
;;;                                      LaTeX-German-accent-table
;;;                                      LaTeX-extra-accent-table))
;;;
;;;                        In the event of duplications, the first match in
;;;                        the combined list will be the one used.
;;;
;;;                        The default setting of LaTeX-accent-table is the
;;;                        concatenation of LaTeX-standard-accent-table and
;;;                        LaTeX-extra-accent-table, which offers more
;;;                        possibilities than you probably care to have.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

;;; ltxaccnt.el --- support for accented letter input

;;; Copyright (C) 1993, 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

;;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Created: 24 September 1993
;;; Version: 0.04
;;; Keywords: LaTeX, SliTeX

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

;;; Change log:
;;;=====================================================================
;;; NB: Do NOT forget to update LaTeX-accent-version below!
;;;
;;; Version 0.20	[12-Dec-2011]
;;; Add Turkish {\u{g}} and {\u{G}}, and Romanian {\c{s}}, {\c{t}},
;;;{\c{S}}, and {\c{T}} to LaTeX-standard-accent-table, and Hungarian
;;;{\H{u}} and {\H{U}} to LaTeX-Hungarian-accent-table.
;;;
;;; Version 0.19	[27-Sep-2011]
;;; Add forgotten uppercase members of LaTeX-Spanish-accent-table.
;;;
;;; Version 0.18	[22-Nov-2004]
;;; Add missing o-slash and O-slash to LaTeX-standard-accent-table.
;;;
;;; Version 0.17	[05-Nov-2004]
;;; Add missing C-with-acute to LaTeX-standard-accent-table.
;;;
;;; Version 0.16	[28-Jul-2004]
;;; Add Hungarian accent support.
;;;
;;; Version 0.15	[29-Dec-2000]
;;; Change several accent-i to accent-undotted i (the changes at 0.14
;;; missed a few, sigh...).  Add accent-i to Spanish accent table.
;;;
;;; Version 0.14	[13-Apr-2000]
;;; Change last remaining accent-i to accent-undotted i.
;;;
;;; Version 0.13	[17-Oct-1999]
;;; Move German umlaut accents on [aouAOU] to the front of the lists in
;;; LaTeX-standard-accent-table, because long use has shown that they
;;; are the most commonly-needed in the TUG and BibNet bibliography
;;; archive projects.  Similar accents on [eiEI] retain their old
;;; positions, because they are relatively uncommon, and rare in German.
;;;
;;; Remove the accented `A' in ISO8859-2, rather than TeX, encoding from
;;; LaTeX-Czech-accent-table; it was left there by mistake when part of
;;; the code was imported from another file.
;;;
;;;
;;; Version 0.12	[03-Dec-1998]
;;; Add Gaelic accent support.
;;;
;;;
;;; Version 0.11	[10-Nov-1998]
;;; Add Romanian accent support.
;;;
;;;
;;; Version 0.10	[15-Aug-1998]
;;; Add c-acute to LaTeX-accent-table.
;;;
;;;
;;; Version 0.09	[18-Dec-1997]
;;; Add Turkish accent support.
;;;
;;;
;;; Version 0.08	[17-Sep-1997]
;;; Add defvar calls for LaTeX-accent-language-current and
;;; LaTeX-accent-mode to avoid byte-compilation warnings.
;;;
;;;
;;; Version 0.07	[16-Sep-1997]
;;; Minor adjustments to Icelandic accents after I found an Icelandic
;;; grammar.
;;;
;;; Add Faroese support [yes, it is small language group, but adding it
;;; completes coverage of the Scandinavian family].
;;;
;;; Add new function LaTeX-accent-modeline and use it to display the
;;; current accent language in the modeline, and to make
;;; LaTeX-accent-language-current, LaTeX-accent-table, and
;;; minor-mode-alist buffer-local variables.
;;;
;;; Add new function LaTeX-accent-menu-setup to provide for simple
;;; one-line addition of accent support to ANY editing mode.  It is
;;; currently invoked from btxaccnt.el and ltxmenu.el.
;;;
;;;
;;; Version 0.06	[12-Sep-1997]
;;; Add Greek (math), Icelandic, Irish, Latin, Polish, Portuguese, and
;;; Romaji accent support.
;;;
;;; Add aa, ae, and oe expansions to Danish, Norwegian, and Swedish
;;; tables, since these are common transliterations.
;;;
;;; Add e-trema to Finnish table.
;;;
;;; Add i-circumflex and u-trema to French table.  Reorder French table
;;; entries to put *-grave before *-trema, since the former is more
;;; commonly required.
;;;
;;; Add SS, SZ, and sz to German table.  The capital form of scharfe-ess
;;; (also called ess-tzet) has not existed in German text, but recently
;;; is appearing, and may become conventional, thereby removing the
;;; peculiarity of having a single-glyph lowercase form with no
;;; corresponding single-glyph uppercase form, which complicates
;;; lettercase conversion in computer programs.
;;;
;;; Change i-accent to dotless-i-accent in all tables, and remove
;;; braces around the dotless-i in dotless-i-accent.
;;;
;;;
;;; Version 0.05	[16-Apr-1996]
;;; Change accent tables to eliminate enclosing braces around single
;;; accented letters, when the preceding control sequence is a
;;; non-letter.
;;;
;;;
;;; Version 0.04	[09-Mar-1996]
;;; Add new LaTeX2e \r accent to LaTeX-standard-accent-table, and
;;; correct some errors in the upper-case accents there, which in a few
;;; places had lower-case expansions.
;;;
;;; Add new variable LaTeX-accent-match-index, and new function
;;; LaTeX-sublist.  Update LaTeX-accent-toggle and
;;; LaTeX-accent-toggle-sublist to rotate accent sublist, so that the
;;; next time an accent is needed, the last used for the current
;;; letter(s) will be the first choice offered.
;;;
;;;
;;; Version 0.03	[24-Oct-1994]
;;; Add Italian accent table.
;;;
;;;
;;; Version 0.02	[23-Oct-1994]
;;; Add a few more accents (hacek c, r, s, and Polish l) to
;;; LaTeX-standard-accent-table because of their frequency of
;;; occurrence.  Correct two errors in Czech accent table that prevented
;;; two accents from being recognized.  Correct transposed character
;;; error in Swedish accent table.  Add more accents to Danish and
;;; German accent tables.
;;;
;;;
;;; Version 0.01	[01-Oct-1994]
;;; Unknown; I forgot to make a change log entry.
;;;
;;;
;;; Version 0.00	[24-Sep-1993]
;;; Wrote and debugged package.
;;;
;;;=====================================================================

;;; Code:

(provide 'ltxaccnt)

(defconst LaTeX-accent-version "0.20")		;NB: update this at every change
(defconst LaTeX-accent-date "[12-Dec-2011]")	;NB: update this at every change

(defvar LaTeX-accent-language-current nil
  "Name of current language selected from the Accent menu; it is
displayed in the Emacs mode line.  However, when the default LaTeX
accent language is selected, this value is set to NIL, and the mode
line does not show the language.")

(defvar LaTeX-accent-mode nil
  "Name of an entry in the minor-mode-alist, used to record the
current accent language.")

(defvar LaTeX-accent-table nil
  "*List of sublists of characters that are equivalent except for
accents.  M-x LaTeX-accent-toggle on \\[LaTeX-accent-toggle] will cycle among
them.")

;;; We define accent sequences with a surrounding pair of braces.
;;; While this is usually redundant for TeX, the extra braces are
;;; necessary for BibTeX.  Inner braces surround arguments to TeX
;;; control sequences that end in a letter.  A blank would otherwise be
;;; necessary there, but would also allow an unwanted line break when
;;; word wrapping is in effect.  Thus, "{\\H{o}}" is preferable to
;;; "\\H o".

(defconst LaTeX-standard-accent-table
  '(
    ;; The vowels aioue may take all 14 TeX accents.  For other
    ;; characters, the selection is more limited.
    ("a" "{\\\"a}" "{\\'a}" "{\\.a}" "{\\=a}" "{\\H{a}}"
     "{\\^a}" "{\\`a}" "{\\b{a}}" "{\\c{a}}" "{\\d{a}}"
     "{\\r{a}}" "{\\t{a}}" "{\\u{a}}" "{\\v{a}}" "{\\~a}")
    ("c" "{\\c{c}}" "{\\v{c}}" "{\\'c}")
    ("e" "{\\'e}" "{\\.e}" "{\\=e}" "{\\H{e}}" "{\\\"e}"
     "{\\^e}" "{\\`e}" "{\\b{e}}" "{\\c{e}}" "{\\d{e}}"
     "{\\r{e}}" "{\\t{e}}" "{\\u{e}}" "{\\v{e}}" "{\\~e}")
    ("g" "{\\u{g}}")			;new at version 0.20 [12-Dec-2011]
    ("i" "{\\'\\i}" "{\\.\\i}" "{\\=\\i}" "{\\H{\\i}}" "{\\\"\\i}"
     "{\\^\\i}" "{\\`\\i}" "{\\b{i}}" "{\\c{i}}" "{\\d{i}}"
     "{\\r{\\i}}" "{\\t{i}}" "{\\u{\\i}}" "{\\v{\\i}}" "{\\~\\i}")
    ("l" "{\\l}")
    ("n" "{\\~n}")
    ("o" "{\\\"o}" "{\\o}" "{\\'o}" "{\\.o}" "{\\=o}" "{\\H{o}}"
     "{\\^o}" "{\\`o}" "{\\b{o}}" "{\\c{o}}" "{\\d{o}}"
     "{\\r{o}}" "{\\t{o}}" "{\\u{o}}" "{\\v{o}}" "{\\~o}")
    ("r" "{\\v{r}}")
    ("s" "{\\v{s}}" "{\\c{s}}")		;updated with {\c{s}} at version 0.20 [12-Dec-2011]
    ("t" "{\\c{t}}")			;new at version 0.20 [12-Dec-2011]
    ("u" "{\\\"u}" "{\\'u}" "{\\.u}" "{\\=u}" "{\\H{u}}"
     "{\\^u}" "{\\`u}" "{\\b{u}}" "{\\c{u}}" "{\\d{u}}"
     "{\\r{u}}" "{\\t{u}}" "{\\u{u}}" "{\\v{u}}" "{\\~u}")
    ("A" "{\\\"A}" "{\\'A}" "{\\.A}" "{\\=A}" "{\\H{A}}"
     "{\\^A}" "{\\`A}" "{\\b{A}}" "{\\c{A}}" "{\\d{A}}"
     "{\\r{A}}" "{\\t{A}}" "{\\u{A}}" "{\\v{A}}" "{\\~A}")
    ("C" "{\\c{C}}" "{\\v{C}}" "{\\'C}")
    ("E" "{\\'E}" "{\\.E}" "{\\=E}" "{\\H{E}}" "{\\\"E}"
     "{\\^E}" "{\\`E}" "{\\b{E}}" "{\\c{E}}" "{\\d{E}}"
     "{\\r{E}}" "{\\t{E}}" "{\\u{E}}" "{\\v{E}}" "{\\~E}")
    ("G" "{\\u{G}}")			;new at version 0.20 [12-Dec-2011]
    ("I" "{\\'I}" "{\\.I}" "{\\=I}" "{\\H{I}}" "{\\\"I}"
     "{\\^I}" "{\\`I}" "{\\b{I}}" "{\\c{I}}" "{\\d{I}}"
     "{\\r{I}}" "{\\t{I}}" "{\\u{I}}" "{\\v{I}}" "{\\~I}")
    ("L" "{\\L}")
    ("N" "{\\~N}")
    ("O" "{\\\"O}" "{\\O}" "{\\'O}" "{\\.O}" "{\\=O}" "{\\H{O}}"
     "{\\^O}" "{\\`O}" "{\\b{O}}" "{\\c{O}}" "{\\d{O}}"
     "{\\r{O}}" "{\\t{O}}" "{\\u{O}}" "{\\v{O}}" "{\\~O}")
    ("R" "{\\v{R}}")
    ("S" "{\\v{S}}" "{\\c{S}}")		;updated with {\c{S}} at version 0.20 [12-Dec-2011]
    ("T" "{\\c{T}}")			;new at version 0.20 [12-Dec-2011]
    ("U" "{\\\"U}" "{\\'U}" "{\\.U}" "{\\=U}" "{\\H{U}}"
     "{\\^U}" "{\\`U}" "{\\b{U}}" "{\\c{U}}" "{\\d{U}}"
     "{\\r{U}}" "{\\t{U}}" "{\\u{U}}" "{\\v{U}}" "{\\~U}")))

(defvar LaTeX-extra-accent-table nil
  "*List of sublists of extra accented letter combinations to augment
those in LaTeX-standard-accent-table.")

(setq LaTeX-accent-table (append LaTeX-standard-accent-table
				 LaTeX-extra-accent-table))

(defconst LaTeX-Czech-accent-table
  '(
    ("\"" "{\\glqq}")
    ("<" "{\\flqq}")
    (">" "{\\frqq}")
    ("A" "{\\'A}")
    ("C" "{\\v{C}}")
    ("D" "{\\'D}" "{\\v{D}}")
    ("E" "{\\'E}" "{\\v{E}}")
    ("I" "{\\'I}")
    ("N" "{\\v{N}}")
    ("O" "{\\'O}")
    ("R" "{\\v{R}}")
    ("S" "{\\v{S}}")
    ("T" "{\\v{T}}" "{\\'T}")
    ("U" "{\\'U}" "{\\accent'27U}")
    ("Y" "{\\'Y}")
    ("Z" "{\\v{Z}}")
    ("a" "{\\'a}")
    ("c" "{\\v{c}}")
    ("d" "{\\'d}" "{d$\\!^{^,}$}")
    ("e" "{\\'e}" "{\\v{e}}")
    ("i" "{\\'\\i}")
    ("n" "{\\v{n}}")
    ("o" "{\\'o}")
    ("r" "{\\v{r}}")
    ("s" "{\\v{s}}")
    ("t" "{\\'t}" "{t$\\!^{^,}$}")
    ("u" "{\\'u}" "{\\accent'27u}")
    ("y" "{\\'y}")
    ("z" "{\\v{z}}")))

(defconst LaTeX-Danish-accent-table
  '(
    ; NB: Do NOT sort this table: entries with longest possible
    ; match must come first!
    ("AA" "{\\AA}")
    ("AE" "{\\AE}")
    ("A"  "{\\AA}" "{\\AE}")
    ("OE" "{\\O}")
    ("E"  "{\\'E}")
    ("O"  "{\\O}")
    ("aa" "{\\aa}")
    ("ae" "{\\ae}")
    ("a"  "{\\aa}" "{\\ae}")
    ("oe"  "{\\o}")
    ("e"  "{\\'e}")
    ("o"  "{\\o}")))

(defconst LaTeX-Faroese-accent-table
  '(
    ;; Source: G. V. C. (George Vaughan Chichester) Young and Cynthia
    ;; R. Clewer, Foroysk-ensk ordabok = Faroese-English dictionary:
    ;; with Faroese folk-lore and proverbs and a section by Professor
    ;; W. B. Lockwood on Faroese pronunciation Peel, Isle of Man :
    ;; Mansk-Svenska Pub. Co.; Torshavn, Foroyar: Distributed by
    ;; Foroya Skulabokagrunnar, 1985, 684 pages, ISBN 0-907715-22-2.

    ("AE" "{\\AE}")
    ("A"  "{\\'A}" "{\\AE}")
    ("D"  "{\\Eth}")
    ("I"  "{\\'I}")
    ("O"  "{\\'O}" "{\\O}" "{\\\"O}")
    ("U"  "{\\'U}")
    ("Y"  "{\\'Y}")
    ("ae" "{\\ae}")
    ("a"  "{\\'a}" "{\\ae}")
    ("d"  "{\\eth}")
    ("i"  "{\\'\\i}")
    ("n"  "{\\sc n}")
    ("o"  "{\\'o}"  "{\\o}" "{\\\"o}")
    ("u"  "{\\'u}")
    ("y"  "{\\'y}")))

(defconst LaTeX-Finnish-accent-table
  '(
    ("A" "{\\\"A}")
    ("E" "{\\\"E}")
    ("O" "{\\\"O}")
    ("a" "{\\\"a}")
    ("e" "{\\\"e}")
    ("o" "{\\\"o}")))

(defconst LaTeX-French-accent-table
  '(
    ("a" "{\\`a}" "{\\^a}")
    ("c" "{\\c{c}}")
    ("e" "{\\'e}" "{\\`e}" "{\\^e}" "{\\\"e}")
    ("i" "{\\^\\i}" "{\\\"\\i}")
    ("o" "{\\^o}")
    ("u" "{\\`u}" "{\\^u}" "{\\\"u}")
    ;; Accented upper-case letters are used in Canadian French
    ("A" "{\\^A}" "{\\`A}")
    ("C" "{\\c{C}}")
    ("E" "{\\'E}" "{\\\"E}" "{\\^E}" "{\\`E}")
    ("I" "{\\^I}" "{\\\"I}")
    ("O" "{\\^O}")
    ("U" "{\\`U}" "{\\^U}" "{\\\"U}")))

(defconst LaTeX-Gaelic-accent-table
  '(
    ;; Source: Edward Dwelly, The illustrated Gaelic-English
    ;; dictionary: containing every Gaelic word and meaning given in
    ;; all previously published dictionaries and a great number never
    ;; in print before, to which is prefixed a concise Gaelic grammar,
    ;; Gairm Publications, Glasgow, Scotland, 1971, 7th edition,
    ;; pp. xiv + 1034, ISBN 0-901771-21-X, LCCN PB1591 .D8 1971.

    ("a" "{\\'a}" "{\\`a}")
    ("e" "{\\'e}" "{\\`e}")
    ("i" "{\\'\\i}" "{\\`\\i}")
    ("o" "{\\'o}" "{\\`o}")
    ("u" "{\\'u}" "{\\`u}")
    ("A" "{\\'A}" "{\\`A}")
    ("E" "{\\'E}" "{\\`E}")
    ("I" "{\\'I}" "{\\`I}")
    ("O" "{\\'O}" "{\\`O}")
    ("U" "{\\'U}" "{\\`U}")))

(defconst LaTeX-German-accent-table
  '(
    ("A" "{\\\"A}")
    ("E" "{\\\"E}")
    ("O" "{\\\"O}")
    ("U" "{\\\"U}")
    ("SS" "{\\SS}")
    ("SZ" "{\\SS}")
    ("a" "{\\\"a}")
    ("e" "{\\\"e}")
    ("o" "{\\\"o}")
    ("u" "{\\\"u}")
    ("ss" "{\\ss}")
    ("sz" "{\\ss}")))

(defconst LaTeX-Greek-accent-table
  '(
    ; NB: Do NOT sort this table: entries with longest possible
    ; match must come first!
    ("G" "\\Gamma ")
    ("D" "\\Delta ")
    ("T" "\\Theta ")
    ("L" "\\Lambda ")
    ("X" "\\Xi " "\\Chi ")
    ("O" "\\Omega ")
    ("P" "\\Pi " "\\Psi ")
    ("S" "\\Sigma ")
    ("U" "\\Upsilon ")
    ("Y" "\\Upsilon ")
    ("F" "\\Phi ")
    ("W" "\\Omega ")
    ("a" "\\alpha ")
    ("b" "\\beta ")
    ("g" "\\gamma ")
    ("d" "\\delta ")
    ("e" "\\epsilon " "\\varepsilon ")
    ("z" "\\zeta ")
    ("h" "\\eta ")
    ("i" "\\iota ")
    ("k" "\\kappa ")
    ("l" "\\lambda ")
    ("m" "\\mu ")
    ("n" "\\nu ")
    ("x" "\\xi " "\\chi ")
    ("o" "\\omicron " "\\omega ")
    ("p" "\\pi " "\\varpi " "\\psi ")
    ("r" "\\rho " "\\varrho ")
    ("s" "\\sigma " "\\varsigma ")
    ("t" "\\tau " "\\theta " "\\vartheta " )
    ("u" "\\upsilon ")
    ("y" "\\upsilon ")
    ("f" "\\phi " "\\varphi ")
    ("w" "\\omega ")))

(defconst LaTeX-Hungarian-accent-table
  '(
    ;; Source: Chicago Manual of Style, 15th ed, section 10.51, p. 413
    ;; URL: http://seas3.elte.hu/szigetva/etcetera/Hungarian/diacritics.html
    ;;
    ;; [12-Dec-2011] That source omits the letter u with long accent. Those
    ;; characters are provided in Unicode as
    ;;     U0170 "Latin capital letter u with double acute"
    ;;     U0171 "Latin small letter u with double acute"
    ;; and with version 0.20 and later of this file, are now included
    ;; in the Hungarian accent table.  See also this article:
    ;;     http://en.wikipedia.org/wiki/Hungarian_alphabet

    ("A" "{\\'A}" "{\\\"A}")
    ("E" "{\\'E}" "{\\\"E}")
    ("I" "{\\'I}")
    ("O" "{\\'O}" "{\\\"O}" "{\\H{O}}" "{\\^O}")
    ("U" "{\\'U}" "{\\\"U}" "{\\H{U}}" "{\\^U}")
    ("a" "{\\'a}" "{\\\"a}")
    ("e" "{\\'e}" "{\\\"e}")
    ("i" "{\\'i}")
    ("o" "{\\'o}" "{\\\"o}" "{\\H{o}}" "{\\^o}")
    ("u" "{\\'u}" "{\\\"u}" "{\\H{u}}" "{\\^u}")))

(defconst LaTeX-Icelandic-accent-table
  '(
    ;; Source: P. J. T. Glendenning, Teach Yourself Icelandic, NTC
    ;; Publishing Group, 4255 West Touhy Avenue, Lincolnwood
    ;; (Chicago), IL 60646, USA, 1975, ISBN 0-8442-3797-3, p. xi.
    ("AE" "{\\AE}")
    ("A"  "{\\'A}" "{\\AE}")
    ("D"  "{\\Eth}")
    ("E"  "{\\'E}")
    ("I"  "{\\'I}")
    ("O"  "{\\'O}" "{\\\"O}")
    ("T"  "{\\Thorn}")
    ("U"  "{\\'U}")
    ("Y"  "{\\'Y}")
    ("ae" "{\\ae}")
    ("a"  "{\\'a}" "{\\ae}")
    ("d"  "{\\eth}")
    ("e"  "{\\'e}")
    ("i"  "{\\'\\i}")
    ("n"  "{\\sc n}")
    ("o"  "{\\'o}" "{\\\"o}")
    ("t"  "{\\thorn}")
    ("u"  "{\\'u}")
    ("y"  "{\\'y}")))

(defconst LaTeX-Irish-accent-table
  '(
    ("A" "{\\'A}")
    ("E" "{\\'E}")
    ("I" "{\\'I}")
    ("O" "{\\'O}")
    ("U" "{\\'U}")
    ("a" "{\\'a}")
    ("e" "{\\'e}")
    ("i" "{\\'\\i}")
    ("o" "{\\'o}")
    ("u" "{\\'u}")))

(defconst LaTeX-Italian-accent-table
  '(
    ("a" "{\\`a}")
    ("e" "{\\'e}" "{\\`e}")
    ("i" "{\\'\\i")
    ("o" "{\\'o}" "{\\`o}")
    ("u" "{\\'u}")
    ("A" "{\\`A}")
    ("E" "{\\'E}" "{\\`E}")
    ("I" "{\\'I}")
    ("O" "{\\'O}" "{\\`O}")
    ("U" "{\\'U}")))

(defconst LaTeX-Latin-accent-table
  '(
    ("A" "{\\=A}")
    ("E" "{\\=E}")
    ("I" "{\\=I}")
    ("O" "{\\=O}")
    ("U" "{\\=U}")
    ("a" "{\\=a}")
    ("e" "{\\=e}")
    ("i" "{\\=\\i}")
    ("o" "{\\=o}")
    ("u" "{\\=u}")))

(defconst LaTeX-Norwegian-accent-table	;identical to LaTeX-Danish-accent-table
  (eval 'LaTeX-Danish-accent-table))

(defconst LaTeX-Polish-accent-table
  '(
    ;; Source: Bernard Comrie, The World's Major Languages, Oxford
    ;; University Press, 1987, ISBN 0-19-520521-9, 0-19-506511-5,
    ;; p. 354.
    ("A" "{\\k{A}}")
    ("C" "{\\'C}")
    ("E" "{\\k{E}}")
    ("L" "{\\L}")
    ("N" "{\\'N}")
    ("O" "{\\'O}")
    ("S" "{\\'S}")
    ("Z" "{\\'Z}" "{\\.Z}")
    ("a" "{\\k{a}}")
    ("c" "{\\'c}")
    ("e" "{\\k{e}}")
    ("l" "{\\l}")
    ("n" "{\\'n}")
    ("o" "{\\'o}")
    ("s" "{\\'s}")
    ("z" "{\\'z}" "{\\.z}")
))

(defconst LaTeX-Portuguese-accent-table
  '(
    ;; Source: Bernard Comrie, The World's Major Languages, Oxford
    ;; University Press, 1987, ISBN 0-19-520521-9, 0-19-506511-5,
    ;; pp. 262--263.
    ("A" "{\\'A}" "{\\~A}" "{\\^A}")
    ("C" "{\\c{C}}")
    ("E" "{\\'E}" "{\\^E}")
    ("I" "{\\'I}")
    ("O" "{\\'O}" "{\\~O}" "{\\'O}")
    ("U" "{\\'U}")
    ("a" "{\\'a}" "{\\~a}" "{\\^a}")
    ("c" "{\\c{c}}")
    ("e" "{\\'e}" "{\\^e}")
    ("i" "{\\'\\i}")
    ("o" "{\\'o}" "{\\~o}" "{\\'o}")
    ("u" "{\\'u}")))

(defconst LaTeX-Romaji-accent-table
  '(
    ("O" "{\\=O}")
    ("U" "{\\=U}")
    ("o" "{\\=o}")
    ("u" "{\\=u}")))

(defconst LaTeX-Romanian-accent-table
  '(
    ;; The Romanian alphabet is, from
    ;;
    ;;	Irina Panovf
    ;;	Romanian-English, English-Romanian dictionary
    ;;	Bucuresti : Editura Stiintifica si Enciclopedica, 1988.
    ;;  ISBN 0-87052-713-4
    ;;
    ;;	Christina N. Hoffman
    ;;	Romanian grammar
    ;;	New York : Hippocrene Books, 1991.
    ;;	ISBN 0-87052-892-0
    ;;
    ;;	S. H. Gould and P. E. Obreanu
    ;;	Romanian-English dictionary and grammar for the mathematical sciences
    ;;	Providence : American Mathematical Society, 1967.
    ;;	[notes that \^a is now usually written as \^\i]
    ;;
    ;;	a \^a \u{a} b c d e f g h i \^\i j k l m n o p q r s \c{s} t \c{t} u v w x y z
    ;;
    ;; though q w x z ... are used only in foreign words

    ("A" "{\\^A}" "{\\u{A}}")
    ("I" "{\\^I}")
    ("S" "{\\c{S}}")
    ("T" "{\\c{T}}")
    ("a" "{\\^a}" "{\\u{a}}")
    ("i" "{\\^\\i}")
    ("s" "{\\c{s}}")
    ("t" "{\\c{t}}")))

(defconst LaTeX-Spanish-accent-table
  '(
    ("A" "{\\'A}")
    ("E" "{\\'E}")
    ("I" "{\\'\\I}")
    ("O" "{\\'O}")
    ("N" "{\\~N}")
    ("a" "{\\'a}")
    ("e" "{\\'e}")
    ("i" "{\\'\\i}")
    ("o" "{\\'o}")
    ("n" "{\\~n}")))

(defconst LaTeX-Swedish-accent-table
  '(
    ; NB: Do NOT sort this table: entries with longest possible
    ; match must come first!
    ("AA" "{\\AA}")
    ("AE" "{\\\"A}")
    ("A"  "{\\AA}" "{\\\"A}")
    ("E"  "{\\'E}")
    ("OE" "{\\\"O}")
    ("O"  "{\\\"O}")
    ("aa" "{\\aa}")
    ("ae" "{\\\"a}")
    ("a"  "{\\aa}" "{\\\"a}")
    ("oe" "{\\\"o}")
    ("e"  "{\\'e}")
    ("o"  "{\\\"o}")))

(defconst LaTeX-Turkish-accent-table
  '(
    ;; The Turkish alphabet is, from Redhouse Minidictionary:
    ;; English-Turkish, Turkish-English, Redhouse Yay{\i}nevi, Istanbul,
    ;; Turkey, 1984, ISBN 975-413-025-6:
    ;;
    ;; A B C C-cedilla D E F G G-uaccent H I-dotless I-dot J-dot K L M N
    ;; O O-umlaut P R S S-cedilla T U U-umlaut V Y Z (Q W X are unused)
    ;;
    ;; a b c c-cedilla d e f g g-uaccent h i-dotless i-dot j-dot k l m n
    ;; o o-umlaut p r s s-cedilla t u u-umlaut v y z (q w x are unused)
    ;;
    ;; The sole accent is a-caret, which sorts with a, but in the
    ;; curious order acitmak (to hurt), \^acil (immediate), aciz
    ;; (inability), \^aciz (unable).

    ("A" "{\\^A}")
    ("C" "{\\c{C}}")
    ("G" "{\\u{G}}")
    ("I" "{\\.I}")
    ("O" "{\\\"O}")
    ("S" "{\\c{S}}")
    ("U" "{\\\"U}")
    ("a" "{\\^a}")
    ("c" "{\\c{c}}")
    ("g" "{\\u{g}}")
    ("i" "{\\i}")
    ("o" "{\\\"o}")
    ("s" "{\\c{s}}")
    ("u" "{\\\"u}")))

(defvar LaTeX-extra-language-accent-tables nil
  "*List of association lists that pair language names with accent tables.")

(defconst LaTeX-standard-language-accent-tables
  '(
    ("Czech"	LaTeX-Czech-accent-table)
    ("Danish"	LaTeX-Danish-accent-table)
    ("Faroese"	LaTeX-Faroese-accent-table)
    ("Finnish"	LaTeX-Finnish-accent-table)
    ("French"	LaTeX-French-accent-table)
    ("Gaelic"	LaTeX-Gaelic-accent-table)
    ("German"	LaTeX-German-accent-table)
    ("Greek"	LaTeX-Greek-accent-table)
    ("Hungarian" LaTeX-Hungarian-accent-table)
    ("Icelandic" LaTeX-Icelandic-accent-table)
    ("Irish"	LaTeX-Irish-accent-table)
    ("Italian"	LaTeX-Italian-accent-table)
    ("LaTeX"	(append LaTeX-standard-accent-table
			LaTeX-extra-accent-table))
    ("Latin"	LaTeX-Latin-accent-table)
    ("Norwegian" LaTeX-Norwegian-accent-table)
    ("Polish"	LaTeX-Polish-accent-table)
    ("Portuguese" LaTeX-Portuguese-accent-table)
    ("Romaji"	LaTeX-Romaji-accent-table)
    ("Romanian"	LaTeX-Romanian-accent-table)
    ("Spanish"	LaTeX-Spanish-accent-table)
    ("Swedish"	LaTeX-Swedish-accent-table)
    ("Turkish"	LaTeX-Turkish-accent-table)))

(if (string-lessp (substring emacs-version 0 2) "19")
    (global-set-key "\e[224z" 'LaTeX-accent-toggle) ;F1 on Sun keyboards
  (global-set-key [f1] 'LaTeX-accent-toggle))

(defun LaTeX-accent-index-of-largest (list)
  "Return the index of the largest integer element in LIST, counting
from 0."
  (let ((k 1) (largest 0) (n (length list)))
    (while (< k n)
      (if (> (nth k list) (nth largest list))
	  (setq largest k))
      (setq k (1+ k)))
    largest))

(defun LaTeX-accent-language (&optional language)
  "Select the language used for subsequent LaTeX-accent-toggle.  With
a prefix argument, append the language accent list to the current
list, instead of replacing it."
  (interactive
   (list (completing-read "Language: "
			  (append LaTeX-standard-language-accent-tables
				  LaTeX-extra-language-accent-tables)
			  nil t nil)))

  (LaTeX-accent-modeline language)

  (let ((table nil))
    (setq table
	  (nth 1 (assoc language (append LaTeX-standard-language-accent-tables
					 LaTeX-extra-language-accent-tables))))
    (if (null current-prefix-arg)
	(setq LaTeX-accent-table (eval table))
      (setq LaTeX-accent-table (append LaTeX-accent-table (eval table))))))

(defun LaTeX-accent-match-length (s)
  "Compare S against the characters preceding point, and return the
length of S on a match, and otherwise, zero."
  (let ((start (- (point) (length s))))
    (if (and (>= start (point-min))
	     (string-equal (buffer-substring start (point)) s))
	(length s)
      0)))

(defvar LaTeX-accent-match-index 0 "Index of last matching accent
found by LaTeX-accent-toggle-sublist.")

(defun LaTeX-accent-menu-setup (mode-map)
  "Create a language accent menu in MODE-MAP."

  (define-key mode-map [menu-bar accents]
    (cons "Accents" (make-sparse-keymap "Accents")))

  ;; NB: These functions must be called in REVERSE alphabetical order by
  ;; language, because define-key adds an entry to the start of the
  ;; menu, rather than at the end.

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Turkish]
    '("Turkish" . internal-x-LaTeX-menu-accents-Turkish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Swedish]
    '("Swedish" . internal-x-LaTeX-menu-accents-Swedish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Spanish]
    '("Spanish" . internal-x-LaTeX-menu-accents-Spanish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Romanian]
    '("Romanian" . internal-x-LaTeX-menu-accents-Romanian))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Romaji]
    '("Romaji" . internal-x-LaTeX-menu-accents-Romaji))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Portuguese]
    '("Portuguese" . internal-x-LaTeX-menu-accents-Portuguese))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Polish]
    '("Polish" . internal-x-LaTeX-menu-accents-Polish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Norwegian]
    '("Norwegian" . internal-x-LaTeX-menu-accents-Norwegian))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Latin]
    '("Latin" . internal-x-LaTeX-menu-accents-Latin))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-LaTeX]
    '("LaTeX" . internal-x-LaTeX-menu-accents-LaTeX))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Italian]
    '("Italian" . internal-x-LaTeX-menu-accents-Italian))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Irish]
    '("Irish" . internal-x-LaTeX-menu-accents-Irish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Icelandic]
    '("Icelandic" . internal-x-LaTeX-menu-accents-Icelandic))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Hungarian]
    '("Hungarian" . internal-x-LaTeX-menu-accents-Hungarian))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Greek]
    '("Greek" . internal-x-LaTeX-menu-accents-Greek))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-German]
    '("German" . internal-x-LaTeX-menu-accents-German))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Gaelic]
    '("Gaelic" . internal-x-LaTeX-menu-accents-Gaelic))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-French]
    '("French" . internal-x-LaTeX-menu-accents-French))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Finnish]
    '("Finnish" . internal-x-LaTeX-menu-accents-Finnish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Faroese]
    '("Faroese" . internal-x-LaTeX-menu-accents-Faroese))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Danish]
    '("Danish" . internal-x-LaTeX-menu-accents-Danish))

  (define-key mode-map [menu-bar accents internal-x-LaTeX-menu-accents-Czech]
    '("Czech" . internal-x-LaTeX-menu-accents-Czech)))

(defun LaTeX-accent-modeline (language)
  "Put the current accent language in the minor-mode-list and force it
to show up in the mode line, unless it is the default (LaTeX), in
which case it is suppressed."
  (make-variable-buffer-local 'LaTeX-accent-language-current)
  (make-variable-buffer-local 'LaTeX-accent-table)
  (make-variable-buffer-local 'minor-mode-alist)
  (setq LaTeX-accent-language-current
	(if (string-equal language "LaTeX") nil (concat " " language)))
  (or (assq 'LaTeX-accent-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(LaTeX-accent-mode LaTeX-accent-language-current) minor-mode-alist)))
  (setq LaTeX-accent-mode t)
  (force-mode-line-update))

(defun LaTeX-accent-toggle ()
  "Find the preceding character(s) to the next entry in the
LaTeX-accent-table, and replace it by the next entry in the sublist,
treating each sublist circularly."
  (interactive)
  (let ((k 0) (n (length LaTeX-accent-table)))
    (while (and (< k n)
		(not (LaTeX-accent-toggle-sublist (nth k LaTeX-accent-table))))
      (setq k (1+ k)))
    (cond
     ((= k n)
      (error "No accented letter match"))
     ((> LaTeX-accent-match-index 0)
      (setq LaTeX-accent-table
	   (append (LaTeX-sublist 0 (1- k) LaTeX-accent-table)
		   (list (LaTeX-rotate-accent-list (nth k LaTeX-accent-table)
						   LaTeX-accent-match-index))
		   (LaTeX-sublist (1+ k) (length LaTeX-accent-table)
				  LaTeX-accent-table)))))))

(defun LaTeX-accent-toggle-sublist (sublist)
  "Compare the characters preceding point with each element of
SUBLIST, a list of accented character strings.  Find the longest
match, delete it from the buffer, and replace it by the next element
of SUBLIST, treating the list circularly."
  (if sublist
      (let* ((max-match (mapcar 'LaTeX-accent-match-length sublist))
	     (k (LaTeX-accent-index-of-largest max-match))
	     (element (nth k sublist)))
	(if (> (nth k max-match) 0)
	    (progn
	      (delete-backward-char (length element))
	      (setq LaTeX-accent-match-index (% (1+ k) (length sublist)))
	      (insert (nth LaTeX-accent-match-index sublist))
	      t)
	  nil))
    nil))

(defun LaTeX-rotate-accent-list (accent-list index)
  "Rotate an accent list, such that list (0 1 .. (index-1) index .. n)
is returned as a new list (0 index .. n 1 .. (index-1)).  Notice that
the initial element of the list does not move."
  (let ((new-list nil) (k 1))
    (setq new-list (append (list (car accent-list)) (nthcdr index accent-list)))
    (while (< k index)
      (setq new-list (append new-list (list (nth k accent-list))))
      (setq k (1+ k)))
    new-list))

(defun LaTeX-sublist (first last the-list)
  "Return a sublist containing elements FIRST .. LAST of THE-LIST,
counting list elements from 0.  Strange that Emacs Lisp lacks such a
function!"
  (let ((k (max 0 first)) (last (min last (length the-list))) (new-list nil))
    (while (<= k last)
      (setq new-list (append new-list (list (nth k the-list))))
      (setq k (1+ k)))
    new-list))

;;; ltxaccnt.el ends here
