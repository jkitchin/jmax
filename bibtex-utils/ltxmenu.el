;;; /u/sy/beebe/emacs/ltxmenu.el, Tue Sep 16 11:24:10 1997
;;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;;;
;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "2.00",
;;;     date            = "12 December 2011",
;;;     time            = "14:04:17 MDT",
;;;     filename        = "ltxmenu.el",
;;;     address         = "University of Utah
;;;                        Department of Mathematics, 110 LCB
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 581 4148",
;;;     checksum        = "36130 865 2897 31329",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@computer.org (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "LaTeX; SLiTeX; X Window System",
;;;     license         = "GNU Public License",
;;;     supported       = "yes",
;;;     docstring       = "This file provides X Window System popup
;;;                        menu support for LaTeX-mode in GNU Emacs
;;;                        version 19 and later.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

;;; ltxmenu.el --- LaTeX popup menu support for latex.el

;;; Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
;;; 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
;;; 2012, Free Software Foundation, Inc.
;;;
;;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;;; Created: 16 June 1993
;;; Version: 1.07
;;; Keywords: LaTeX, SliTeX, X window system

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
;;; Because LaTeX-mode provides a large collection of functions, the
;;; number of menu items is so large that several menu panes are
;;; required.  More panes are needed than can be accommodated by entries
;;; in the menu bar, so we provide only a single menu bar item labelled
;;; "LaTeX" which pops up a stack of menu panes.  The stack of menu panes
;;; can be popped as well by S-mouse-1, in case the menu bar is not
;;; enabled, or is too far away.

(defconst LaTeX-menu-version "2.00")		;NB: update this at every change
(defconst LaTeX-menu-date "[12-Dec-2011]") 	;NB: update this at every change

;;; Change log:
;;;=====================================================================
;;;
;;; Version 2.00	[12-Dec-2011]
;;; This version moves the backquote-expanded macro definition of the
;;; pull-down menu into two files, ltxmenu-23.el (for emacs versions
;;; 19..23, and ltxmenu-2.el for emacs versions 24..).  This change is
;;; necessary to work around an incompatible change in backquoting for
;;; load-time macro evaluation that was introduced in emacs-20 and
;;; finally made permanent in emacs-24 when support for the old style
;;; was removed.
;;;
;;;
;;; Version 1.15	[28-Jul-2004]
;;; Add Hungarian accent support.
;;;
;;;
;;; Version 1.14	[03-Dec-1998]
;;; Add Gaelic accent support.
;;;
;;;
;;; Version 1.13	[10-Nov-1998]
;;; Add Romanian accent support.  Call LaTeX-accent-modeline on loading
;;; to reset the modeline to reflect the new default language.  In
;;; previous versions. the modeline was incorrectly left unchanged, even
;;; though the language was reset.
;;;
;;;
;;; Version 1.12	[01-Dec-1997]
;;; Add Turkish accent support.
;;;
;;;
;;; Version 1.11	[16-Sep-1997]
;;; Add Faroese accent support.
;;;
;;; Call the new function LaTeX-accent-modeline to display the current
;;; accent language in the modeline.
;;;
;;; Call the new function LaTeX-accent-menu-setup to add a top-level
;;; accent menu, since it is needed often enough that a submenu in the
;;; LaTeX menu is inconvenient (though still available).
;;;
;;;
;;; Version 1.10	[12-Sep-1997]
;;; Add Finnish, Greek, Icelandic, Irish, Latin, Polish, Portuguese, and
;;; Romaji accent support.
;;;
;;;
;;; Version 1.09	[24-Oct-1994]
;;; Add Italian accent support.
;;;
;;;
;;; Version 1.08	[23-Oct-1994]
;;; Add menus for accent support.
;;;
;;;
;;; Version 1.07	[07-Sep-1993]
;;; Add menu items for LaTeX-prettyprint-math and LaTeX-prettyprint-displaymath.
;;;
;;;
;;; Version 1.06	[16-Jun-1993]
;;; Update for Emacs 19.  The GNU Emacs version 18 LaTeX menu support is
;;; preserved in the file latex-menu.el (version 1.05), but is absent
;;; here, because there are too many differences between version 18 and
;;; 19 menu support to allow both versions to comfortably reside in a
;;; single file.

;;; Code:

;;; Please keep the functions sorted ALPHABETICALLY, except for those
;;; that must be defined after certain others.

(provide 'ltxmenu)

(require 'latex-mode "latex.el")
(require 'ltxaccnt)

(defun internal-LaTeX-accent-prepend-language (language)
  (setq LaTeX-accent-table
	(append
	 (eval (nth 1 (assoc language
			     (append LaTeX-standard-language-accent-tables
				     LaTeX-extra-language-accent-tables))))
	 ;; [12-Sep-1997] I used to include LaTeX-standard-accent-table
	 ;; in this list, but sometimes found it confusing when the
	 ;; LaTeX accents showed up during toggling of language-specific
	 ;; accents, so LaTeX-standard-accent-table is now omitted.
	 nil))
  (LaTeX-accent-modeline language))

(defun internal-x-LaTeX-menu-accents-Czech ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Czech"))

(defun internal-x-LaTeX-menu-accents-Danish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Danish"))

(defun internal-x-LaTeX-menu-accents-Faroese ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Faroese"))

(defun internal-x-LaTeX-menu-accents-Finnish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Finnish"))

(defun internal-x-LaTeX-menu-accents-French ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "French"))

(defun internal-x-LaTeX-menu-accents-Gaelic ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Gaelic"))

(defun internal-x-LaTeX-menu-accents-German ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "German"))

(defun internal-x-LaTeX-menu-accents-Greek ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Greek"))

(defun internal-x-LaTeX-menu-accents-Hungarian ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Hungarian"))

(defun internal-x-LaTeX-menu-accents-Icelandic ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Icelandic"))

(defun internal-x-LaTeX-menu-accents-Irish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Irish"))

(defun internal-x-LaTeX-menu-accents-Italian ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Italian"))

(defun internal-x-LaTeX-menu-accents-LaTeX ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "LaTeX"))

(defun internal-x-LaTeX-menu-accents-Latin ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Latin"))

(defun internal-x-LaTeX-menu-accents-Norwegian ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Norwegian"))

(defun internal-x-LaTeX-menu-accents-Polish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Polish"))

(defun internal-x-LaTeX-menu-accents-Portuguese ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Portuguese"))

(defun internal-x-LaTeX-menu-accents-Romaji ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Romaji"))

(defun internal-x-LaTeX-menu-accents-Romanian ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Romanian"))

(defun internal-x-LaTeX-menu-accents-Spanish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Spanish"))

(defun internal-x-LaTeX-menu-accents-Swedish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Swedish"))

(defun internal-x-LaTeX-menu-accents-Turkish ()
  (interactive)
  (internal-LaTeX-accent-prepend-language "Turkish"))

(defvar internal-x-LaTeX-menu-accents nil
  "Menu of LaTeX accent languages.")

(defvar internal-x-LaTeX-menu-begin-end nil
  "Menu of \\begin-\\end support functions.")

(defvar internal-x-LaTeX-menu-begin-end-1 nil
  "Supplementary menu of \\begin-\\end support functions.")

(defvar internal-x-LaTeX-menu-begin-end-2 nil
  "Supplementary menu of \\begin-\\end support functions.")

(defvar internal-x-LaTeX-menu-begin-end-3 nil
  "Supplementary menu of \\begin-\\end support functions.")

(defvar internal-x-LaTeX-menu-begin-end-4 nil
  "Supplementary menu of \\begin-\\end support functions.")

(defvar internal-x-LaTeX-menu-checking nil
  "Menu of consistency check support functions.")

(defvar internal-x-LaTeX-menu-comment nil
  "Menu of comment support functions.")

(defvar internal-x-LaTeX-menu-cross-reference nil
  "Menu of cross-reference support functions.")

(defvar internal-x-LaTeX-menu-font nil
  "Menu of font support functions.")

(defvar internal-x-LaTeX-menu-index nil
  "Menu of indexing support functions.")

(defvar internal-x-LaTeX-menu-insertion nil
  "Menu of insertion support functions.")

(defvar internal-x-LaTeX-menu-miscellaneous nil
  "Menu of miscellaneous support functions.")

(defvar internal-x-LaTeX-menu-of-menus nil
  "Top-level menu of LaTeX commands.")

(defvar internal-x-LaTeX-menu-startup nil
  "Menu of LaTeX commands for new documents.")

;;; These functions from latex.el do not get menu bindings, because they
;;; are generally run from one input character:
;;;	LaTeX-comma
;;;	LaTeX-dollar
;;;	LaTeX-dot
;;;	LaTeX-quote
;;;	LaTeX-subscript
;;;	LaTeX-superscript
;;;	LaTeX-tab
;;;	TeX-dollar

(setq internal-x-LaTeX-menu-accents
      '("Accents"
	("Czech"                . internal-x-LaTeX-menu-accents-Czech)
	("Danish"               . internal-x-LaTeX-menu-accents-Danish)
	("Faroese"              . internal-x-LaTeX-menu-accents-Faroese)
	("Finnish"              . internal-x-LaTeX-menu-accents-Finnish)
	("French"               . internal-x-LaTeX-menu-accents-French)
	("Gaelic"               . internal-x-LaTeX-menu-accents-Gaelic)
	("German"               . internal-x-LaTeX-menu-accents-German)
	("Greek"                . internal-x-LaTeX-menu-accents-Greek)
	("Hungarian"		. internal-x-LaTeX-menu-accents-Hungarian)
	("Icelandic"            . internal-x-LaTeX-menu-accents-Icelandic)
	("Irish"                . internal-x-LaTeX-menu-accents-Irish)
	("Italian"              . internal-x-LaTeX-menu-accents-Italian)
	("LaTeX"                . internal-x-LaTeX-menu-accents-LaTeX)
	("Latin"                . internal-x-LaTeX-menu-accents-Latin)
	("Norwegian"            . internal-x-LaTeX-menu-accents-Norwegian)
	("Polish"               . internal-x-LaTeX-menu-accents-Polish)
	("Portuguese"           . internal-x-LaTeX-menu-accents-Portuguese)
	("Romaji"               . internal-x-LaTeX-menu-accents-Romaji)
	("Romanian"             . internal-x-LaTeX-menu-accents-Romanian)
	("Spanish"              . internal-x-LaTeX-menu-accents-Spanish)
	("Swedish"              . internal-x-LaTeX-menu-accents-Swedish)
	("Turkish"              . internal-x-LaTeX-menu-accents-Turkish)
	))

(setq internal-x-LaTeX-menu-begin-end
      '("\\begin-\\end "
	("\\begin-\\end block (any)"         . LaTeX-begin-end-block)
	("displaymath"                       . LaTeX-displaymath)
	("equation"                          . LaTeX-equation)
	("indent LaTeX \\begin-\\end groups" . indent-LaTeX-begin-end-groups)
	("supply matching \\end"             . LaTeX-end)
	("move to \\begin"                   . LaTeX-to-begin)
	("move to \\end"                     . LaTeX-to-end)
	))

(setq internal-x-LaTeX-menu-begin-end-1
      '("\\begin-\\end: abstract .. figure*"
	("abstract"                          . internal-LaTeX-begin-abstract)
	("alltt"                             . internal-LaTeX-begin-alltt)
	("array"                             . internal-LaTeX-begin-array)
	("bf"                                . internal-LaTeX-begin-bf)
	("center"                            . internal-LaTeX-begin-center)
	("description"                       . internal-LaTeX-begin-description)
	("displaymath"                       . internal-LaTeX-begin-displaymath)
	("document"                          . internal-LaTeX-begin-document)
	("em"                                . internal-LaTeX-begin-em)
	("enumerate"                         . internal-LaTeX-begin-enumerate)
	("eqnarray"                          . internal-LaTeX-begin-eqnarray)
	("eqnarray*"                         . internal-LaTeX-begin-eqnarray*)
	("equation"                          . internal-LaTeX-begin-equation)
	("figure"                            . internal-LaTeX-begin-figure)
	("figure*"                           . internal-LaTeX-begin-figure*)
	))

(setq internal-x-LaTeX-menu-begin-end-2
      '("\\begin-\\end: flushleft .. note"
	("flushleft"                         . internal-LaTeX-begin-flushleft)
	("flushright"                        . internal-LaTeX-begin-flushright)
	("footnotesize"                      . internal-LaTeX-begin-footnotesize)
	("fussy"                             . internal-LaTeX-begin-fussy)
	("guess"                             . internal-LaTeX-begin-guess)
	("huge"                              . internal-LaTeX-begin-huge)
	("hunch"                             . internal-LaTeX-begin-hunch)
	("it"                                . internal-LaTeX-begin-it)
	("itemize"                           . internal-LaTeX-begin-itemize)
	("large"                             . internal-LaTeX-begin-large)
	("list"                              . internal-LaTeX-begin-list)
	("math"                              . internal-LaTeX-begin-math)
	("minipage"                          . internal-LaTeX-begin-minipage)
	("normalsize"                        . internal-LaTeX-begin-normalsize)
	("note"                              . internal-LaTeX-begin-note)
	))

(setq internal-x-LaTeX-menu-begin-end-3
      '("\\begin-\\end: overlay .. table"
	("overlay"                           . internal-LaTeX-begin-overlay)
	("picture"                           . internal-LaTeX-begin-picture)
	("quotation"                         . internal-LaTeX-begin-quotation)
	("quote"                             . internal-LaTeX-begin-quote)
	("scriptsize"                        . internal-LaTeX-begin-scriptsize)
	("setlength"                         . internal-LaTeX-begin-setlength)
	("sf"                                . internal-LaTeX-begin-sf)
	("simple"                            . internal-LaTeX-begin-simple)
	("sl"                                . internal-LaTeX-begin-sl)
	("slide"                             . internal-LaTeX-begin-slide)
	("sloppy"                            . internal-LaTeX-begin-sloppy)
	("sloppypar"                         . internal-LaTeX-begin-sloppypar)
	("small"                             . internal-LaTeX-begin-small)
	("tabbing"                           . internal-LaTeX-begin-tabbing)
	("table"                             . internal-LaTeX-begin-table)
	))

(setq internal-x-LaTeX-menu-begin-end-4
      '("\\begin-\\end: tabular .. xipt"
	("tabular"                           . internal-LaTeX-begin-tabular)
	("tabular*"                          . internal-LaTeX-begin-tabular*)
	("thebibliography"                   . internal-LaTeX-begin-thebibliography)
	("theindex"                          . internal-LaTeX-begin-theindex)
	("theorem"                           . internal-LaTeX-begin-theorem)
	("tiny"                              . internal-LaTeX-begin-tiny)
	("titlepage"                         . internal-LaTeX-begin-titlepage)
	("trivlist"                          . internal-LaTeX-begin-trivlist)
	("tt"                                . internal-LaTeX-begin-tt)
	("verbatim"                          . internal-LaTeX-begin-verbatim)
	("verse"                             . internal-LaTeX-begin-verse)
	("xguess"                            . internal-LaTeX-begin-xguess)
	("xiipt"                             . internal-LaTeX-begin-xiipt)
	("xipt"                              . internal-LaTeX-begin-xipt)
	))

(setq internal-x-LaTeX-menu-checking
      '("Checking"
	("\\begin-\\end nesting"             . check-LaTeX-nesting)
	("labels"                            . check-LaTeX-labels)
	("angle balance"                     . LaTeX-check-angle-balance)
	("brace balance"                     . LaTeX-check-brace-balance)
	("bracket balance"                   . LaTeX-check-bracket-balance)
	("dollar balance"                    . LaTeX-check-dollar-balance)
	("parenthesis balance"               . LaTeX-check-parenthesis-balance)
	))

(setq internal-x-LaTeX-menu-comment
      '("Comments"
	("comment region"                    . internal-LaTeX-comment)
	("uncomment region"                  . internal-LaTeX-uncomment)
	))

(setq internal-x-LaTeX-menu-cross-reference
      '("Cross-referencing"
	("\\label"                           . LaTeX-label)
	("\\pageref"                         . LaTeX-pageref)
	("\\pageref with completion"         . LaTeX-pageref-with-completion)
	("\\ref"                             . LaTeX-ref)
	("\\ref with completion"             . LaTeX-ref-with-completion)
	("show LaTeX labels"                 . show-LaTeX-labels)
	("update LaTeX labels"               . update-LaTeX-labels)
	))

(setq internal-x-LaTeX-menu-font
      '("Fonts"
	("bold       (\\bf)"                 . LaTeX-font-bf)
	("emphasized (\\em)"                 . LaTeX-font-em)
	("italic     (\\it)"                 . LaTeX-font-it)
	("roman      (\\rm)"                 . LaTeX-font-rm)
	("sans serif (\\sf)"                 . LaTeX-font-sf)
	("slanted    (\\sl)"                 . LaTeX-font-sl)
	("small caps (\\sc)"                 . LaTeX-font-sc)
	("typewriter (\\tt)"                 . LaTeX-font-tt)
	))

(setq internal-x-LaTeX-menu-index
      '("Indexing"
	("add word to index"                 . LaTeX-add-word-to-index)
	("\\index"                           . LaTeX-index)
	))

(setq internal-x-LaTeX-menu-insertion
      '("Insertion"
	("angle brackets <...>"              . LaTeX-angles)
	("\\bibitem"                         . LaTeX-bibitem)
	("braces {...}"                      . LaTeX-braces)
	("brackets [...]"                    . LaTeX-brackets)
	("\\cite"                            . LaTeX-cite)
	("counter"                           . LaTeX-counter)
	("\\footnote"                        . LaTeX-footnote)
	("\\item"                            . LaTeX-item)
	("macro (any)"                       . LaTeX-macro)
	("parentheses (...)"                 . LaTeX-parentheses)
	("\\path"                            . LaTeX-path)
	("\\protect"                         . LaTeX-protect)
	("\\verb"                            . LaTeX-verb)
	))

(setq internal-x-LaTeX-menu-miscellaneous
      '("Miscellaneous"
	("bugs and gripe reports by e-mail"  . LaTeX-gripe)
	("news of recent changes"            . LaTeX-news)
	("prettyprint inline math"           . LaTeX-prettyprint-math)
	("prettyprint display math"          . LaTeX-prettyprint-displaymath)
	("renumber figures (in comments)"    . renumber-figures)
	("renumber slides (in comments)"     . renumber-slides)
	("renumber tables (in comments)"     . renumber-tables)
	("set indentation to current column" . LaTeX-set-indentation)
	))

(setq internal-x-LaTeX-menu-startup
      '("Getting started"		; displayed at top and bottom
	("make LaTeX document"               . make-LaTeX-document)
	("make SLiTeX document"              . make-SLiTeX-document)
        ("set fundamental mode"              . internal-fundamental-mode)
	("set LaTeX mode"                    . LaTeX-mode)
	("set TeX mode"                      . tex-mode)
	("set text mode"                     . text-mode)
	("toggle abbrev mode"                . abbrev-mode)
	("toggle fill mode"                  . auto-fill-mode)
	("toggle overwrite mode"             . overwrite-mode)
	("toggle save mode"                  . auto-save-mode)
	))

;;; NB: x-popup-menu (defined in emacs/src/xmenu.c) doesn't eval its
;;; arguments---it expects them to be (position menu), and works as
;;; follows:
;;;
;;; >> Pop up a deck-of-cards menu and return user's selection.
;;; >> POSITION is a position specification.  This is either a mouse
;;; >> button event or a list ((XOFFSET YOFFSET) WINDOW) where XOFFSET
;;; >> and YOFFSET are positions in characters from the top left corner
;;; >> of WINDOW's frame.  (WINDOW may be a frame object instead of a
;;; >> window.)  This controls the position of the center of the first
;;; >> line in the first pane of the menu, not the top left of the menu
;;; >> as a whole.
;;; >>
;;; >> MENU is a specifier for a menu.  For the simplest case, MENU is a
;;; >> keymap.  The menu items come from key bindings that have a menu
;;; >> string as well as a definition; actually, the \"definition\" in
;;; >> such a key binding looks like \(STRING . REAL-DEFINITION).  To
;;; >> give the menu a title, put a string into the keymap as a
;;; >> top-level element.
;;; >>
;;; >> You can also use a list of keymaps as MENU.  Then each keymap
;;; >> makes a separate pane.
;;; >>
;;; >> When MENU is a keymap or a list of keymaps, the return value is a
;;; >> list of events.
;;; >>
;;; >> Alternatively, you can specify a menu of multiple panes with a
;;; >> list of the form (TITLE PANE1 PANE2...), where each pane is a
;;; >> list of form (TITLE ITEM1 ITEM2...).  Each ITEM is normally a
;;; >> cons cell (STRING . VALUE); but a string can appear as an
;;; >> item--that makes a nonselectable line in the menu.  With this
;;; >> form of menu, the return value is VALUE from the chosen item.
;;;
;;; We therefore need to use backquote here to eval them in advance.
;;; This setq for internal-x-LaTeX-menu-of-menus must occur after all
;;; of its components have been defined above.
;;;
;;; The menu definition prior to emacs version 24 is syntactically
;;; incompatible with that from older emacs versions, and vice versa,
;;; so we have to load the appropriate file based on the emacs version
;;; number.

(if (string-lessp (substring emacs-version 0 2) "24")
    (require 'ltxmenu-23)
  (require 'ltxmenu-24))

;;; NB: All internal-x-LaTeX-xxx commands that are called from the
;;; menus MUST call (interactive) (so commandp is true).

(defun internal-fundamental-mode ()
  "Select fundamental mode, and force mode line update, which
fundamental-mode forgot to do."
  (interactive)
  (fundamental-mode)
  (redraw-display))

(defun internal-LaTeX-begin-abstract ()
  (interactive)
  (LaTeX-begin-end-block "abstract"))

(defun internal-LaTeX-begin-alltt ()
  (interactive)
  (LaTeX-begin-end-block "alltt"))

(defun internal-LaTeX-begin-array ()
  (interactive)
  (LaTeX-begin-end-block "array"))

(defun internal-LaTeX-begin-bf ()
  (interactive)
  (LaTeX-begin-end-block "bf"))

(defun internal-LaTeX-begin-center ()
  (interactive)
  (LaTeX-begin-end-block "center"))

(defun internal-LaTeX-begin-description ()
  (interactive)
  (LaTeX-begin-end-block "description"))

(defun internal-LaTeX-begin-displaymath ()
  (interactive)
  (LaTeX-begin-end-block "displaymath"))

(defun internal-LaTeX-begin-document ()
  (interactive)
  (LaTeX-begin-end-block "document"))

(defun internal-LaTeX-begin-em ()
  (interactive)
  (LaTeX-begin-end-block "em"))

(defun internal-LaTeX-begin-enumerate ()
  (interactive)
  (LaTeX-begin-end-block "enumerate"))

(defun internal-LaTeX-begin-eqnarray ()
  (interactive)
  (LaTeX-begin-end-block "eqnarray"))

(defun internal-LaTeX-begin-eqnarray* ()
  (interactive)
  (LaTeX-begin-end-block "eqnarray*"))

(defun internal-LaTeX-begin-equation ()
  (interactive)
  (LaTeX-begin-end-block "equation"))

(defun internal-LaTeX-begin-figure ()
  (interactive)
  (LaTeX-begin-end-block "figure"))

(defun internal-LaTeX-begin-figure* ()
  (interactive)
  (LaTeX-begin-end-block "figure*"))

(defun internal-LaTeX-begin-flushleft ()
  (interactive)
  (LaTeX-begin-end-block "flushleft"))

(defun internal-LaTeX-begin-flushright ()
  (interactive)
  (LaTeX-begin-end-block "flushright"))

(defun internal-LaTeX-begin-footnotesize ()
  (interactive)
  (LaTeX-begin-end-block "footnotesize"))

(defun internal-LaTeX-begin-fussy ()
  (interactive)
  (LaTeX-begin-end-block "fussy"))

(defun internal-LaTeX-begin-guess ()
  (interactive)
  (LaTeX-begin-end-block "guess"))

(defun internal-LaTeX-begin-huge ()
  (interactive)
  (LaTeX-begin-end-block "huge"))

(defun internal-LaTeX-begin-hunch ()
  (interactive)
  (LaTeX-begin-end-block "hunch"))

(defun internal-LaTeX-begin-it ()
  (interactive)
  (LaTeX-begin-end-block "it"))

(defun internal-LaTeX-begin-itemize ()
  (interactive)
  (LaTeX-begin-end-block "itemize"))

(defun internal-LaTeX-begin-large ()
  (interactive)
  (LaTeX-begin-end-block "large"))

(defun internal-LaTeX-begin-list ()
  (interactive)
  (LaTeX-begin-end-block "list"))

(defun internal-LaTeX-begin-math ()
  (interactive)
  (LaTeX-begin-end-block "math"))

(defun internal-LaTeX-begin-minipage ()
  (interactive)
  (LaTeX-begin-end-block "minipage"))

(defun internal-LaTeX-begin-normalsize ()
  (interactive)
  (LaTeX-begin-end-block "normalsize"))

(defun internal-LaTeX-begin-note ()
  (interactive)
  (LaTeX-begin-end-block "note"))

(defun internal-LaTeX-begin-overlay ()
  (interactive)
  (LaTeX-begin-end-block "overlay"))

(defun internal-LaTeX-begin-picture ()
  (interactive)
  (LaTeX-begin-end-block "picture"))

(defun internal-LaTeX-begin-quotation ()
  (interactive)
  (LaTeX-begin-end-block "quotation"))

(defun internal-LaTeX-begin-quote ()
  (interactive)
  (LaTeX-begin-end-block "quote"))

(defun internal-LaTeX-begin-scriptsize ()
  (interactive)
  (LaTeX-begin-end-block "scriptsize"))

(defun internal-LaTeX-begin-setlength ()
  (interactive)
  (LaTeX-begin-end-block "setlength"))

(defun internal-LaTeX-begin-sf ()
  (interactive)
  (LaTeX-begin-end-block "sf"))

(defun internal-LaTeX-begin-simple ()
  (interactive)
  (LaTeX-begin-end-block "simple"))

(defun internal-LaTeX-begin-sl ()
  (interactive)
  (LaTeX-begin-end-block "sl"))

(defun internal-LaTeX-begin-slide ()
  (interactive)
  (LaTeX-begin-end-block "slide"))

(defun internal-LaTeX-begin-sloppy ()
  (interactive)
  (LaTeX-begin-end-block "sloppy"))

(defun internal-LaTeX-begin-sloppypar ()
  (interactive)
  (LaTeX-begin-end-block "sloppypar"))

(defun internal-LaTeX-begin-small ()
  (interactive)
  (LaTeX-begin-end-block "small"))

(defun internal-LaTeX-begin-tabbing ()
  (interactive)
  (LaTeX-begin-end-block "tabbing"))

(defun internal-LaTeX-begin-table ()
  (interactive)
  (LaTeX-begin-end-block "table"))

(defun internal-LaTeX-begin-tabular ()
  (interactive)
  (LaTeX-begin-end-block "tabular"))

(defun internal-LaTeX-begin-tabular* ()
  (interactive)
  (LaTeX-begin-end-block "tabular*"))

(defun internal-LaTeX-begin-thebibliography ()
  (interactive)
  (LaTeX-begin-end-block "thebibliography"))

(defun internal-LaTeX-begin-theindex ()
  (interactive)
  (LaTeX-begin-end-block "theindex"))

(defun internal-LaTeX-begin-theorem ()
  (interactive)
  (LaTeX-begin-end-block "theorem"))

(defun internal-LaTeX-begin-tiny ()
  (interactive)
  (LaTeX-begin-end-block "tiny"))

(defun internal-LaTeX-begin-titlepage ()
  (interactive)
  (LaTeX-begin-end-block "titlepage"))

(defun internal-LaTeX-begin-trivlist ()
  (interactive)
  (LaTeX-begin-end-block "trivlist"))

(defun internal-LaTeX-begin-tt ()
  (interactive)
  (LaTeX-begin-end-block "tt"))

(defun internal-LaTeX-begin-verbatim ()
  (interactive)
  (LaTeX-begin-end-block "verbatim"))

(defun internal-LaTeX-begin-verse ()
  (interactive)
  (LaTeX-begin-end-block "verse"))

(defun internal-LaTeX-begin-xguess ()
  (interactive)
  (LaTeX-begin-end-block "xguess"))

(defun internal-LaTeX-begin-xiipt ()
  (interactive)
  (LaTeX-begin-end-block "xiipt"))

(defun internal-LaTeX-begin-xipt ()
  (interactive)
  (LaTeX-begin-end-block "xipt"))

(defun internal-LaTeX-comment ()
  "Comment region."
  (interactive)
  (LaTeX-comment t))

(defun internal-LaTeX-uncomment ()
  "Uncomment region."
  (interactive)
  (LaTeX-uncomment t))

(defun x-LaTeX-help (&optional arg)
  "Mouse commands for LaTeX mode.  ARG is a pair (xoffset,yoffset)
giving the offset from the upper-left window corner in character
positions of the pointer when the mouse button is activated."
  (interactive)
  (let ((selection (x-popup-menu last-nonmenu-event
				 internal-x-LaTeX-menu-of-menus)))
    (if selection
	(call-interactively selection))))

(define-key LaTeX-mode-map [menu-bar LaTeX] (cons "LaTeX" 'x-LaTeX-help))
(define-key LaTeX-mode-map [S-mouse-1] 'x-LaTeX-help)
(LaTeX-accent-modeline "LaTeX")		;reset modeline to default language

;;; Because accents are frequently required, it has proven convenient
;;; for them to have their own top-level menu, so we provide them
;;; there, as well as having them also available in the LaTeX popup
;;; menu.

(LaTeX-accent-menu-setup LaTeX-mode-map)

;;; The following two functions took me several hours to get right.
;;; Because of the large number of menu panes needed for LaTeX-mode, it
;;; is not practical to have separate menu bar entries for each, but it
;;; may be for other editing modes, so these functions are preserved in
;;; comments, even though they are not required here.
;;;
;;; (defun internal-LaTeX-define-menu-entry (pair map)
;;;   "Add the title-function PAIR to the keymap MAP."
;;;   (define-key map (make-vector 1 (intern (car pair))) pair))
;;;
;;; (defun internal-LaTeX-new-menu (tail)
;;;   "Make a new menu from the list in internal-x-LaTeX-menu-TAIL for the
;;; argument string, TAIL."
;;;   (let ((menu (concat "internal-x-LaTeX-menu-" tail))
;;; 	(v (make-vector 2 nil))
;;; 	(keymap (concat "menu-bar-" tail)))
;;;     (aset v 0 'menu-bar)
;;;     (aset v 1 (intern tail))
;;;     (set (intern keymap) (make-sparse-keymap tail))
;;;     (define-key LaTeX-mode-map v (cons tail (eval (intern keymap))))
;;;     (mapcar (function (lambda (pair)
;;; 			(internal-LaTeX-define-menu-entry pair (eval (intern keymap)))))
;;; 	    (reverse (cdr (eval (intern menu)))))
;;;     nil))
;;;
;;; (internal-LaTeX-new-menu "begin-end-1")
;;; (internal-LaTeX-new-menu "begin-end-2")
;;; (internal-LaTeX-new-menu "begin-end-3")
;;; (internal-LaTeX-new-menu "begin-end-4")

;;; ltxmenu.el ends here
