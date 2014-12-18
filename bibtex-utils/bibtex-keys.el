;;; -*-emacs-lisp-*-
;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.04",
;;;     date            = "02 April 2002",
;;;     time            = "07:47:47 MST",
;;;     filename        = "bibtex-keys.el",
;;;     address         = "Center for Scientific Computing
;;;                        University of Utah
;;;                        Department of Mathematics, 110 LCB
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 585 1640, +1 801 581 4148",
;;;     URL             = "http://www.math.utah.edu/~beebe",
;;;     checksum        = "64531 144 694 6111",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@computer.org, beebe@ieee.org
;;;                        (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "BibTeX; GNU Emacs",
;;;     supported       = "yes",
;;;     docstring       = "This file provides key bindings for function
;;;                        keys on my Sun Type 5 keyboard to functions
;;;                        that I use frequently in BibTeX file editing.
;;;
;;;                        Because all of the bindings here are to
;;;                        function keys, rather than normal keys, this
;;;                        function is not likely to be useful for
;;;                        editing on non-Sun Type 5 keyboards.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================

(provide 'bibtex-keys)

(defconst bibtex-keys-version "1.04")		;NB: update this at every change
(defconst bibtex-keys-date "[02-Apr-2002]")	;NB: update this at every change

(require 'bibtools)
(require 'ltxaccnt)
(require 'bibtex-misc)
(require 'bibtex-mods)			;for (defun extended-bibtex-find-text ()...)

;;; (require 'filehdr)	; should be defined in filehdr.el, but is not

;;; ====================================================================
;;; Change log:
;;;
;;; Version 1.04	[02-Apr-2002]
;;; Wrap local-set-key bindings in a conditional that ensures that they
;;; only apply to bibtex-mode, so as to avoid altering local key
;;; bindings in this library is loaded in some other editing mode.
;;;
;;; Update address, FAX, and email data in file header.
;;;
;;;
;;; Version 1.03	[16-Oct-1999]
;;; Add bindings for bibtex-find-next-empty-string, and include
;;; bindings for extended-bibtex-find-text as well.
;;;
;;; Add binding for how-many to A-/ (think: A-?, since ? and / share a
;;; key).
;;;
;;;
;;; Version 1.02	[30-Aug-1999]
;;; Add binding to SunXK_F36, new in Sun Solaris 2.7.
;;;
;;;
;;; Version 1.01	[16-Sep-1997]
;;; Add binding for SunXK_F37, since emacs-20 renamed that key from
;;; SunF37.
;;;
;;;
;;; Version 1.00	[14-Sep-1997]
;;; Original version developed over a couple of years.

;;; ====================================================================
;;; Global settings (useful in other modes too):

(global-set-key [f1]		'bibtex-forward-brace-words)
(global-set-key [f2]		'bibtex-backward-brace-words)
(global-set-key [f3]		'capitalize-word)
(global-set-key [f4]		'forward-word)
(global-set-key [f5]		'toggle-case-fold-search)
(global-set-key [f6]		'downcase-word)
(global-set-key [f7]		'upcase-word)
(global-set-key [f8]		'modify-syntax-entry)
(global-set-key [SunF36]	'local-set-key)			;Sun F11
(global-set-key [C-SunF36]	'global-set-key)		;Ctl-(Sun F11)
(global-set-key [SunF37]	'LaTeX-accent-toggle)		;Sun F12 (emacs-19)
(global-set-key [SunXK_F37]	'LaTeX-accent-toggle)		;Sun F12 (emacs-20)
(global-set-key [SunXK_F36]	'local-set-key)			;Sun F11 (Solaris 2.7)
(global-set-key [C-SunXK_F36]	'global-set-key)		;Ctl-(Sun F11) (Solaris 2.7)
(global-set-key [?\A-/]		'how-many)			;ALT ?

;;; Local settings for BibTeX editing, but only if we are in bibtex-mode

(if (eq major-mode 'bibtex-mode)
    (progn

      (local-set-key [f11]		'delete-to-end-of-BibTeX-field)	;Sun STOP
      (local-set-key [f12]		'call-last-kbd-macro) ;Sun AGAIN
      (local-set-key [f19]		'find-duplicate-key) ;Sun FIND
      (local-set-key [f20]		'bibtex-fill-value) ;Sun CUT
      (local-set-key [f9] 		'update-file-header-and-save) ;Sun F9
      (local-set-key [f10]		'get-string) ;Sun F10

      (local-set-key "\C-i"		'extended-bibtex-find-text)
      (local-set-key [tab]		'extended-bibtex-find-text)
      (local-set-key [M-tab]		'bibtex-find-next-empty-string)
      ;; I have found C-tab convenient to type on Sun keyboards, although
      ;; it can only be detected inside a window system.  M-tab works in
      ;; an ASCII terminal
      (if window-system
	  (local-set-key [C-tab]	'bibtex-find-next-empty-string))

      ;; Local settings for DEC Alpha and DECstation keyboards

      (local-set-key [remove]		'delete-to-end-of-BibTeX-field)	;DEC Remove
      (local-set-key [find]		'find-duplicate-key) ;DEC Find
      (local-set-key [select]		'push-mark) ;DEC Select

      ;; Omit these two, because they overwrite bindings of PageUp and
      ;; PageDown on Sun systems:
      ;;
      ;; (local-set-key [prior]	'backward-paragraph)		;DEC Prev
      ;; (local-set-key [next]	'forward-paragraph)		;DEC Next

      ;; I've found that I use the how-many function quite often, yet it
      ;; has no standard binding.  This binding uses a key sequence that is
      ;; not otherwise bound by standard Emacs libraries, and is is
      ;; somewhat mnemonic, since ? and / are on the same key on many
      ;; keyboards.
      (local-set-key [?\A-/] 'how-many)	;ALT ?
      ))
