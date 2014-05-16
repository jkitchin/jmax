;;; kitchingroup-mode.el --- Summary
;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; some utilities for the kitchingroup

(require 'easymenu)

(defvar kitchingroup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\e\er") 'email-region)
    (define-key map (kbd "\e\eh") 'email-heading)
    (define-key map (kbd "\e\ep") 'ox-manuscript-export-and-build-and-email)
    (define-key map (kbd "\e\ea") 'ox-archive-create-and-mail)
    ;; insert keys
    (define-key map (kbd "\e\eR") 'org-ref-insert-ref-link)
    (define-key map (kbd "\e\eC") 'org-ref-insert-cite-link)
    (define-key map (kbd "\e\ef") 'org-footnote-action)
    map)
  "Keymap for kitchingroup mode.")

;;email aliases
(defalias 'jer 'email-region)
(defalias 'jeh 'email-heading)
(defalias 'jep 'ox-manuscript-export-and-build-and-email)
(defalias 'jea 'ox-archive-create-and-mail)

;; insert aliases
(defalias 'jif 'org-footnote-action)
(defalias 'jir 'org-ref-insert-ref-link)
(defalias 'jic 'org-ref-insert-cite-link)

(global-set-key (kbd "\e\eg") 'goto-line) 

(easy-menu-define my-menu kitchingroup-mode-map "My own menu"
  '("KitchinGroup"
    ("email"
    ["email region" email-region t]
    ["email org-mode heading" email-heading t]
    ["email org-mode as PDF" ox-manuscript-export-and-build-and-email t]
    ["email org-archive" ox-archive-create-and-mail t])
    ("org-mode"
     ["Toggle symbols" org-toggle-pretty-entities t]
     ["Toggle inline images" org-toggle-inline-images t]
     ["Toggle LaTeX images" org-preview-latex-fragment t]
     ("editing"
      ["Insert citation" org-ref-insert-cite-link t]
      ["Insert ref link" org-ref-insert-ref-link t]
      ["Open notes at point" org-ref-open-notes-at-point t]
      ["Open PDF at point" org-ref-open-pdf-at-point t]
      ["Open url at point" org-ref-open-url-at-point t]
      )
     ("export"
      ["manuscript PDF" ox-manuscript-export-and-build-and-open t]
      ["submission PDF" ox-manuscript-build-submission-manuscript-and-open t]      
      ))
    ("bibtex"
     ["find non-ascii characters" org-ref-find-non-ascii-characters t]
     ["reformat entry" bibtex-reformat t]
     ["clean entry" bibtex-clean-entry t]
     ["validate bibtex file" bibtex-validate t]
     ["sort bibtex file" bibtex-sort-buffer t]
     ["extract bibtex entries from org" org-ref-extract-bibtex-entries t]
     ["build bibliography pdf from bib file" org-ref-build-full-bibliography t]
     ["find bad citations" org-ref-bib-find-bad-citations t]
     )))


(define-minor-mode kitchingroup-mode
  "Minor mode for kitchingroup

\\{kitchingroup-mode-map}"
  :lighter " KG"
  :global t
  :keymap kitchingroup-mode-map)

(provide 'kitchingroup-mode)
;;; kitchingroup-mode.el ends here
