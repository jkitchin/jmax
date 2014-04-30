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
    (define-key map (kbd "\M-e r") 'email-region)
    (define-key map (kbd "\M-e h") 'email-heading)
    (define-key map (kbd "\M-e p") 'ox-manuscript-export-and-build-and-email)
    (define-key map (kbd "\M-e a") 'ox-archive-create-and-mail)
    ;; insert keys
    (define-key map (kbd "\M-i r") 'org-insert-ref-link)
    (define-key map (kbd "\M-i c") 'jorg-insert-cite-link)
    (define-key map (kbd "\M-i f") 'org-footnote-action)
    map)
  "Keymap for kitchingroup mode.")

(easy-menu-define my-menu kitchingroup-mode-map "My own menu"
  '("KitchinGroup"
    ("email"
    ["email region" email-region t]
    ["email org-mode heading" email-heading t]
    ["email org-mode as PDF" ox-manuscript-export-and-build-and-email t]
    ["email org-archive" ox-archive-create-and-mail t])
    ("org-mode"
     ("editing"
      ["Insert citation" jorg-insert-cite-link t]
      ["Insert ref link" org-insert-ref-link t]
      ["Open notes at point" jorg-bib-open-notes-at-point t]
      ["Open PDF at point" jorg-bib-open-pdf-at-point t]
      ["Open url at point" jorg-bib-open-url-at-point t]
      ["Citation tooltip" jorg-bib-tooltip t]
      )
     ("export"
      ["manuscript PDF" ox-manuscript-export-and-build-and-open t]
      ["submission PDF" ox-manuscript-build-submission-manuscript-and-open t]      
      ))
    ("bibtex"
     ["find non-ascii characters" find-non-ascii-characters t]
     ["reformat entry" bibtex-reformat t]
     ["clean entry" bibtex-clean-entry t]
     ["validate bibtex file" bibtex-validate-globally t]
     ["build bibliography pdf from bib file" jb-build-full-bibliography t]
     ["find bad citations" jorg-bib-find-bad-citations]
     )))


(define-minor-mode kitchingroup-mode
  "Minor mode for kitchingroup

\\{kitchingroup-mode-map}"
  :lighter " KG"
  :global t
  :keymap kitchingroup-mode-map)

(provide 'kitchingroup-mode)
;;; kitchingroup-mode.el ends here
