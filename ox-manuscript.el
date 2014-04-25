;;; ox-manuscript.el -- utilities to export scientific manuscripts

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
;; provides the export menu and setup for the scientific manuscripts we write
;;
;; A guiding principle here is that nothing is used by default. You
;; should specify it all in the org file.

;; important functions
;; ox-manuscript-export-and-build
;; ox-manuscript-export-and-build-and-open
;; ox-manuscript-build-submission-manuscript
;; ox-manuscript-build-submission-manuscript-and-open
;; ox-manuscript-export-and-build-and-email

(require 'ox)
(require 'ox-publish)

;; <<ACS journals>>
(add-to-list 'org-latex-classes
	     '("achemso"                          ;class-name
	       "\\documentclass{achemso}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"        ; header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; <<APS journals>>
(add-to-list 'org-latex-classes '("revtex4-1"    ;class-name
				   "\\documentclass{revtex4-1}
				   [NO-DEFAULT-PACKAGES]
				   [PACKAGES]
				   [EXTRA]"
				   ("\\section{%s}" . "\\section*{%s}")
				   ("\\subsection{%s}" . "\\subsection*{%s}")
				   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				   ("\\paragraph{%s}" . "\\paragraph*{%s}")
				   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; <<Springer journals>>
(add-to-list 'org-latex-classes '("svjour3"
				  "\\documentclass{svjour3}
				   [NO-DEFAULT-PACKAGES]
				   [PACKAGES]
                                   [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; <<Elsevier journals>>
(add-to-list 'org-latex-classes '("elsarticle"
				  "\\documentclass{elsarticle}
				   [NO-DEFAULT-PACKAGES]
				   [EXTRA]
				   [PACKAGES]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(defun ox-manuscript-cleanup (&optional depth)
  "delete a bunch of temporary files based on extension. depth is an optional symbol to also remove the tex source and pdf file.

'deep will also remove the tex source and pdf file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (org-base (file-name-sans-extension org-file))
         (extensions '(".aux" ".pyg" ".bbl" ".blg" ".toc"
		       ".log" ".out" ".spl" "_flymake.out" 
		       "Notes.bib" ".dvi"))
         (temp-files (mapcar (lambda (extension) (concat org-base extension)) extensions)))
    (mapcar (lambda (temp-file) 
              (if (file-exists-p temp-file) (delete-file temp-file))) temp-files)
    (when (file-exists-p "texput.log") (delete-file "texput.log"))
    (when depth
      (cond ((eq depth 'deep)
	     (when (file-exists-p (concat org-base ".tex")) (delete-file (concat org-base ".tex")))
	     (when (file-exists-p (concat org-base ".pdf")) (delete-file (concat org-base ".pdf"))))))))


(defun ox-manuscript-export-and-build (&optional async subtreep visible-only body-only options)
  "cleans up, then exports the latex and builds using the org-mode machinery"
  (interactive)
  (ox-manuscript-cleanup 'deep)
  (prog1
      (org-latex-export-to-pdf async subtreep visible-only body-only options)
    (ox-manuscript-cleanup)))


(defun ox-manuscript-export-and-build-and-open (&optional async subtreep visible-only body-only options)	      
  "cleanup, export, build and open pdf"
  (interactive)
  (org-open-file (ox-manuscript-export-and-build  async subtreep visible-only body-only options)))


(defun ox-manuscript-remove-image-extensions ()
  "Removes .png extensions from \includegraphics directives in an exported latex file.

Run this from an org-buffer after you have exported it to a LaTeX file"
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (tex-contents (with-temp-buffer (insert-file-contents tex-file) (buffer-string))))
    (message tex-file)
    (with-temp-file tex-file (insert (replace-regexp-in-string 
                                      (concat "\\(\\includegraphics"
                                              "\\(\[?[^\].*\]?\\)?\\)"       ;; match optional [stuff]
                                              "{\\([^}].*\\)\.\\(png\\)}")
                                      "\\1{\\3}"  tex-contents)))))

(defun ox-manuscript-pdflatex ()
  "run pdflatex on the tex file corresponding to an exported org file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
	 (tex-file (replace-regexp-in-string "org$" "tex" org-file)))

    ;; if no .tex run commands to get one.
    (if (file-exists-p tex-file)
	(progn
	  (message (format "running pdflatex on %s" tex-file))
	  (unless (eq 0 (shell-command 
			 (concat "pdflatex -shell-escape " tex-file)))
	    (switch-to-buffer "*Shell Command Output*")
	    (end-of-buffer)
	    (error "pdflatex  failed to build")))
      (error "no tex file found"))))

(defun ox-manuscript-bibtex ()
  "run bibtex"
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
	 (bib-file (file-name-sans-extension tex-file)))
    (message (format "running bibtex on %s" bib-file))
    (unless (eq 0 (shell-command 
		     (concat "bibtex " bib-file)))
	(switch-to-buffer "*Shell Command Output*")
	(end-of-buffer)
	(error "bibtex failed to build"))))

(defun ox-manuscript-bibliography-to-bbl ()
  "Replace \bibliography{} in tex file with contents of the bbl file.

we check for a bbl file, and if there is not one, we run pdflatex, then bibtex to get one."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (bbl-file (replace-regexp-in-string "org$" "bbl" org-file))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
	 (bib-file (file-name-sans-extension tex-file)))

    ;; if no .bbl run commands to get one.
    (unless (file-exists-p bbl-file)
      (ox-manuscript-pdflatex)    
      (ox-manuscript-bibtex))

    (find-file tex-file)
    (goto-char (point-min))
    (re-search-forward "bibliography{" (point-max))
    (beginning-of-line)
    (kill-line)
    (insert-file-contents bbl-file)
    (delete-file bbl-file)
    (save-buffer)
    (kill-buffer)))


(defun ox-manuscript-run-bibtex-p ()
  "return whether we need to run bibtex or not. Based on there being a cite link in the buffer.
We assume there is a bibliography and style defined if a cite is found. no check is made for that."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "cite:" nil t)))


(defun ox-manuscript-build ()
  "build manuscript. This is done manually here for building the submission manuscript pdf"
  (interactive)

  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (pdf-file (replace-regexp-in-string "org$" "pdf" org-file)))
    (ox-manuscript-pdflatex)

    (when (ox-manuscript-run-bibtex-p)
      (ox-manuscript-bibtex))

    (ox-manuscript-pdflatex)
    (ox-manuscript-pdflatex)
  

    (ox-manuscript-cleanup)
    (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version))

    ;; return pdf name
    pdf-file))

(defun ox-manuscript-build-submission-manuscript (&optional async subtreep visible-only body-only options)
  "create manuscript for submission. This removes the .png extensions from graphics, and replaces the bibliography with the contents of the bbl file. the result is a single, standalone tex-file, and the corresponding pdf."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (pdf-file (replace-regexp-in-string "org$" "pdf" org-file)))
    (ox-manuscript-cleanup 'deep)
    (org-latex-export-to-latex async subtreep visible-only body-only options)
    (ox-manuscript-remove-image-extensions)
    (ox-manuscript-bibliography-to-bbl)    
    (ox-manuscript-pdflatex)
    (ox-manuscript-pdflatex)
    (ox-manuscript-cleanup)

    (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version))
    pdf-file))

(defun ox-manuscript-build-submission-manuscript-and-open (&optional async subtreep visible-only body-only options)
  "build manuscript for submission and open the pdf. This removes the .png extensions from graphics, and replaces the bibliography with the contents of the bbl file. the result is a single, standalone tex-file, and the corresponding pdf."
  (interactive)
  (org-open-file (ox-manuscript-build-submission-manuscript async subtreep visible-only body-only options)))

(defun ox-manuscript-export-and-build-and-email (&optional async subtreep visible-only body-only options)
  "build the manuscript and attach the pdf to an email buffer."
  (interactive)
  (let ((pdf (ox-manuscript-export-and-build async subtreep visible-only body-only options)))
	(org-open-file pdf)
	(message-mail)
	(mml-attach-file pdf)
	(message-goto-to)))


(org-export-define-derived-backend 'cmu-manuscript 'latex
  :menu-entry
  '(?j "Export with cmu-manuscript"
       ((?L "As LaTeX buffer" org-latex-export-as-latex)
	(?l "As LaTeX file" org-latex-export-to-latex)
	(?p "As manuscript PDF file" ox-manuscript-export-and-build)
	(?o "As manuscript PDF and open" ox-manuscript-export-and-build-and-open)
	(?e "As PDF and email" ox-manuscript-export-and-build-and-email)
	(?M "As submission manuscript" ox-manuscript-build-submission-manuscript)
	(?m "As submission manuscript and open" ox-manuscript-build-submission-manuscript-and-open))))

(provide 'ox-manuscript)
