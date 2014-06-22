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

(defgroup ox-manuscript nil
  "customization group for ox-manuscript")

(defcustom ox-manuscript-latex-command
  "pdflatex"
  "Command to run latex."
  :group 'ox-manuscript)

(defcustom ox-manuscript-bibtex-command
  "bibtex8"
  "Command to run bibtex."
  :group 'ox-manuscript)

(defcustom ox-manuscript-interactive-build
  nil
  "Determines if pdfs are built with interaction from the user. nil means just build without user interaction. Anything else will show the user a window of the results of each build step, and ask if you should continue to the next step."
  :group 'ox-manuscript)

(defun ox-manuscript-toggle-interactive-build ()
 "toggle state of ox-manuscript-interactive-build"
  (interactive)
  (if ox-manuscript-interactive-build
      (setq ox-manuscript-interactive-build nil)
    (setq ox-manuscript-interactive-build t)))

;; <<ACS journals>>
(add-to-list 'org-latex-classes
	     '("achemso"                         
	       "\\documentclass{achemso}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"        
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; <<APS journals>>
(add-to-list 'org-latex-classes '("revtex4-1"   
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
 [PACKAGES]
 [EXTRA]" 
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

  ;; this turns out to be a bad idea. some classes define their own.
  ;; insert bibliography style if needed
;  (save-excursion
;    (beginning-of-buffer)
;    (unless (re-search-forward "^bibliographystyle:" nil t)
;      (end-of-buffer)
;      (insert "\n\nbibliographystyle:unsrt")))

  ;; insert bibliography if needed
  (save-excursion
    (beginning-of-buffer)
    (unless (re-search-forward "^bibliography:" nil t)
      (end-of-buffer)
      (insert (format "\n\nbibliography:%s" (mapconcat (lambda (x) (file-relative-name x (file-name-directory (buffer-file-name)))) org-ref-default-bibliography ",")))))

  (save-buffer)
  
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

(defun ox-manuscript-latex (tex-file)
  "run `ox-manuscript-latex-command' on tex-file. This function checks for the presence of minted, and uses -shell-escape if needed. You can run this interactively, and you will be prompted for a tex file name."
  (interactive "fTex file: ")
  (message "running pdflatex on %s" tex-file)

  (let ((minted-p (with-temp-buffer
		    (insert-file-contents tex-file)
		    (beginning-of-buffer)
		    (re-search-forward "{minted}" nil t)))
	(search-upper-case nil)
	(cb (current-buffer))
	(results))

    ;; run pdflatex
    (if minted-p
	(setq results (shell-command-to-string 
		       (concat 
			ox-manuscript-latex-command 
			" -shell-escape -interaction nonstopmode " 
			tex-file)))
      ;; else
      (setq results 
	    (shell-command-to-string 
	     (concat ox-manuscript-latex-command 
		     " -interaction nonstopmode " 
		     tex-file))))

    (with-current-buffer (get-buffer-create "*latex*")
      (insert results))))

(defun ox-manuscript-bibtex (tex-file)
  "Run `ox-manuscript-bibtex-command' on the tex-file."
  (interactive "fTex file: ")
  (message "running bibtex on %s" tex-file)

  (let* ((basename (file-name-sans-extension tex-file))
	 (output (shell-command-to-string (concat ox-manuscript-bibtex-command " " basename))))
    (with-current-buffer (get-buffer-create "*bibtex*")
      (insert output))))

(defun ox-manuscript-makeindex (tex-file)
  "run makeindex program"
  (interactive "fTex file: ")
  (let* ((basename (file-name-sans-extension tex-file))
	 (output (shell-command-to-string (concat "makeindex " basename))))
    (with-current-buffer (get-buffer-create "*makeindex*")
      (insert output))))

(defun ox-manuscript-latex-pdf-process (quoted-tex-file)
  "Build a tex-file to pdf. The argument is called quoted-tex-file because this seems to be what org-mode passes to this function. The function strips the quotes out. Depending on the value of `ox-manuscript-interactive-build', you will get buffers of the intermediate output steps."
  (interactive "fTex file: ")
  ;; it seems the filename passed to this function from org-mode has
  ;; "" in it. we remove them here.
  (let* ((tex-file (replace-regexp-in-string "\"" "" quoted-tex-file))	  
	 (basename (file-name-sans-extension tex-file))
	 (pdf-file (concat basename ".pdf"))
	 (status)
	 (cb (current-buffer))
	 (run-makeindex-p) 
	 (run-bibtex-p))
 
    ;; start out clean
    (ox-manuscript-cleanup)

    (when (file-exists-p pdf-file)
      (delete-file pdf-file))
			 
    (with-temp-buffer
      (insert-file-contents tex-file)
      (beginning-of-buffer)
      (setq run-makeindex-p (re-search-forward "\\\\makeindex" nil t))
      (beginning-of-buffer)
      (setq run-bibtex-p (re-search-forward "bibliography" nil t)))

    (setq status (catch 'status
      ;; run first latex
      (ox-manuscript-latex tex-file)    
      (when ox-manuscript-interactive-build
	(switch-to-buffer "*latex*")
	(end-of-buffer)
	(occur "warning\\|undefined\\|error\\|missing")
	(if (y-or-n-p "Continue to bibtex?")
	    ;; continuing. delete buffers
	    (progn 
	      (mapcar (lambda (x) (when (get-buffer x) (kill-buffer x)))
		      '("*latex*" "*bibtex*" "*makeindex*" "*Occur*"))
	      (switch-to-buffer cb))
	  ;; not continuing
	  (throw 'status nil)))

      ;; run bibtex if needed
      (when run-bibtex-p
	(ox-manuscript-bibtex tex-file)
	(when ox-manuscript-interactive-build
	  (switch-to-buffer "*bibtex*")
	  (end-of-buffer)
	  (occur "warning\\|undefined\\|error\\|missing")
	  (if (y-or-n-p "Continue?")
	      ;; continuing. delete buffers
	      (progn 
	 	(mapcar (lambda (x) (when (get-buffer x) (kill-buffer x)))
	 		'("*latex*" "*bibtex*" "*makeindex*" "*Occur*"))
		(switch-to-buffer cb))
	    ;; not continuing
	    (throw 'status nil))))

      (when run-makeindex-p
	(ox-manuscript-makeindex tex-file)
	(when ox-manuscript-interactive-build
	  (switch-to-buffer "*makeindex*")
	  (end-of-buffer)
	  (occur "warning\\|undefined\\|error\\|missing")
	  (if (y-or-n-p "Continue to latex 2?")
	      ;; continuing. delete buffers
	      (progn 
	 	(mapcar (lambda (x) (when (get-buffer x) (kill-buffer x)))
	 		'("*latex*" "*bibtex*" "*makeindex*" "*Occur*"))
		(switch-to-buffer cb))
	    ;; not continuing
	    (throw 'status nil))))

      (ox-manuscript-latex tex-file)    
      (when ox-manuscript-interactive-build
	(switch-to-buffer "*latex*")
	(end-of-buffer)
	(occur "warning\\|undefined\\|error\\|missing")
	(if (y-or-n-p "Continue to latex3?")
	    ;; continuing. delete buffers
	    (progn 
	      (mapcar (lambda (x) (when (get-buffer x) (kill-buffer x)))
		      '("*latex*" "*bibtex*" "*makeindex*" "*Occur*"))
	      (switch-to-buffer cb))
	  ;; not continuing
	  (throw 'status nil)))

      (ox-manuscript-latex tex-file)
      (mapcar (lambda (x) (when (get-buffer x) (kill-buffer x)))
	      '("*latex*" "*bibtex*" "*makeindex*" "*Occur*"))
      "done"))

    (message "Finished with status = %s. %s exists = %s in %s." status pdf-file (file-exists-p pdf-file) default-directory)
    0))

(setq org-latex-pdf-process 'ox-manuscript-latex-pdf-process)

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
      (ox-manuscript-latex tex-file)    
      (ox-manuscript-bibtex tex-file))

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
	 (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (pdf-file (replace-regexp-in-string "org$" "pdf" org-file)))
    (ox-manuscript-latex tex-file)

    (when (ox-manuscript-run-bibtex-p)
      (ox-manuscript-bibtex tex-file))

    (ox-manuscript-latex tex-file)
    (ox-manuscript-latex tex-file)
  
    (ox-manuscript-cleanup)
    (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version))

    ;; return pdf name
    pdf-file))

(defun ox-manuscript-export-submission-manuscript (&optional async subtreep visible-only body-only options)
  "create manuscript for submission. This removes the .png extensions from graphics, and replaces the bibliography with the contents of the bbl file. the result is a single, standalone tex-file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file)))
    (ox-manuscript-cleanup 'deep)
    (org-latex-export-to-latex async subtreep visible-only body-only options)
    (ox-manuscript-remove-image-extensions)
    (ox-manuscript-bibliography-to-bbl)
    (ox-manuscript-cleanup)
    tex-file))

(defun ox-manuscript-build-submission-manuscript (&optional async subtreep visible-only body-only options)
  "create manuscript for submission. This removes the .png extensions from graphics, and replaces the bibliography with the contents of the bbl file. the result is a single, standalone tex-file, and the corresponding pdf."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
	 (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (pdf-file (replace-regexp-in-string "org$" "pdf" org-file)))
    (ox-manuscript-cleanup 'deep)
    (org-latex-export-to-latex async subtreep visible-only body-only options)
    (ox-manuscript-remove-image-extensions)
    (ox-manuscript-bibliography-to-bbl)    
    (ox-manuscript-latex tex-file)
    (ox-manuscript-latex tex-file)
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

;; The backend options
(org-export-define-derived-backend 'cmu-manuscript 'latex
  :menu-entry
  '(?j "Export with cmu-manuscript"
       ((?L "As LaTeX buffer" org-latex-export-as-latex)
	(?l "As LaTeX file" org-latex-export-to-latex)
	(?p "As manuscript PDF file" ox-manuscript-export-and-build)
	(?o "As manuscript PDF and open" ox-manuscript-export-and-build-and-open)
	(?e "As PDF and email" ox-manuscript-export-and-build-and-email)
	(?s "As submission manuscript tex" ox-manuscript-export-submission-manuscript)
	(?M "As submission manuscript pdf" ox-manuscript-build-submission-manuscript)
	(?m "As submission manuscript pdf and open" ox-manuscript-build-submission-manuscript-and-open))))

(provide 'ox-manuscript)
