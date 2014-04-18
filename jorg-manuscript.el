(require 'ox-publish)

(setq org-latex-default-packages-alist 
      '(("utf8" "inputenc" nil)
	("T1" "fontenc" nil)
	("" "fixltx2e" nil)
	("" "natbib" t)
	("" "url" t)
	("version=3" "mhchem" t)
	("" "float" t)
	("" "graphicx" t)
	("" "textcomp" t)
	("" "underscore" t)
	("" "amsmath" t)
	;("" "attachfile" nil)
	;("" "minted" nil)
	("linktocpage,pdfstartview=FitH,colorlinks,linkcolor=blue,anchorcolor=blue,citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	 "hyperref" nil)))

;; do not put in \hypersetup
;; use your own \hypersetup{pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}
(setq org-latex-with-hyperref nil)

;; this is for code syntax highlighting in export
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
        "bibtex %b"
        "makeindex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))

;; I have not had good luck with this on windows
;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch"))
	


;; customized article. better margins
(add-to-list 'org-latex-classes
	     '("cmu-article"                          ;class-name
	       "\\documentclass{article}
\\usepackage[top=1in, bottom=1.in, left=1in, right=1in]{geometry}
[PACKAGES]
[EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; ACS journals
(add-to-list 'org-latex-classes
	     '("achemso"                          ;class-name
	       "\\documentclass{achemso}"        ; header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; APS journals
(add-to-list 'org-latex-classes '("revtex4-1"    ;class-name
				   "\\documentclass{revtex4-1}
				   [NO-DEFAULT-PACKAGES]
				   [EXTRA]
				   [PACKAGES]"
				   ("\\section{%s}" . "\\section*{%s}")
				   ("\\subsection{%s}" . "\\subsection*{%s}")
				   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				   ("\\paragraph{%s}" . "\\paragraph*{%s}")
				   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Springer journals
(add-to-list 'org-latex-classes '("svjour3"
				  "\\documentclass{svjour3}
				   [NO-DEFAULT-PACKAGES]
				   [EXTRA]
				   [PACKAGES]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Elsevier journals
(add-to-list 'org-latex-classes '("elsarticle"
				  "\\documentclass{elsarticle}"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defun kg-manuscript-export-latex ()
  "export a manuscript to latex"
  (interactive)
  (let ((org-latex-title-command "") ; avoids getting \maketitle right after begin{document}
      ;; these packages are loaded in the latex file
      (async nil)
      (subtreep nil)
      (visible-only nil)
      (body-only nil))

  (org-latex-export-to-latex async subtreep visible-only body-only
			     '(:with-author nil
			       :with-date nil
			       :with-title nil
			       :with-toc nil))))


(defun kg-build (&optional bibtex-p)
  "build manuscript. if bibtex-p is non-nil run bibtex.
TODO replace manuscript with code." 
  (interactive)
  (let ((basename (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (unless (eq 0 (shell-command 
		   (concat "pdflatex -shell-escape " basename)))
      (switch-to-buffer "*Shell Command Output*")
      (end-of-buffer)
      (error "pdflatex  failed to build"))

    (when bibtex-p
      (unless (eq 0 (shell-command (concat "bibtex " basename)))
	(switch-to-buffer "*Shell Command Output*")
	(end-of-buffer)
	(error "bibtex failed to build"))
      
      (unless (eq 0 (shell-command (concat "pdflatex -shell-escape " basename)))
	(switch-to-buffer "*Shell Command Output*")
	(end-of-buffer)
	(error "pdflatex  failed to build")))
    
    (unless (eq 0 (shell-command (concat "pdflatex -shell-escape " basename)))
      (switch-to-buffer "*Shell Command Output*")
      (end-of-buffer)
      (error "pdflatex  failed to build"))

    (kg-manuscript-cleanup)
    (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version))))

(defun kg-vanilla-export-and-build ( &optional bibtex-p)
  "simple export and build command"
  (interactive)
  (kg-manuscript-cleanup 'deep) 
  (kg-manuscript-export-latex)
  (kg-build bibtex-p) ; we run bibtex here
  (kg-manuscript-cleanup))

(defun kg-manuscript-remove-image-extensions ()
  "Removes .png extensions from \includegraphics directives in an exported latex file

Run this from an org-buffer after you have exported it to a LaTeX file"

  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (tex-contents (with-temp-buffer (insert-file-contents tex-file) (buffer-string))))
    (message tex-file)
    (with-temp-file tex-file (insert (replace-regexp-in-string 
                                      (concat "\\(\\includegraphics"
                                              "\\(\[?[^\].*\]?\\)?\\)"       ;; match optional [stuff]
                                              "{\\([^}].*\\)\.\\(png\\)}")
                                      "\\1{\\3}"  tex-contents)))))

(defun kg-manuscript-bibliography-to-bbl ()
  "Replace \bibliography{} in tex file with contents of the bbl file"
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (bbl-file (replace-regexp-in-string "org$" "bbl" org-file))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
	 (bib-file (file-name-sans-extension tex-file)))

    ;; if no .bbl run commands to get one.
    (unless (file-exists-p bbl-file)
      (unless (eq 0 (shell-command 
		     (concat "pdflatex -shell-escape " tex-file)))
	(switch-to-buffer "*Shell Command Output*")
	(end-of-buffer)
	(error "pdflatex  failed to build"))
    
      (unless (eq 0 (shell-command 
		     (concat "bibtex " bib-file)))
	(switch-to-buffer "*Shell Command Output*")
	(end-of-buffer)
	(error "bibtex failed to build")))

    (find-file tex-file)
    (goto-char (point-min))
    (re-search-forward "bibliography{" (point-max))
    (beginning-of-line)
    (kill-line)
    (insert-file-contents bbl-file)
    (delete-file bbl-file)
    (save-buffer)
    (kill-buffer)))

(defun kg-manuscript-cleanup (&optional depth)
  "delete a bunch of temporary files based on extension. depth is an optional symbol to remove more

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
	    
    
(defun kg-build-submission-manuscript ()
  "create manuscript for submission. This removes the .png extensions from graphics, and replaces the bibliography with the contents of the bbl file."
  (kg-manuscript-cleanup 'deep)
  (kg-manuscript-export-latex)
  (kg-manuscript-remove-image-extensions)
  (kg-manuscript-bibliography-to-bbl)
  (kg-build)
  (kg-manuscript-cleanup)
  (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version)))

(provide 'jorg-manuscript)
;;; jorg-manuscript.el ends here
