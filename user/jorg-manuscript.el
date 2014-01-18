(require 'ox-publish)

(defun j/org-manuscript-export-revtex ()
  "exports an org-buffer to a revtex LaTeX manuscript

You will usually have something like this at the top of the org-file
#+LATEX_CLASS: revtex4-1
#+LATEX_CLASS_OPTIONS: [aps,prb,citeautoscript,preprint,citeautoscript,showkeys]

you can add additional LaTeX packages if needed with 
#+LATEX_HEADER: \\usepackage{xyz}

Code examples are not treated with minted.
"

  (interactive)
  (let ((org-latex-default-class "revtex4-1")
        (org-latex-classes '(("revtex4-1"    ;class-name
                              "\\documentclass{revtex4-1}"        ; header-string
                              ("\\section{%s}" . "\\section*{%s}")
                              ("\\subsection{%s}" . "\\subsection*{%s}")
                              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                              ("\\paragraph{%s}" . "\\paragraph*{%s}")
                              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
        (org-latex-title-command "") ; avoids getting \maketitle right after begin{document}
        (org-latex-default-packages-alist ;; these packages are loaded in the latex file
         '(("AUTO" "inputenc" t)
           ("T1" "fontenc" t)
           ("" "graphicx" t)
           ("" "longtable" nil)
           ("" "float" nil)
           ("" "wrapfig" nil)
           ("normalem" "ulem" t)
           ("version=3" "mhchem" t)
           ("" "textcomp" t)
           ("" "marvosym" t)
           ("" "wasysym" t)
           ("" "latexsym" t)
           ("" "amssymb" t)
           ("" "amsmath" t)
           ("" "amstext" nil)            
           ("" "natbib" t)
           ("" "url" t)
           ("" "underscore" t)
           ("linktocpage,
  pdfstartview=FitH,
  colorlinks,
  linkcolor=blue,
  anchorcolor=blue,
  citecolor=blue,
  filecolor=blue,
  menucolor=blue,
  urlcolor=blue" "hyperref" t)
            "\\tolerance=1000"))
        (async nil)          ; no asynchronous export
        (subtreep nil)       ; export whole buffer
        (visible-only nil)   ; export
        (body-only nil))     ; export whole document
    
    (org-latex-export-to-latex async subtreep visible-only body-only
                               '(:with-author nil
                                 :with-date nil
                                 :with-title nil
                                 :with-toc nil))))


(defun j/org-manuscript-replace-image-extensions ()
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


(defun j/org-manuscript-run-latex-bibtex ()
  "Run Latex and bibtex to generate the bbl file"
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file)))

    (shell-command (concat "latex -shell-escape " (file-name-sans-extension tex-file)))
    (shell-command (concat "bibtex " (file-name-sans-extension tex-file)))))


(defun j/org-manuscript-bibliography-to-bbl ()
  "Replace \bibliography{} in tex file with contents of the bbl file

Run this after you have run latex on your tex file to generate the bbl file"
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (bbl-file (replace-regexp-in-string "org$" "bbl" org-file))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file)))
    (find-file tex-file)
    (goto-char (point-min))
    (re-search-forward "bibliography{" (point-max))
    (beginning-of-line)
    (kill-line)
    (insert-file-contents bbl-file)
    (save-buffer)
    (kill-buffer)))

(defun j/org-manuscript-cleanup ()
  "delete a bunch of temporary files based on extension"
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (org-base (file-name-sans-extension org-file))
         (extensions '(".aux" ".bbl" ".blg" ".log" ".out" ".spl" "_flymake.out" "Notes.bib" ".dvi"))
         (temp-files (mapcar (lambda (extension) (concat org-base extension)) extensions)))
    (mapcar (lambda (temp-file) 
              (if (file-exists-p temp-file) (delete-file temp-file))) temp-files)))
    
(defun j/org-publish-aps ()
  "Convert an org-mode buffer to an APS manuscript"
  (interactive)
  ;; 1. export to regular latex with customized headings
  (j/org-manuscript-export-revtex)
  ;; 2. replace image extensions
  (j/org-manuscript-replace-image-extensions)
  ;; 3. Run latex and bibtex to generate .bbl file
  (j/org-manuscript-run-latex-bibtex)
  ;; 4. Insert .bbl contents
  (j/org-manuscript-bibliography-to-bbl)
  ;; 5. delete temporary files
  (j/org-manuscript-cleanup))

(provide 'jorg-manuscript)
;;; jorg-manuscript.el ends here
