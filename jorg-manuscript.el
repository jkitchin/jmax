;; jorg-manuscript
;; provides some customized Latex classes for export
;; No default packages are used for the journal exports. You must specify them in #+LATEX_HEADER: lines
;;
;; provides the default list of packages used in the kitchin group
;;
;; Provides functions to export and build manuscripts
;; 
;; To simply build a pdf run (kg-vanilla-export-and-build ( &optional bibtex-p))
;;
;; To make the submission version, which has no graphics extensions, and embedded bibliography run
;; (kg-build-submission-manuscript) after you build it in the previous step.
;; 
;; [[ACS journals]]  C-c o will jump to this position
;; [[APS journals]]
;; [[Springer journals]]
;; [[Elsevier journals]]

(require 'ox)
(require 'ox-publish)

(defun kg-manuscript-export-latex ()
  "export a manuscript to latex"
  (interactive)
  (let ((async nil)
	(subtreep nil)
	(visible-only nil)
	(body-only nil))

    (org-latex-export-to-latex async subtreep visible-only body-only
			       '(:with-author nil
					      :with-date nil
					      :with-title nil
					      :with-toc nil))))

(defun kg-manuscript-cleanup (&optional depth)
  "delete a bunch of temporary files based on extension. depth is an optional symbol to remove more.

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

(defun kg-run-bibtex-p ()
  "return whether we need to run bibtex or not. Based on there being a cite link in the buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "cite:" nil t)))


(defun kg-build (&optional bibtex-p)
  "build manuscript. if bibtex-p is non-nil run bibtex."
  (interactive)
  (let ((basename (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (unless (eq 0 (shell-command 
		   (concat "pdflatex -shell-escape " basename)))
      (switch-to-buffer "*Shell Command Output*")
      (end-of-buffer)
      (error "pdflatex  failed to build"))

    (when (or bibtex-p (kg-run-bibtex-p))
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
    (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version))

    ;; return pdf name
    (concat basename ".pdf")
    ))

(defun kg-vanilla-export-and-build ( &optional bibtex-p)
  "simple export and build command. Starts from a clean build, and cleans up after itself."
  (interactive)
  (kg-manuscript-cleanup 'deep) 
  (kg-manuscript-export-latex)
  (org-open-file (kg-build bibtex-p)) ; we run bibtex here if it is asked for.
  (kg-manuscript-cleanup))

(defun kg-manuscript-remove-image-extensions ()
  "Removes .png extensions from \includegraphics directives in an exported latex file.

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


(defun kg-build-submission-manuscript ()
  "create manuscript for submission. This removes the .png extensions from graphics, and replaces the bibliography with the contents of the bbl file. the result is a single, standalone tex-file, and the corresponding pdf. You must have built the manuscript with bibtex first."
  (kg-manuscript-cleanup 'deep)
  (kg-manuscript-export-latex)
  (kg-manuscript-remove-image-extensions)
  (kg-manuscript-bibliography-to-bbl)
  (kg-build)
  (kg-manuscript-cleanup)
  (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version)))



(provide 'jorg-manuscript)
;;; jorg-manuscript.el ends here
