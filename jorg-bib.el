(defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (make-local-variable 'reftex-cite-format)
    (setq reftex-cite-format 'org)
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(eval-after-load 'reftex-vars
  '(progn
     (add-to-list 'reftex-cite-format-builtin
                  '(org "Org-mode citation"
                        ((?\C-m . "[[cite:%l]]")
                         (?t . "[[textcite:%l]]")
                         (?p . "[[parencite:%l]]")
                         (?s . "[[posscite:%l]]")
                         (?a . "[[citeauthor:%l]]")
                         (?y . "[[citeyear:%l]]"))))))

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; bibliography and bibliography style code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; link to hold a bibliography bibtex file. Mostly so I can click on
;; the link and open the file. It also sets the default bibliography
(org-add-link-type "bibliography"
		   ;; this code is run on clicking. The bibliography
		   ;; may contain multiple files. this code finds the
		   ;; one you clicked on and opens it. 
		   (lambda (link-string)
		     (save-excursion
		       (beginning-of-line) ; search forward from beginning of the line
		       (search-forward link-string nil t 1)
		       (setq link-string-beginning (match-beginning 0))
		       (setq link-string-end (match-end 0)))
		     ;; now we want to search forward to next comma from point
		     (save-excursion
		       (if (search-forward "," link-string-end 1 1)
			   (setq key-end (- (match-end 0) 1)) ; we found a match
			 (setq key-end (point)))) ; no comma found so take the point
		     ;; and backward to previous comma from point
		     (save-excursion
		       (if (search-backward "," link-string-beginning 1 1)
			   (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
			 (setq key-beginning (point)))) ; no match found
		     ;; save the key we clicked on.
		     (setq bibfile (cite-strip-key (buffer-substring key-beginning key-end)))
		     ;; update the default list for reftex
		     (append 'reftex-default-bibliography bibfile)
		     (find-file bibfile)) ; open file on click
		   ;; formatting code
		   (lambda (keyword desc format)
		     (cond
		      ((eq format 'html) (format "")); no output for html
		      ((eq format 'latex)
		       ;; write out the latex bibliography command
		       (format "\\bibliography{%s}" (replace-regexp-in-string  ".bib" "" keyword))))))

(org-add-link-type "bibliographystyle"
		   (lambda (arg) (message "Nothing implemented for clicking here.")) 
		   (lambda (keyword desc format)
		     (cond
		      ((eq format 'latex)
					; write out the latex bibliography command
		       (format "\\bibliographystyle{%s}" keyword)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ref and label links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO make this work with #+label: or \label
(org-add-link-type
 "ref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (let ((n (count-matches (format "\\label{%s}" label) (point-min) (point-max) t)))
     (if (< n 1) (error (format "no matching label found for \\label{%s}!" label)))
     (if (> n 1) (error (format "%d matches found for %s!" n label)))
   (org-mark-ring-push)
   (goto-char (point-min))
   (re-search-forward (format "\\label{%s}" label))
   (message "go back with `C-c &`")))
 ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\ref{%s}" keyword)))))

(org-add-link-type
 "label"
 (lambda (label)
   "on clicking count the number of label tags used in the buffer. A number greater than one means multiple labels!"
   (count-matches (format "label:%s\\b" label) (point-min) (point-max) t))
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\label{%s}" keyword)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; cite links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemenation of cite:  to make bibtex citations that are also clickable.
; TODO make this work with #+BIBLIOGRAPHY?
; TODO make this use contents of reftex-default-bibliography if not bibliography found
(defun cite-find-bibliography ()
  "find the bibliography file(s) in the buffer. Assumes you use a bibliography link. returns a list of stripped file names."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "bibliography:\\([^\]\|\n]+\\)" nil t)
    (setq cite-bibliography-files
          (mapcar 'cite-strip-key (split-string (match-string 1) ",")))))

(defun cite-goto-bibentry (bibfile key)
  "open bibfile in another window at the key"
  (interactive)
  (find-file-other-window bibfile)
  (goto-char (point-min)) ; always start search from beginning.
  (re-search-forward key nil t 1))

(defun cite-strip-key (key)
  "strip leading and trailing whitespace from the key"
  (interactive)
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" key)))

(defun cite-split-keys (key-string)
  "split key-string and strip keys. Assumes the key-string is comma delimited"
  (mapcar 'citeu-strip-key (split-string key-string ",")))

(defun cite-key-in-file-p (key filename)
  "determine if the key is in the file"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (search-forward key nil t 1)))

(defun cite-onclick (link-string)
  "this function executes when you click on cite link. It identifies the key you clicked on and opens the first bibliography file it finds containing the key."
  ;; First we find the boundaries of the link you clicked on, then
  ;; identify the key you clicked on. First get boundaries of the link-string
  (save-excursion
    (beginning-of-line) ; search forward from beginning of the line
    (search-forward link-string nil t 1)
    (setq link-string-beginning (match-beginning 0))
    (setq link-string-end (match-end 0)))
  ;; now we want to search forward to next comma from point, which defines the end character of the key
  (save-excursion
    (if (search-forward "," link-string-end 1 1)
	(setq key-end (- (match-end 0) 1)) ; we found a match
      (setq key-end (point)))) ; no comma found so take the point
  ;; and backward to previous comma from point which defines the start character
  (save-excursion
    (if (search-backward "," link-string-beginning 1 1)
	(setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
      (setq key-beginning (point)))) ; no match found
  ;; save the key we clicked on.
  (setq bibtex-key (cite-strip-key (buffer-substring key-beginning key-end)))
 
  ;; now we get the bibliography files
  (setq cite-bibliography-files (cite-find-bibliography))
					
  ;; now find the first bib file containing the key
  (setq bib-file (loop for file in cite-bibliography-files do
		       (if (cite-key-in-file-p bibtex-key file) (return file))))
  ;; and finally, open the file at the key
  (cite-goto-bibentry bib-file bibtex-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; cite links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-add-link-type
 "cite"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (concat "\\cite{" 
	     (mapconcat (lambda (key) key) (cite-split-keys keyword) ",")
	     "}")))))

(org-add-link-type
 "citealp"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (concat "\\citealp{" 
	     (mapconcat (lambda (key) key) (cite-split-keys keyword) ",")
	     "}")))))

