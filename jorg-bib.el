;; Lisp code to setup bibliographycite, ref and label org-mode links.
;; also sets up reftex for org-mode

(defgroup jorg-bib nil
  "customization group for jorg-bib")

(defcustom jorg-bib-bibliography-notes
  nil
  "filename to where you will put all your notes about an entry in
  the default bibliography."
  :group 'jorg-bib)

(defcustom jorg-bib-default-bibliography
  nil
  "list of bibtex files to search for."
  :group 'jorg-bib)

(defcustom jorg-bib-pdf-directory
  nil
  "directory where pdfs are stored by key. put a trailing / in"
  :group 'jorg-bib)


;; variables that control bibtex key format for auto-generation
;; I want firstauthor-year-title-words
;; this usually makes a legitimate filename to store pdfs under.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
	 (global-auto-revert-mode t)
         (reftex-parse-all))
    (make-local-variable 'reftex-cite-format)
    (setq reftex-cite-format 'org)
    (define-key org-mode-map (kbd "C-c ]") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(eval-after-load 'reftex-vars
  '(progn
      (add-to-list 'reftex-cite-format-builtin
                   '(org "Org-mode citation"
                         ((?\C-m . "cite:%l"))))))

(defun jorg-bib/upload-bibtex-entry-to-citeulike ()
  "with point in  a bibtex entry get bibtex string and submit to citeulike.

Relies on the python script /upload_bibtex_citeulike.py being in the personal prelude directory."
  (interactive)
  (message "uploading to citeulike")
  (save-restriction
    (bibtex-narrow-to-entry)
    (let ((startpos (point-min))
          (endpos (point-max))
          (bibtex-string (buffer-string))
          (script (concat "python " prelude-personal-dir "/upload_bibtex_citeulike.py&")))
      (with-temp-buffer (insert bibtex-string)
                        (shell-command-on-region (point-min) (point-max) script t nil nil t)))))

(defun jorg-bib-open-bibtex-pdf ()
  "open pdf for a bibtex entry, if it exists. assumes point is in
the entry of interest. but does not check that."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "=key=" entry))
           (pdf (format (concat jorg-bib-pdf-directory "%s.pdf") key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
          (org-open-link-from-string (format "[[file:%s]]" pdf))
        (ding)))))

(require 'reftex-cite)

(defun jorg-bib-open-bibtex-notes ()
  "from a bibtex entry, open the notes if they exist, and create a heading if they do not.

I never did figure out how to use reftex to make this happen
non-interactively. the reftex-format-citation function did not
work perfectly; there were carriage returns in the strings, and
it did not put the key where it needed to be. so, below I replace
the carriage returns and extra spaces with a single space and
construct the heading by hand."
  (interactive)
  (if (eq major-mode 'bibtex-mode)
      (progn
        (bibtex-beginning-of-entry)
        (let* ((cb (current-buffer))
               (bibtex-expand-strings t)
               (entry (bibtex-parse-entry t))
               (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
               (year  (reftex-get-bib-field "year" entry))
               (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
               (key (reftex-get-bib-field "=key=" entry))
               (journal (reftex-get-bib-field "journal" entry))
               (volume (reftex-get-bib-field "volume" entry))
               (pages (reftex-get-bib-field "pages" entry))
               (doi (reftex-get-bib-field "doi" entry))
               (url (reftex-get-bib-field "url" entry))
               )

          ;; now look for entry
          (find-file jorg-bib-bibliography-notes)

          (goto-char (point-min))
          ;; put new entry in notes if we don't find it.
          (unless (re-search-forward (format ":Custom_ID: %s$" key) nil 'end)
            (insert (format "\n** TODO %s - %s" year title))
            (insert (format"
 :PROPERTIES:
  :Custom_ID: %s
  :AUTHOR: %s
  :JOURNAL: %s
  :YEAR: %s
  :VOLUME: %s
  :PAGES: %s
  :DOI: %s
  :URL: %s
 :END:
[[cite:%s]] [[file:%s/%s.pdf][pdf]]\n\n"
key author journal year volume pages doi url key jorg-bib-pdf-directory key )))))))


(defun jorg-bib-open-in-browser ()
  "Open the bibtex entry at point in a browser using the url field or doi field"
(interactive)
(save-excursion
  (bibtex-beginning-of-entry)
  (catch 'done
    (let ((url (bibtex-autokey-get-field "url")))
      (when  url
        (browse-url url)
        (throw 'done nil)))

    (let ((doi (bibtex-autokey-get-field "doi")))
      (when doi
        (if (string-match "^http" doi)
            (browse-url doi)
          (browse-url (format "http://dx.doi.org/%s" doi)))
        (throw 'done nil)))

    (message "No url or doi found"))))

(global-set-key [f10] 'jorg-bib-open-bibtex-notes)
(global-set-key [f11] 'jorg-bib-open-bibtex-pdf)
(global-set-key [f12] 'jorg-bib-open-in-browser)

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
		     (message (format "link-string = %s" link-string))
		     (save-excursion
		       ;; get link-string boundaries
		       ;; we have to go to the beginning of the line, and then search forwardq
		       (beginning-of-visual-line)
		       (search-forward link-string nil nil 1)
		       (setq link-string-beginning (match-beginning 0))
		       (setq link-string-end (match-end 0)))
		     (message (format "link found %s "(buffer-substring link-string-beginning link-string-end)))
		     ;; now if we have comma separated bibliographies
		     ;; we find the one clicked on. we want to
		     ;; search forward to next comma from point
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
		     (message (format "bibfile = %s" bibfile))
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
		       ;; write out the latex bibliography command
		       (format "\\bibliographystyle{%s}" keyword)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ref and label links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO make this work with #+label: or \label



(org-add-link-type
 "ref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   ;(let ((n (count-matches (format "\\label{%s}" label) (point-min) (point-max) t)))
   ;  (if (< n 1) (error (format "no matching label found for \\label{%s}!" label)))
   ;  (if (> n 1) (error (format "%d matches found for %s!" n label)))
   (org-mark-ring-push)
   ;; next search from beginning of the buffer
   (goto-char (point-min))
   (or (re-search-forward (format "label:%s" label) nil t)
       (re-search-forward (format "\\label{%s}" label) nil t)
       (re-search-forward (format "#\\+label:%s" label) nil t))
   (message "go back with `C-c &`"))
 ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<ref>%s</ref>)" path))
    ((eq format 'latex)
     (format "\\ref{%s}" keyword)))))

(org-add-link-type
 "label"
 (lambda (label)
   "on clicking count the number of label tags used in the buffer. A number greater than one means multiple labels!"
   (message (format "%s occurences"
		    (+ (count-matches (format "label:%s\\b" label) (point-min) (point-max) t)
		       (count-matches (format "\\label{%s}\\b" label) (point-min) (point-max) t)
		       (count-matches (format "#\\+label:%s\\b" label) (point-min) (point-max) t)))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "(<label>%s</label>)" path))
    ((eq format 'latex)
     (format "\\label{%s}" keyword)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; cite links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemenation of cite:  to make bibtex citations that are also clickable.
; TODO make this use contents of reftex-default-bibliography if not bibliography found
(defun cite-find-bibliography ()
  "find the bibliography in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;;  look for a bibliography link
    (re-search-forward "bibliography:\\([^\]\|\n]+\\)" nil t)
    (if (match-string 1) ; we found a link
	(setq cite-bibliography-files
	      (mapcar 'cite-strip-key (split-string (match-string 1) ",")))
      (progn ;we did not find a bibliography link. now look for \bibliography
	(message "no bibliography link found")
	(goto-char (point-min))
	(re-search-forward "\\bibliography{\\([^\]\|\n]+\\)}" nil t)
	(if (match-string 1) ; we found a link
	    (setq cite-bibliography-files
		  (mapcar 'cite-strip-key (split-string (match-string 1) ",")))
	 ; we did not find a raw latex bibliography. look for bibitems
	 (progn
	   (message "no \\bibliography found")
	   (goto-char (point-min))
	   (re-search-forward "\\(bibitem\\)" nil t)
	   (if (match-string 1) (setq cite-bibliography-files "internal")
             ; no internal bibliography found. now we use the default setting
             ; from the user
             (progn
               (message "no bibitems found")
               (setq cite-bibliography-files jorg-bib-default-bibliography)
               (message "cite-bibliography-files = %s" cite-bibliography-files))))))))
  (message "cite-bibliography-files = %s" cite-bibliography-files)
  cite-bibliography-files)

(defun cite-goto-bibentry (bibfile key)
  "goto the key, in another window if needed."
  (interactive)
  (message "cite-goto-bibentry key=%s | bibfile=%s" key bibfile)
  (if (and bibfile (not (equal bibfile "internal")))
      (find-file-other-window bibfile))
  (org-mark-ring-push)
  (goto-char (point-min)) ; always start search from beginning.
  (message (format "searching for {%s}" key))
  (set-text-properties 0 (length key) nil key)
  (search-forward (format "{%s," key) nil nil 1)
  (message "go back with `C-c &`"))

(defun cite-strip-key (key)
  "strip leading and trailing whitespace from the key"
  (interactive)
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" key)))

(defun cite-split-keys (key-string)
  "split key-string and strip keys. Assumes the key-string is comma delimited"
  (mapcar 'cite-strip-key (split-string key-string ",")))

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
  (message "\n\nyou clicked on %s" link-string)
  (message "%s" last-input-event)
  (save-excursion
    (search-backward "cite:")
    (search-forward link-string nil t 1)
    (setq link-string-beginning (match-beginning 0))
    (setq link-string-end (match-end 0)))
  ;; now we want to search forward to next comma from point, which
  ;; defines the end character of the key
  (save-excursion
    (if (search-forward "," link-string-end t 1)
	(setq key-end (- (match-end 0) 1)) ; we found a match
      (setq key-end link-string-end))) ; no comma found so take the end
  ;; and backward to previous comma from point which defines the start character
  (save-excursion
    (if (search-backward "," link-string-beginning 1 1)
	(setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
      (setq key-beginning link-string-beginning))) ; no match found
  ;; save the key we clicked on.
  (setq bibtex-key (cite-strip-key (buffer-substring key-beginning key-end)))
  (message (format "found bibtex key: %s" bibtex-key))
  (message "%s" last-input-event)
  (let ((button (car last-input-event)))
    (cond ((eq button 'mouse-2)
           ;; now we get the bibliography files
           (setq cite-bibliography-files (cite-find-bibliography))
           (message "cite-bibliography-files = %s" cite-bibliography-files)
           ;; now find the first bib file containing the key if it is a file
           (if (not (equal cite-bibliography-files "internal"))
               (progn
                 (message "checking files")
                 (setq bib-file (loop for file in cite-bibliography-files do
                                      (if (cite-key-in-file-p bibtex-key file) (return file))))))
           ;; and finally, open the file at the key
           (cite-goto-bibentry bib-file bibtex-key)
           (recenter-top-bottom 1))

          ;; if you right clicked, open the pdf. point is in the bibtex entry.
          ((eq button 'mouse-3)
               (let ((pdf (format (concat jorg-bib-pdf-directory "%s.pdf") bibtex-key)))
                 (if (file-exists-p pdf)
                     (org-open-link-from-string (format "[[file:%s]]" pdf))
                   (message "%s not found" pdf)
                   (ding)))))))



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
    ((eq format 'html) (format "(<citealp>%s</citealp>)" path))
    ((eq format 'latex)
     (concat "\\citealp{"
	     (mapconcat (lambda (key) key) (cite-split-keys keyword) ",")
	     "}")))))

(org-add-link-type
 "citet"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citet{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citet*"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citet*{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

;; TODO these links do not support options [see][]
(org-add-link-type
 "citep"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citep{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citep*"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citep*{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citeauthor"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeauthor{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citeauthor*"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeauthor*{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "citeyear"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\citeyear{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

(org-add-link-type
 "nocite"
 'cite-onclick
 ;; formatting
 (lambda (keyword desc format)
   (cond
((eq format 'html) (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
  (concat "\\nocite{" (mapconcat (lambda (key) key) (cite-split-keys keyword) ",") "}")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; index links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-add-link-type
 "index"
 () ; clicks do nothing.
 (lambda (keyword desc format)
   (cond
((eq format 'html)
     (format ""))
    ((eq format 'latex)
     (format "\\index{%s}" keyword)))))

(provide 'jorg-bib)
;;; jorg-bib.el ends here
