;;; jmax-utils.el --- Utility functions in jmax

;;; Commentary:
;;

;;; Code:

(defun jmax-help ()
  "Open the ‘org-ref’ manual."
  (interactive)
  (find-file (expand-file-name
              "jmax.org"
	      starter-kit-dir)))


(defun kill-all-buffers ()
  "Kill all buffers.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))


(defun kill-other-buffers ()
    "Kill all other buffers but this one.  Leave one frame open."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer) (buffer-list)))
    (delete-other-windows))


(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))


(defun double-space ()
  "Make buffer look approximately double-spaced."
  (interactive)
  (setq line-spacing 10))


(defun single-space ()
  "Make buffer single-spaced."
  (interactive)
  (setq line-spacing nil))

;;* utility functions
;http://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#File-Name-Expansion

(defun get-path()
  "Opens dired so you can navigate to a file to insert a path to it in the current buffer."
  (interactive)
  ; store current point so we can change back to it later
  (setq current_point (point-marker))
  ; now call dired to navigate to the path you want
  (dired nil))


(defun insert-relative-path()
  "Inserts the relative path between the original buffer and current file selected in dired."
  (interactive)
  (let ((selected_file (dired-get-filename)))
    (switch-to-buffer (marker-buffer current_point))
    (goto-char current_point)
    (insert (file-relative-name selected_file))))


(defun insert-absolute-path()
  "Inserts the absolute path to the file selected in dired to the previous buffer."
  (interactive)
  (let ((selected_file (dired-get-filename))) ; this is the file the cursor is on
    (switch-to-buffer (marker-buffer current_point))
    (goto-char current_point)
    (insert  (expand-file-name selected_file))))


(defun insert-path (&optional arg)
  "Insert relative path unless prefix is used, then absolute path"
  (interactive "P")
  (if (equal arg nil)
      (insert-relative-path)
    (insert-absolute-path)))


(defun insert-buffer-filename()
  "Inserts filename associated with current buffer."
  (interactive)
  (insert (buffer-file-name)))


;;* markup commands for org-mode
(loop for (type beginning-marker end-marker)
      in '((subscript "_{" "}")
	   (superscript "^{" "}")
	   (italics "/" "/")
	   (bold "*" "*")
	   (verbatim "=" "=")
	   (code "~" "~")
	   (underline "_" "_")
	   (strikethrough "+" "+"))
      do
      (eval `(defun ,(intern (format "%s-region-or-point" type)) ()
	       ,(format "%s the region or character at point"
			(upcase (symbol-name type)))
	       (interactive)
	       (if (region-active-p)
		   (progn
		     (goto-char (region-end))
		     (insert ,end-marker)
		     (goto-char (region-beginning))
		     (insert ,beginning-marker)
		     (re-search-forward (regexp-quote ,end-marker))
		     (goto-char (match-end 0)))
		 (insert ,(concat beginning-marker end-marker))
		 (backward-char ,(length end-marker))))))


(defun latex-math-region-or-point (&optional arg)
  "Wrap the selected region in latex math markup.
\(\) or $$ (with prefix ARG) or @@latex:@@ with double prefix.
Or insert those and put point in the middle to add an equation."
  (interactive "P")
  (let ((chars
	 (cond
	  ((null arg)
	   '("\\(" . "\\)"))
	  ((equal arg '(4))
	   '("$" . "$"))
	  ((equal arg '(16))
	   '("@@latex:" . "@@")))))
    (if (region-active-p)
	(progn
	  (goto-char (region-end))
	  (insert (cdr chars))
	  (goto-char (region-beginning))
	  (insert (car chars)))
      (insert (concat  (car chars) (cdr chars)))
      (backward-char (length (cdr chars))))))


(define-key org-mode-map (kbd "s--") 'subscript-region-or-point)
(define-key org-mode-map (kbd "s-=") 'superscript-region-or-point)
(define-key org-mode-map (kbd "s-i") 'italics-region-or-point)
(define-key org-mode-map (kbd "s-b") 'bold-region-or-point)
(define-key org-mode-map (kbd "s-v") 'verbatim-region-or-point)
(define-key org-mode-map (kbd "s-c") 'code-region-or-point)
(define-key org-mode-map (kbd "s-u") 'underline-region-or-point)
(define-key org-mode-map (kbd "s-+") 'strikethrough-region-or-point)
(define-key org-mode-map (kbd "s-4") 'latex-math-region-or-point)


(provide 'jmax-utils)

;;; jmax-utils.el ends here
