;;; pydoc.el --- pydoc - functional, syntax highlighted python documentation


;;; Commentary:
;; 

;;; Code:

(defun pydoc-make-file-link ()
  "Find FILE in a pydoc buffer and make it a clickable link."
  (goto-char (point-min))
  (when (re-search-forward "^FILE
    \\(.*\\)$" nil t)

    (make-variable-buffer-local 'pydoc-file)
    (setq pydoc-file (match-string 1))
    

    (let ((map (make-sparse-keymap))
	  (start (match-beginning 1))
	  (end (match-end 1)))
      
      ;; set file to be clickable to open the source
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (find-file ,pydoc-file)))

      (set-text-properties
       start end
       `(local-map, map
		   font-lock-face (:foreground "blue"  :underline t)
		   mouse-face highlight
		   help-echo "mouse-1: click to open")))))


(defun pydoc-make-url-links ()
  "Propertize urls so they are clickable."
  (goto-char (point-min))
  (while (re-search-forward "\\(http\\(s\\)?://.*$\\)" nil t)
    (let ((map (make-sparse-keymap))
	  (start (match-beginning 1))
	  (end (match-end 1)))
	
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (browse-url ,(buffer-substring start end))))
	
      (set-text-properties
       start end
       `(local-map ,map
		   font-lock-face (:foreground "blue"  :underline t)
		   mouse-face highlight
		   help-echo (format "mouse-1: click to open"))))))


(defun pydoc-get-name ()
  "Get NAME and store locally."
  (make-variable-buffer-local 'pydoc-name)
  (goto-char (point-min))
  (when (re-search-forward "^NAME
\\s-*\\([^-][a-zA-Z]*\\)" nil t)
    (setq pydoc-name (match-string 1))))


(defun pydoc-make-package-links ()
  "Make links in PACKAGE CONTENTS."
  (goto-char (point-min))
  (when (re-search-forward "^PACKAGE CONTENTS" nil t)
    (forward-line)

    (while (string-match
	    "^    \\([a-zA-Z0-9_]*\\)[ ]?\\((package)\\)?"
	    (buffer-substring
	     (line-beginning-position)
	     (line-end-position)))
		
      (let ((map (make-sparse-keymap))
	    (start (match-beginning 1))
	    (end (match-end 1))
	    (package (concat
		      pydoc-name "."
		      (match-string 1
				    (buffer-substring
				     (line-beginning-position)
				     (line-end-position))))))
	
	(define-key map [mouse-1]
	  `(lambda ()
	    (interactive)
	    (pydoc ,package)))
	  
	(set-text-properties
	 (+ (line-beginning-position) start)
	 (+ (line-beginning-position) end)
	 `(local-map, map
		      font-lock-face (:foreground "blue"  :underline t)
		      mouse-face highlight
		      help-echo (format "mouse-1: click to open %s" ,package))))
      (forward-line))))

(defun pydoc-colorize-functions ()
  "Change color of function names and args.
Also, make function names clickable so they open the source file
at the function definition."
  (goto-char (point-min))
  (when (re-search-forward "^Functions" nil t)
    ;; we use this regexp to find functions "    name(args)"
    (while (re-search-forward "    \\([a-zA-z0-9-]+\\)(\\([^)]*\\))" nil t)

      (let ((map (make-sparse-keymap))
	    (start (match-beginning 1))
	    (end (match-end 1))
	    (function (match-string 1)))
		
	(define-key map [mouse-1]
	  `(lambda ()
	     (interactive)
	     (find-file ,pydoc-file)
	     (re-search-forward
	      (format "%s" ,function nil t))))

	(set-text-properties
	 start end
	 `(local-map, map
		     font-lock-face (:foreground "brown")
		     mouse-face highlight
		     help-echo (format "mouse-1: click to open %s" ,function)))

	(set-text-properties
	 (match-beginning 2)
	 (match-end 2)
	 '(font-lock-face (:foreground "red")))))))

(defun pydoc-colorize-envvars ()
  "Makes environment variables a green color"
  (goto-char (point-min))
  (while (re-search-forward "\\$[a-zA-Z_]*\\>" nil t)
    (set-text-properties
     (match-beginning 0)
     (match-end 0)
     '(font-lock-face (:foreground "forest green")))))


(defun pydoc-colorize-strings ()
  "Make strings in single ' or \" a green color.
This is not very robust."
  (goto-char (point-min))
  (while (re-search-forward
	  (concat "\\('\\|\"\\)" ; opening quote
		  "[^'\\|\"\\|\\n]*"  ; chars that are not a quote or line ending
		  "\\('\\|\"\\)"); closing quote
		  nil t)
    (set-text-properties
     (match-beginning 0)
     (match-end 0)
     '(font-lock-face (:foreground "forest green")))))


(defun pydoc-linkify-classes ()
  "TODO: find class lines, and linkify them"
  (goto-char (point-min))
  ;; first match is class name, second match is super class
  (while (re-search-forward "    class \\(.*\\)(\\(.*\\))$" nil t)
    ;; colorize the class
    (let ((map (make-sparse-keymap)))
    
      ;; set file to be clickable to open the source
      (define-key map [mouse-1]
	`(lambda ()
	   (interactive)
	   (find-file ,pydoc-file)
	   (re-search-forward (format "class[ ]*%s[ ]+("  ,(match-string 1)))))

      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       `(local-map, map
		    font-lock-face (:foreground "SteelBlue3")
		    mouse-face highlight
		    help-echo "mouse-1: click to open")))

    ;; colorize and link superclass
    (let ((map (make-sparse-keymap)))
    
      ;; set file to be clickable to open the source
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (pydoc ,(match-string 2))))

      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       `(local-map, map
		    font-lock-face (:foreground "SteelBlue4"  :underline t)
		    mouse-face highlight
		    help-echo "mouse-1: click to open")))))


(defun pydoc-insert-back-link ()
  "Insert link to previous buffer."
  (goto-char (point-max))
  (insert "
[back]")
  (let ((map (make-sparse-keymap)))
    
    ;; set file to be clickable to open the source
    (define-key map [mouse-1]
      (lambda ()
	(interactive)
        (pydoc *pydoc-last*)))

      (set-text-properties
       (line-beginning-position)
       (line-end-position)
       `(local-map, map
		    font-lock-face (:foreground "blue"  :underline t)
		    mouse-face highlight
		    help-echo "mouse-1: click to return"))))

(defvar *pydoc-current* nil
 "Stores current pydoc command.")

(defvar *pydoc-last* nil
 "Stores the last pydoc command.")

(defun pydoc (name)
  "Display pydoc information for NAME in a buffer named *pydoc*."
  (interactive "sName of function or module: ")

  (switch-to-buffer-other-window "*pydoc*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (shell-command-to-string (format "python -m pydoc %s" name)))
  (goto-char (point-min))

  ;; save
  (when *pydoc-current*
      (setq *pydoc-last* *pydoc-current*))
  (setq *pydoc-current* name)


  (save-excursion
    (pydoc-get-name)
    (pydoc-make-url-links)
    (pydoc-make-file-link)
    (pydoc-make-package-links)
    (pydoc-linkify-classes)
    (pydoc-colorize-functions)
    (pydoc-colorize-envvars)
    (pydoc-colorize-strings)
    (pydoc-insert-back-link))

  ;; make read-only and press q to quit
  (setq buffer-read-only t)
  (use-local-map (copy-keymap org-mode-map))
  (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))

  (font-lock-mode))

(provide 'pydoc)

;;; pydoc.el ends here
