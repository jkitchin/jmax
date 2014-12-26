;;; pydoc.el --- pydoc - functional, syntax highlighted python documentation
;; Copyright (C) 2014
;;
;; Author: John Kitchin <jkitchin@andrew.cmu.edu>

;;; Commentary:
;; This module runs pydoc on an argument, inserts the output into a
;; buffer, and then linkifies and colorizes the buffer. For example,
;; some things are linked to open the source code, or to run pydoc on
;; them. Some things are colorized for readability, e.g. environment
;; variables and strings, function names and arguments.
;;
;; https://github.com/jkitchin/jmax/blob/master/pydoc.el
;;
;; There is one command. M-x pydoc

;;; Code:

(defvar *pydoc-current* nil
 "Stores current pydoc command.")


(defvar *pydoc-last* nil
 "Stores the last pydoc command.")


(defun pydoc-make-file-link ()
  "Find FILE in a pydoc buffer and make it a clickable link that
opens the file."
  (goto-char (point-min))
  (when (re-search-forward "^FILE
    \\(.*\\)$" nil t)
    
    (setq pydoc-file (match-string 1))
    
    (let ((map (make-sparse-keymap))
	  (start (match-beginning 1))
	  (end (match-end 1)))
      
      ;; set file to be clickable to open the source
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (find-file ,pydoc-file)
	  (goto-char (point-min))))

      (set-text-properties
       start end
       `(local-map, map
		   font-lock-face (:foreground "blue"  :underline t)
		   mouse-face highlight
		   help-echo "mouse-1: click to open")))))


(defun pydoc-make-url-links ()
  "Propertize urls so they are clickable."
  (goto-char (point-min))

  (while (re-search-forward "http" nil t)
    ;; this seems like a clumsy way to set this link, but it works.
    (let ((url (browse-url-url-at-point)))      
      (re-search-backward "http")
      (re-search-forward url))
    
    (let ((map (make-sparse-keymap))
	  (start (match-beginning 0))
	  (end (match-end 0)))
	
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
  (goto-char (point-min))
  (when (re-search-forward "^NAME
    \\([a-zA-Z0-9_]*\\(\\..*\\)?\\) -"
			   nil t)
    (setq pydoc-name (match-string 1))))


(defun pydoc-make-package-links ()
  "Make links in PACKAGE CONTENTS."
  (goto-char (point-min))
  (when (re-search-forward "^PACKAGE CONTENTS" nil t)
    (forward-line)

    (while (string-match
	    "^    \\([a-zA-Z0-9_-]*\\)[ ]?\\((package)\\)?"
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


(defun pydoc-colorize-class-methods ()
  (goto-char (point-min))
  ;; group1 is the method, group2 is the args
  (while (re-search-forward "     |  \\([a-zA-Z0-9_]*\\)(\\(.*\\))" nil t)

    (let ((map (make-sparse-keymap))
	  (start (match-beginning 1))
	  (end (match-end 1))
	  (function (match-string 1)))
		
      (define-key map [mouse-1]
	`(lambda ()
	   (interactive)
	   (find-file ,pydoc-file)
	   (goto-char (point-min))
	   (re-search-forward
	    ;; fragile if spacing is not right
	    (format "def %s(" ,function nil t))))
      
      (set-text-properties
       start end
       `(local-map, map
		    font-lock-face (:foreground "brown")
		    mouse-face highlight
		    help-echo (format "mouse-1: click to open %s" ,function)))
      
      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       '(font-lock-face (:foreground "red"))))))


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
	     (goto-char (point-min))
	     (re-search-forward
	      (format "def %s(" ,function nil t))))

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


(defun pydoc-linkify-sphinx-directives ()
  "Make sphinx directives into clickable links.

class, func and mod directive links will run pydoc on the link contents.

we just colorize parameters in red."

  (goto-char (point-min))
  (while (re-search-forward ":\\(class\\|func\\|mod\\):`\\([^`]*\\)`" nil t)
    (let ((map (make-sparse-keymap)))    
      ;; we run pydoc on the func
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
		    help-echo
		    (format "mouse-1: pydoc %s" ,(match-string 1))))))

  (goto-char (point-min))
  ;; param, parameter, arg, argument, key, keyword
  (while (re-search-forward
	  (concat
	   ":\\(param\\|parameter\\|arg\\|argument\\|key\\|keyword\\):"
	   "`\\([^`]*\\)`")
	  nil t)
    (set-text-properties
     (match-beginning 2)
     (match-end 2)
     '(font-lock-face (:foreground "red"))))

  ;; :param type name:
  (goto-char (point-min))
  (while (re-search-forward
	  ":param\\s-*\\([^: ]*\\)\\s-*\\([^:]*\\):"
	  nil t)

    (cond
     ;; neither present
     ((and (string= "" (match-string 1))
	   (string= "" (match-string 2)))
      ;; pass
      )
     ;; no type and one arg.
     ((and (not (string= "" (match-string 1)))
	   (string= "" (match-string 2)))
      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       '(font-lock-face (:foreground "red"))))
     ;; both type and arg
     (t
      ;; optional type
      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       '(font-lock-face (:foreground "DeepSkyBlue3")))
      
      ;; arg
      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       '(font-lock-face (:foreground "red")))))))


(defun pydoc-fontify-inline-code ()
  "fontify lines with >>> in them, which are inline python."
  (goto-char (point-min))
  (while (re-search-forward "\\(\\.\\.\\.\\|>>>\\)" nil t)
    (org-src-font-lock-fontify-block
     "python"
     (line-beginning-position)
     (line-end-position))))

    
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
	   (goto-char (point-min))
	   ;; this might be fragile if people put other spaces in
	   (re-search-forward (format "class %s("  ,(match-string 1)))))

      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       `(local-map, map
		    font-lock-face (:foreground "SteelBlue3")
		    mouse-face highlight
		    help-echo "mouse-1: click to open")))

    ;; colorize and link superclass
    (let ((map (make-sparse-keymap)))
    
      ;; we run pydoc on the superclass
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
		    help-echo
		    (format "mouse-1: pydoc %s" ,(match-string 2)))))))


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

  (make-variable-buffer-local 'pydoc-file)
  (make-variable-buffer-local 'pydoc-name)

  (save-excursion
    (pydoc-get-name)
    (pydoc-make-url-links)
    (pydoc-make-file-link)
    (pydoc-make-package-links)
    (pydoc-linkify-classes)
    (pydoc-colorize-functions)
    (pydoc-colorize-class-methods)
    (pydoc-colorize-envvars)
    (pydoc-colorize-strings)
    (pydoc-linkify-sphinx-directives)
    (pydoc-fontify-inline-code)
    (pydoc-insert-back-link))

  ;; make read-only and press q to quit. add some navigation keys
  (setq buffer-read-only t)
  (use-local-map (copy-keymap org-mode-map))
  (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))
  (local-set-key "n" #'(lambda () (interactive) (next-line)))
  (local-set-key "N" #'(lambda () (interactive) (forward-page)))
  (local-set-key "p" #'(lambda () (interactive) (previous-line)))
  (local-set-key "P" #'(lambda () (interactive) (backward-page)))
  (local-set-key "f" #'(lambda () (interactive) (forward-char)))
  (local-set-key "b" #'(lambda () (interactive) (backward-char)))
  (local-set-key "F" #'(lambda () (interactive) (forward-word)))
  (local-set-key "B" #'(lambda () (interactive) (backward-word)))
  (local-set-key "o" #'(lambda () (interactive) (call-interactively 'occur)))
  (local-set-key "s" #'(lambda () (interactive) (isearch-forward)))
  (font-lock-mode))

(provide 'pydoc)

;;; pydoc.el ends here
