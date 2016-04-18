(require 'ob)

(add-to-list 'load-path (expand-file-name "lispy" starter-kit-dir))
(require 'lispy)

(add-to-list 'load-path (expand-file-name "hy-mode" starter-kit-dir))
(require 'hy-mode)

;;* hyldoc setup
(require 'le-hy)

(setq inferior-lisp-program "/Users/jkitchin/Dropbox/python/hyve/imhy")
(setq hy-mode-inferior-lisp-command "/Users/jkitchin/Dropbox/python/hyve/imhy")

(defun hy-eldoc-function ()
  "`eldoc-documentation-function' for hy."
  ;; hyldoc returns u'signature', this hacks off the u' and '
  (let ((func (lispy--current-function)))
    (cond
     ((s-starts-with? "." func)
      "Can't do .functions yet.")
     ((not (null func))
      (substring
       (lispy--eval-hy (format "(? %s)" func))
       2 -1)))))

;; * auto-complete
(add-to-list 'ac-modes 'hy-mode)

(defun hy-defns-macros ()
  "Get a list of defns in the current file."
  (let ((defns '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(?:defn\\|defmacro\\)[[:space:]]+\\(.*?\\) "nil t)
	(push (match-string 1) defns)))
    defns))

(defun hy-variables ()
  "Collect the variable names in the current buffer.
These are every other name after setv."
  (let ((vars '())
	expr
	set-vars
	let-vars)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(setv" nil t)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (setq expr (read (current-buffer)))
	  (setq set-vars (loop for x in (cdr expr) by #'cddr
			       collect x)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(let" nil t)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (setq expr (read (current-buffer)))
	  ;; this is read as a vector, so we convert to a list.
	  (setq let-vars
		(loop for x in (append (nth 1 expr) nil)
		      by #'cddr collect x)))))
    (append set-vars let-vars)))

(defvar ac-source-hy-keywords
  `((candidates . ,(read (lispy--eval-hy "(hy-all-keywords-emacs-completion)"))))
  "Keywords known from hy. The command is defined in hyve.hylp.")

(defvar ac-source-hy-defns
  '((candidates . hy-defns-macros))
  "Functions/macros defined in the file.")

(defvar ac-source-hy-variables
  '((candidates . hy-variables))
  "Hy variables defined in the file.")

(defun ac-strings ()
  (interactive)
  (message "%s"
	   (append
	    (mapcar (lambda (source)
		      (let ((candidates (cdr (assoc 'candidates source))))
			(if (listp candidates)
			    candidates
			  (funcall candidates))))
		    ac-sources))))
;; * setup

(defun hy-setup ()
  "Setup hy-mode."
  (interactive)
  (lispy-mode 1)
  (add-to-list 'lispy-goto-symbol-alist
	       '(hy-mode lispy-goto-symbol-hy-repl le-hy))
  ;; outline mode
  (setq outline-regexp: ";; ?\\*+"
	orgstruct-heading-prefix-regexp: ";; ?\\*+")
  (orgstruct-mode)

  (set (make-local-variable
	'eldoc-documentation-function)
       'hy-eldoc-function)

  (setq ac-sources '(ac-source-hy-keywords
		     ac-source-hy-defns
		     ac-source-hy-variables))

  (ac-set-trigger-key "TAB")
  (auto-complete-mode 1))


(add-to-list 'org-structure-template-alist
             '("hy"
	       "#+BEGIN_SRC hy\n?\n#+END_SRC"
	       "<src lang=\"hy\">\n?\n</src>"))






;;* ob-hy setup
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("hy" . "hy"))

(defvar org-babel-default-header-args:hy '())
(defvar org-babel-header-args:hy '((:results . "output")))

(defun org-babel-expand-body:hy (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	 (result-params (cdr (assoc :result-params params)))
	 (print-level nil)
	 (print-length nil)
	 (body (org-babel-trim
		(if (> (length vars) 0)
		    (concat "(let ["
			    (mapconcat
			     (lambda (var)
			       (format
				"%S (quote %S)"
				(car var)
				(cdr var)))
			     vars "\n      ")
			    "]\n" body ")")
		  body))))
    (when (not (member "output" result-params))
      (setq body (format "(print (do  %s\n))" body)))
    body))


(defun mile-hy-to-file (body)
  "Save body in temporary file.
Returns filename."
  (let* ((temporary-file-directory ".")
	 (tempfile (make-temp-file "hy-")))
    (with-temp-file tempfile
      (insert body))
    tempfile))


(defun org-babel-execute:hy (body params)
  "Execute a block of hy code with Babel.
:results body - the expanded body of code
:results python - The hy code converted to python by hy2py
:results ast - The AST code hy is converted to
:results value - wraps the body in (print (do %s))
:results output - captures the output
"

  (cond
   ((member "body" result-params)
    (setq result body))

   ((member "python" result-params)
    (unwind-protect
	(let ((tempfile (mile-hy-to-file body)))
	  (setq result (shell-command-to-string
			(format "hy2py %s" tempfile)))
	  (delete-file tempfile))))

   ((member "ast" result-params)
    (unwind-protect
	(let ((tempfile (mile-hy-to-file body)))
	  (setq result (shell-command-to-string
			(format "hy2py -a -np %s" tempfile)))
	  (delete-file tempfile))))
   (t
    (cond
     ((assoc :repl params)
      (setq
       result
       (progn
	 ;; Here we make sure the repl is set to where we run from. I think this
	 ;; is the right thing to do.
	 (lispy--eval-hy "(import os)")
	 (lispy--eval-hy (format  "(os.chdir \"%s\")" default-directory))
	 (lispy--eval-hy (org-babel-expand-body:hy body params)))))
     (t
      (let ((tempfile (mile-hy-to-file body)))
	(setq result (shell-command-to-string
		      (format "hy %s" tempfile)))
	(delete-file tempfile))))))
  result)



;; * Use lispy and turn on my eldoc function
(add-hook 'hy-mode-hook 'hy-setup)


(provide 'mile-hy)
