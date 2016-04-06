(require 'ob)

(add-to-list 'load-path (expand-file-name "lispy" starter-kit-dir))
(require 'lispy)

;;* hyldoc setup
(require 'le-hy)

(setq inferior-lisp-program "hy")

(defun hy ()
  "Open the hy repl."
  (if ())
  (let ((proc-name "hy"))
    (if (process-live-p proc-name)
        (get-process proc-name)
      (get-buffer-process
       (make-comint proc-name "hy")))))

(defun hy-eldoc-function ()
  "`eldoc-documentation-function' for hy."
  ;; hyldoc returns u'signature', this hacks off the u' and '
  (substring
   (lispy--eval-hy (format "(hyldoc \"%s\")" (lispy--current-function)))
   2 -1))


(defun hy-setup ()
  "Setup hy-mode."
  (lispy-mode 1)
  (add-to-list 'lispy-goto-symbol-alist
	       '(hy-mode lispy-goto-symbol-hy le-hy))
  (set (make-local-variable
	'eldoc-documentation-function)
       'hy-eldoc-function)
  (message "Starting hy...")
  (lispy--hy-proc)
  (message "importing hylp...")
  (lispy--eval-hy "(import [hy.core.hylp [*]])")
  (message ""))


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
    (setq
     result
     (lispy--eval-hy (org-babel-expand-body:hy body params))))))



;; * Use lispy and turn on my eldoc function
(add-hook 'hy-mode-hook 'hy-setup)


(provide 'mile-hy)
