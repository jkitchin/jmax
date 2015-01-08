;;; python-setup.el --- jmax python setup code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; python customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try the launch pad python-mode
;;; Code:

(add-to-list 'load-path (expand-file-name "python-mode" starter-kit-dir))
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


(setq python-indent-offset 4)

;; turn off yasnippets in python mode
(add-hook 'python-mode-hook #'(lambda () (yas-global-mode -1)))

;; I want python to always show me the output. this advice makes that happen.
(defadvice python-shell-send-buffer (after switch-to-python-output activate)
  "Show python output in another frame after you run a script."
  (switch-to-buffer-other-frame "*Python*"))


(elpy-enable)

;; this is my pydoc

;;; Commentary:
;;

(require 'pydoc)

;; org-mode links for python
(org-add-link-type "func"
		   (lambda (path) (pydoc path)))

;; mod:ase.Atoms
(org-add-link-type "mod"
		   (lambda (path) (pydoc path)))



(defvar jmax-pylint-options
  '(
    "-r no "  ; no reports
    ;; we are not usually writing programs where it
    ;; makes sense to be too formal on variable
    ;; names.
    "--disable=invalid-name "
    ;; don't usually have modules, which triggers
    ;; this when there is not string at the top
    "--disable=missing-docstring "
    ;; superfluous-parens is raised with print(),
    ;; which I am promoting for python3
    ;; compatibility.
    "--disable=superfluous-parens ";

    ;; these do not seem important for my work.
    "--disable=too-many-locals ";

    ;; this is raised in solving odes and is
    ;; unimportant for us.
    "--disable=unused-argument ";
    "--disable=unused-wildcard-import "
    "--disable=redefined-outer-name "
    ;; this is triggered a lot from fsolve
    "--disable=unbalanced-tuple-unpacking "
    ;; these are really annoying with how we use jasp
    "--disable=wildcard-import "
    "--disable=redefined-builtin "
    ;; I dont mind semicolon separated lines
    "--disable=multiple-statements "
    ;; pylint picks up np.linspace as a no-member error. That does not make sense.
    "--disable=no-member "
    )
  "List of options to use with pylint.")

(defun org-py-check ()
  "Run python check programs on a source block.
Opens a buffer with links to what is found. This function installs pyflakes, pep8 and pylint if needed."
  (interactive)
  (let ((eop (org-element-at-point))
	(temporary-file-directory ".")
        (cb (current-buffer))
	(n) ; for line number
	(cn) ; column number
	(content) ; error on line
	(pb "*org pycheck*")
	(pyflakes-status nil)
	(link)
	(tempfile))

    (unless (executable-find "pyflakes")
      (if (executable-find "pip")
	  (shell-command "python -c \"import pip; pip.main(['install','pyflakes'])\"")
	(shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['-U','pyflakes'])\"")))

    (unless (executable-find "pep8")
      (if (executable-find "pip")
	  (shell-command "python -c \"import pip; pip.main(['install','pep8'])\"")
	(shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['-U','pep8'])\"")))

    (unless (executable-find "pylint")
      (if (executable-find "pip")
	  (shell-command "python -c \"import pip; pip.main(['install','pylint'])\"")
	(shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['pylint'])\"")))

    ;; rm buffer if it exists
    (when (get-buffer pb) (kill-buffer pb))

    ;; only run if in a python code-block
    (when (and (eq 'src-block (car eop))
	       (string= "python" (org-element-property :language eop)))

      ;; tempfile for the code
      (setq tempfile (make-temp-file "org-py-check" nil ".py"))
      ;; create code file
      (with-temp-file tempfile
	(insert (org-element-property :value eop)))

      ;;       ;; pyflakes section

      ;;       (let ((status (shell-command
      ;;		     (format "pyflakes %s" (file-name-nondirectory tempfile))))
      ;;	    (output (delete "" (split-string
      ;;				(with-current-buffer "*Shell Command Output*"
      ;;				  (buffer-string)) "\n"))))
      ;;	(setq pyflakes-status status)
      ;;	(kill-buffer "*Shell Command Output*")
      ;;	(when output
      ;;	  (set-buffer (get-buffer-create pb))
      ;;	  (insert (format "\n* pyflakes output (status=%s)
      ;; pyflakes checks your code for errors. You should probably fix all of these.

      ;; " status))
      ;;	  (dolist (line output)
      ;;	    ;; get the line number
      ;;	    (cond
      ;;	     ;; this works on my Mac with pyflakes v0.8.1
      ;;	     ;; file.py:1: mesg
      ;;	     ((string-match (format "^%s:\\([0-9]*\\):\\(.*\\)"
      ;;				    (file-name-nondirectory tempfile))
      ;;			    line)
      ;;	      (setq n (match-string 1 line))
      ;;	      (setq content (match-string 2 line))
      ;;	      (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s))][%s]] %s\n"
      ;;				 cb
      ;;				 (org-element-property :begin eop)
      ;;				 n
      ;;				 (format "Line %s:" n)
      ;;				 content)))
      ;;	     ;; Sometimes there is a column number
      ;;	     ;; it seems to be for certain kinds of errors
      ;;	     ;; file.py:1(6): mesg
      ;;	     ;; "file.py:\\([0-9]*\\)(\\([0-9]*\\)):\\(.*\\)"
      ;;	     ((string-match (format "^%s:\\([0-9]*\\)(\\([0-9]*\\)):\\(.*\\)"
      ;;				    (file-name-nondirectory tempfile))
      ;;			    line)
      ;;	      ;; do more stuff
      ;;	      (setq n (match-string 1 line))
      ;;	      (setq cn (match-string 2 line))
      ;;	      (setq content (match-string 3 line))
      ;;	      (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s)(forward-char %s))][Line %s:]] %s\n"
      ;;				 cb
      ;;				 (org-element-property :begin eop)
      ;;				 n
      ;;				 cn
      ;;				 n
      ;;				 line)))
      ;;	     ;; no match, just insert line
      ;;	     (t
      ;;	      (setq link (concat line "\n"))))

      ;;	    (insert link))))

      (let ((status (shell-command
		     (format "pep8 %s" (file-name-nondirectory tempfile))))
	    (output (delete "" (split-string
				(with-current-buffer "*Shell Command Output*"
				  (buffer-string)) "\n"))))
	(kill-buffer "*Shell Command Output*")
	(when output
	  (set-buffer (get-buffer-create pb))
	  (insert (format "\n\n* pep8 output (status = %s)\n" status))
	  (insert "pep8 is the [[http://legacy.python.org/dev/peps/pep-0008][officially recommended style]] for writing Python code. Fixing these will usually make your code more readable and beautiful. Your code will probably run if you do not fix them, but, it will be ugly.

")
	  (dolist (line output)
	    ;; get the line number
	    (if
		(string-match (format "^%s:\\([0-9]*\\):\\(.*\\)"
				      (file-name-nondirectory tempfile))
			      line)
		(progn
		  (setq n (match-string 1 line))
		  (setq content (match-string 2 line))
		  (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s))][%s]] %s\n"
				     cb
				     (org-element-property :begin eop)
				     n
				     (format "Line %s: " n)
				     content)))
	      ;; no match, just insert line
	      (setq link (concat line "\n")))
	    (insert link))))

      ;; pylint
      (let ((status (shell-command
		     (concat
		      "pylint "
		      (mapconcat 'identity jmax-pylint-options " ")
		      ;; this is the file to check.
		      (file-name-nondirectory tempfile))))

	    ;; remove empty strings
	    (output (delete "" (split-string
				(with-current-buffer "*Shell Command Output*"
				  (buffer-string)) "\n"))))

	;; also remove this line so the output is empty if nothing
	;; comes up
	(setq output (delete
		      "No config file found, using default configuration"
		      output))

	(kill-buffer "*Shell Command Output*")
	(when output
	  (set-buffer (get-buffer-create pb))
	  (insert (format "\n\n* pylint (status = %s)\n" status))
	  (insert "pylint checks your code for errors, style and convention. It is complementary to pyflakes and pep8, and usually more detailed.

")

	  (dolist (line output)
	    ;; pylint gives a line and column number
	    (if
		(string-match "[A-Z]:\\s-+\\([0-9]*\\),\\s-*\\([0-9]*\\):\\(.*\\)"
			      line)
		(let ((line-number (match-string 1 line))
		      (column-number (match-string 2 line))
		      (content (match-string 3 line)))

		  (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s)(forward-line 0)(forward-char %s))][%s]]\n"
				     cb
				     (org-element-property :begin eop)
				     line-number
				     column-number
				     line)))
	      ;; no match, just insert line
	      (setq link (concat line "\n")))
	    (insert link))))

      (when (get-buffer pb)
	;; open the buffer
	(switch-to-buffer-other-window pb)
	(goto-char (point-min))
	(insert "Press q to close the window\n")
	(org-mode)
	(org-cycle '(64))  ; open everything
	;; make read-only and press q to quit
	(setq buffer-read-only t)
	(use-local-map (copy-keymap org-mode-map))
	(local-set-key "q" #'(lambda () (interactive) (kill-buffer)))

	(unless (= 0 pyflakes-status)
	  (forward-line 4)
	  (message "pyflakes exited non-zero. please fix errors"))
	(switch-to-buffer-other-window cb))
      ;; final cleanup and delete file
      (delete-file tempfile))))

(defvar jmax-run-pycheck nil
  "Non-nil means we run `org-py-check' before running")

; let this run before we run a code block
(defadvice org-babel-execute:python (around pychecker nil activate)
  "check python block for syntax and style errors before running"
  ;; we ignore errors so this does not affect people missing the
  ;; executables pep8, pylint and pyflakes, or who have installation
  ;; errors.
  (ignore-errors
    (when jmax-run-pycheck
      (org-py-check)))

  (save-window-excursion
    ;; execute the block as normal
    ad-do-it
    ;; modify the pycheck buffer if it exists with any errors
    (when (get-buffer "*org pycheck*")
      (switch-to-buffer "*org pycheck*")
      (when (get-buffer "*Org-Babel Error Output*")
	(let ((err (with-current-buffer "*Org-Babel Error Output*"
		     (buffer-string))))
	  (when (not (string= err ""))

	    (goto-char (point-min))
	    (forward-line)
	    (setq buffer-read-only nil)
	    (insert "\n* Org-Babel Error Output\n")
	    (insert err)
	    (setq buffer-read-only t)))))
    ))


(defun jmax-activate-pycheck ()
  "turn on jmax-run-pycheck"
  (interactive)
  (setq jmax-run-pycheck t)
  (ad-activate 'org-babel-execute:python)
  (message "org-py-check is active"))


(defun jmax-deactivate-pycheck ()
  "turn off jmax-run-pycheck"
  (interactive)
  (setq jmax-run-pycheck nil)
  (ad-deactivate 'org-babel-execute:python)
  (message "org-py-check is deactivated for this session"))

(provide 'python-setup)

;;; python-setup.el ends here
