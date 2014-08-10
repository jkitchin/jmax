;;; python-mode-utils.el - generating parts of python-mode.el

;; Copyright (C) 2011-2014  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes, python, oop

;; Python-components-mode started from python-mode.el
;; and python.el, where Tim Peters, Barry A. Warsaw,
;; Skip Montanaro, Ken Manheimer, Dave Love and many
;; others wrote major parts. Author of ipython.el's
;; stuff merged is Alexander Schmolck.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Some of this forms to generate parts of
;; python-mode.el are just drafts, others outdated.
;; Kept for inspiration.

;;; Code:

(defvar arkopf)

(setq arkopf
      "\n;; Copyright (C) 2011-2014  Andreas Roehler
;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

")



(setq py-fast-forms (list

'py--fast-send-string
'py-process-region-fast
'py-execute-statement-fast
'py-execute-block-fast
'py-execute-block-or-clause-fast
'py-execute-def-fast
'py-execute-class-fast
'py-execute-def-or-class-fast
'py-execute-expression-fast
'py-execute-partial-expression-fast
'py-execute-top-level-fast
'py-execute-clause-fast
))

(setq py-bol-forms (list 'py-beginning-of-block-bol
'py-beginning-of-clause-bol
'py-beginning-of-block-or-clause-bol
'py-beginning-of-def-bol
'py-beginning-of-class-bol
'py-beginning-of-def-or-class-bol
'py-beginning-of-if-block-bol
'py-beginning-of-try-block-bol
'py-beginning-of-minor-block-bol
'py-beginning-of-statement-bol))

(defvar py-bol-end-forms (list 'py-end-of-block-bol
'py-end-of-clause-bol
'py-end-of-block-or-clause-bol
'py-end-of-def-bol
'py-end-of-class-bol
'py-end-of-def-or-class-bol
'py-end-of-if-block-bol
'py-end-of-try-block-bol
'py-end-of-minor-block-bol
'py-end-of-statement-bol))

(setq py-bol-copy-forms
      (list 'py-copy-block-bol
            'py-copy-clause-bol
            'py-copy-block-or-clause-bol
            'py-copy-def-bol
            'py-copy-class-bol
            'py-copy-def-or-class-bol
            'py-copy-statement-bol))

;; (defvar py-toggle-form-vars (list "py-set-nil-docstring-style" "py-set-onetwo-docstring-style" "py-set-pep-257-docstring-style" "py-set-pep-257-nn-docstring-style" "py-set-symmetric-docstring-style" "py-set-django-docstring-style" ))

(setq py-toggle-form-vars (list "py-nil-docstring-style" "py-onetwo-docstring-style" "py-pep-257-docstring-style" "py-pep-257-nn-docstring-style" "py-symmetric-docstring-style" "py-django-docstring-style" ))

(defvar docstring-styles (list "django" "onetwo" "pep-257" "pep-257-nn" "symmetric")
  "Customizable variable `py-fill-docstring-style' provides default value
  used by `py-fill-string', `py-fill-paragraph'

  DJANGO:

      \"\"\"
      Process foo, return bar.
      \"\"\"

      \"\"\"
      Process foo, return bar.

      If processing fails throw ProcessingError.
      \"\"\"

  ONETWO:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"
      Process foo, return bar.

      If processing fails throw ProcessingError.

      \"\"\"

  PEP-257:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"Process foo, return bar.

      If processing fails throw ProcessingError.

      \"\"\"

  PEP-257-NN:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"Process foo, return bar.

      If processing fails throw ProcessingError.
      \"\"\"

  SYMMETRIC:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"
      Process foo, return bar.

      If processing fails throw ProcessingError.
      \"\"\"

  Built upon code seen at python.el, thanks Fabian")

(setq py-options (list "" "switch" "no-switch" "dedicated" "dedicated-switch"))

(defvar py-shells
  (list 'python 'ipython 'python3 'python2 'jython 'bpython)
  "Python-mode will generate commands opening shells mentioned here. Edit this list \w resp. to your machine. ")

(setq py-shells
  (list 'python 'ipython 'python2  'jython 'python3 'bpython))

(defvar py-commands
  (list "py-python-command" "py-ipython-command" "py-python3-command" "py-python2-command" "py-jython-command" "py-bpython-command")
  "Python-mode will generate commands opening shells mentioned here. Edit this list \w resp. to your machine. ")

;; (setq  py-shells (list "python" "ipython" "python3" "python2" "python2.7" "jython" "python3.2" "python3.3" "bpython"))

(setq py-test-shells
      (list "python" "ipython" "python3" "python2" "python2.7"))

(setq py-shift-forms (list "paragraph" "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class" "line" "statement" "comment" "top-level"))

(setq py-positions-forms (list "paragraph" "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class" "line" "statement" "comment" "top-level" "partial-expression" "expression"))

(setq py-core-command-name '("statement" "block" "def" "class" "region" "file"))

(defvar py-bounds-command-names (list "statement" "block" "clause" "block-or-clause" "def" "class" "region" "buffer" "expression" "partial-expression" "line" "top-level"))

(defvar py-hide-names (list "region" "statement" "block" "clause" "block-or-clause" "def" "class" "expression" "partial-expression" "line" "top-level"))

(setq py-hide-names (list "region" "statement" "block" "clause" "block-or-clause" "def" "class" "expression" "partial-expression" "line" "top-level"))

(setq  py-bounds-command-names (list "statement" "block" "clause" "block-or-clause" "def" "class" "region" "buffer" "expression" "partial-expression" "line" "top-level"))

(setq py-checker-command-names '("clear-flymake-allowed-file-name-masks" "pylint-flymake-mode" "pyflakes-flymake-mode" "pychecker-flymake-mode" "pep8-flymake-mode" "pyflakespep8-flymake-mode" "py-pylint-doku" "py-pyflakes-run" "py-pyflakespep8-run" "py-pyflakespep8-help"))

(setq py-execute-forms-names (list "statement" "block" "block-or-clause" "def" "class" "def-or-class" "expression" "partial-expression" "top-level" "clause"))

(setq py-delete-forms (list "statement" "top-level" "block" "block-or-clause" "def" "class" "def-or-class" "expression" "partial-expression" "minor-block" "clause"))

(setq py-copy-forms (list "statement" "top-level" "block" "clause" "block-or-clause" "def" "class" "def-or-class" "expression" "partial-expression" "minor-block"))

(defvar py-re-forms-names '("block" "clause" "block-or-clause" "def" "class" "def-or-class" "if-block" "try-block" "minor-block")
  "Forms whose start is described by a regexp in python-mode." )

(setq py-re-forms-names '("block" "clause" "block-or-clause" "def" "class" "def-or-class" "if-block" "try-block" "minor-block"))

;; (setq py-re-forms-names '("block" "clause" "block-or-clause" "def" "class" "def-or-class")

;; (defvar py-down-forms (list "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class" "statement"))
;;
(defvar py-down-forms (list "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class" "statement" "top-level"))

(setq py-down-forms (list "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class" "statement" "top-level"))

(setq py-comment-forms (list "block" "clause" "block-or-clause" "def" "class" "def-or-class" "statement"))

;; (setq py-down-forms (list "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class" "statement"))
(setq py-down-forms (list "block" "minor-block" "clause" "block-or-clause" "def" "class" "def-or-class"))

(defun write-execute-forms (&optional command)
  "Write `py-execute-block...' etc. "
  (interactive)
  (let ((py-bounds-command-names (if command (list command) py-execute-forms-names)))
    (set-buffer (get-buffer-create "python-components-exec-forms.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; python-components-exec-forms.el --- Execute forms at point\n")
    (insert arkopf)
    (insert ";;; Execute forms at point\n\n")
    (dolist (ele py-bounds-command-names)
      (insert (concat "(defun py-execute-" ele " ()"))
      (insert (concat "
  \"Send " ele " at point to a Python interpreter. \"\n"))
      (insert (concat "  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-" ele "-p)
		     (save-excursion
		       (py-beginning-of-" ele ")))))
	(end (save-excursion
	       (py-end-of-" ele"))))
    (py-execute-region beg end)))\n\n"))))
  (insert "(provide 'python-components-exec-forms)
;;; python-components-exec-forms.el ends here\n ")
  (emacs-lisp-mode))

(defun write-fast-execute-forms (&optional command)
  "Write `py-process-block...' etc. "
  (interactive)
  (let ((py-bounds-command-names (if command (list command) py-execute-forms-names)))
    (set-buffer (get-buffer-create "python-components-fast-forms.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; python-components-fast-forms.el --- Execute forms at point\n")
    (insert arkopf)
    (insert ";;; Process forms fast\n\n")
    (insert "

\(defun py-fast-process (&optional buffer)
  \"Connect am (I)Python process suitable for large output.

Output arrives in py-output-buffer, \\\"\\\*Python Output\\\*\\\" by default
It is not in interactive, i.e. comint-mode, as its bookkeepings seem linked to the freeze reported by lp:1253907\"
  (interactive)
  (let ((this-buffer
         (set-buffer (or (and buffer (get-buffer-create buffer))
                         (get-buffer-create py-buffer-name)))))
    (let ((proc (start-process py-shell-name this-buffer py-shell-name)))
      (with-current-buffer this-buffer
        (erase-buffer))
      (setq py-output-buffer this-buffer)
      proc)))

\(defun py--fast-send-string (string)
  \"Process Python strings, being prepared for large output.

Output arrives in py-output-buffer, \\\"\\\*Python Output\\\*\\\" by default
See also `py-fast-shell'

\"
  (let ((proc (or (get-buffer-process (get-buffer py-output-buffer))
                  (py-fast-process))))
    ;;    (with-current-buffer py-output-buffer
    ;;      (erase-buffer))
    (process-send-string proc string)
    (or (string-match \"\\n\$\" string)
	(process-send-string proc \"\\n\"))
    (accept-process-output proc 1)
    (switch-to-buffer py-output-buffer)
    (beginning-of-line)
    (skip-chars-backward \"\\r\\n\")
    (delete-region (point) (point-max))))

\(defun py-process-region-fast (beg end)
  (interactive \"r\")
  (let ((py-fast-process-p t))
    (py-execute-region beg end)))\n\n")
    (dolist (ele py-bounds-command-names)
      (insert (concat "(defun py-execute-" ele "-fast ()"))
      (insert (concat "
  \"Process " ele " at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode\"\n"))
      (insert (concat "  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare \"" ele "\")))\n\n")))
  (insert "(provide 'python-components-fast-forms)
;;; python-components-fast-forms.el ends here\n ")
  (emacs-lisp-mode)))

(defun write-options-dokumentation-subform (pyo)
  (cond ((string-match "dedicated" pyo)
         (insert "\n\nUses a dedicated shell.")))
  (cond ((string-match "no-switch" pyo)
         (insert "\nIgnores default of `py-switch-buffers-on-execute-p', uses it with value \\\"nil\\\""))
        ((string-match "switch" pyo)
         (insert "\nIgnores default of `py-switch-buffers-on-execute-p', uses it with value \\\"non-nil\\\""))))

(defun write-menu-entry (&optional erg)
  "Menu Eintrag einfuegen. "
  (interactive "*")
  (let* ((orig (point))
         (erg (or erg (car kill-ring)))
         (name (intern-soft erg))
         (doku (documentation name))
         last)
      (insert (concat "\[\"" (replace-regexp-in-string "-" " " (replace-regexp-in-string "py-" "" erg)) "\" " erg "
 :help \" `" erg "'
"))
      (when doku (insert (regexp-quote doku)))

      (insert (concat
               ". \"]\n\n"))
      (setq last (point))
      (goto-char orig)
    (skip-chars-forward "[[:punct:]]")
    (capitalize-word 1)
    (goto-char last)))

(defun write-execute-file-forms ()
  (interactive)
  ;; write commandp-tests
  ;; -eval "(assert (commandp 'py-fill-string-symmetric) nil \"py-fill-string-symmetric not detected as command\")" \
  (set-buffer (get-buffer-create "Python-Components-Execute-File-Commandp-Tests"))
  (erase-buffer)
  (shell-script-mode)
  ;; (switch-to-buffer (current-buffer))
  (let (name)
    (dolist (ele py-shells)
      (dolist (pyo py-options)
        (insert (concat "
-eval \"(assert (commandp 'py-execute-file-" ele))
        (unless (string= "" pyo)
          (insert (concat "-" pyo)))
        (insert (concat ") nil \\\""))
        (insert (concat "py-execute-file-" ele))
        (unless (string= "" pyo)
          (insert (concat "-" pyo)))
        (insert " not detected as command\\\")\" \\"))))

  (set-buffer (get-buffer-create "Menu-Python-Components-Execute-File"))
  (erase-buffer)
  (insert "(\"Execute file ... \"
            :help \"Execute file functions\"\n\n")

  (switch-to-buffer (current-buffer))
  (dolist (ele py-shells)
    (setq name (concat "py-execute-file-" ele))
    (write-menu-entry name))
  (insert "(\"Ignoring defaults ... \"
 :help \"Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'\"\n\n")
  (dolist (ele py-shells)
    (dolist (pyo py-options)
      (unless (string= "" pyo)
        (setq name (concat "py-execute-file-" ele))
        (setq name (concat name "-" pyo))
        (write-menu-entry name))))
  (insert "      ))")

  (set-buffer (get-buffer-create "python-components-execute-file.el"))
  (erase-buffer)
  ;; (switch-to-buffer (current-buffer))
  (insert ";;; python-components-execute-file.el --- Execute files from python-mode\n")
  (insert arkopf)
  (insert ";;; Execute file commands\n")
  (dolist (ele py-shells)
    (dolist (pyo py-options)
      (insert (concat "
\(defun py-execute-file-" ele))
      (if (string= "" pyo)
          (insert " (&optional filename)\n")
        (insert (concat "-" pyo " (&optional filename)\n")))
      (insert (concat "  \"Send file to a " (capitalize ele) " interpreter."))
      (write-options-dokumentation-subform pyo)
      (insert (concat "\"
  (interactive \"fFile: \")
  (py--execute-prepare filename \"" ele "\""))
      (cond ((string-match "dedicated" pyo)
             (insert " 'dedicated"))
            (t (insert " nil")))
      (cond ((string-match "no-switch" pyo)
             (insert " 'no-switch"))
            ((string-match "switch" pyo)
             (insert " 'switch"))
            (t (insert " nil")))
      (insert " nil nil t))\n")))
  (insert "\n(provide 'python-components-execute-file)
;;; 'python-components-execute-file.el ends here\n ")
  (emacs-lisp-mode))

(defun write-execute-forms-test (&optional command path-to-shell option)
  "Write `py-execute-block...' etc. "
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
        (py-test-shells (if path-to-shell (list path-to-shell) py-shells))
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-executes-test.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; ")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-executes-test.el"))
    (insert " --- executes test")
    (insert arkopf)
    (dolist (ele py-bounds-command-names)
      ;; (dolist (elt py-shells)
      ;; (dolist (pyo py-options)
      (insert (concat "(defun py-execute-" ele "-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \""))
      (cond ((or (string-match "block" ele)(string-match "clause" ele))
             (insert (concat "if True: print(\\\"I'm the py-execute-" ele)))
            ((string-match "def" ele)
             (insert (concat "def foo (): print(\\\"I'm the py-execute-" ele)))
            ((string= "class" ele)
             (insert (concat "class foo (): print(\\\"I'm the py-execute-" ele)))
            (t (insert (concat "print(\\\"I'm the py-execute-" ele))))
      (insert "-test\\\")\"))")
      (insert (concat "
    (py-bug-tests-intern 'py-execute-" ele))
      (insert (concat "-base arg teststring)))\n"))
      (insert (concat "\n(defun py-execute-" ele))
      (insert "-base ()\n")
      (insert (concat "  (assert (markerp (py-execute-" ele))
      (cond ((string= "region" ele)
             (insert " (line-beginning-position) (line-end-position)"))
            ;; ((string= "buffer" ele)
            ;; (insert " (point-min)(point-max)"))
)
      (insert "))")
      (insert (concat " nil \"py-execute-" ele))
      (insert "-test failed\"))\n\n")))
  (insert "\n\n(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here\n ")
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

(defun write-execute-ert-tests (&optional command path-to-shell option)
  "Write `py-execute-block...' etc. "
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
        (py-test-shells (if path-to-shell (list path-to-shell) py-shells))
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-executes-test.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; ")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-executes-ert-tests.el"))
    (insert " --- executes ert tests")
    (insert arkopf)
    (dolist (ele py-bounds-command-names)
      ;; (dolist (elt py-shells)
      ;; (dolist (pyo py-options)
      (insert (concat "(defun py-execute-" ele "-test ()
  (py-tests-with-temp-buffer \n    "))
      (cond ((or (string-match "block" ele)(string-match "clause" ele))
             (insert (concat "if True: print(\\\"I'm the py-execute-" ele)))
            ((string-match "def" ele)
             (insert (concat "def foo (): print(\\\"I'm the py-execute-" ele)))
            ((string= "class" ele)
             (insert (concat "class foo (): print(\\\"I'm the py-execute-" ele)))
            (t (insert (concat "\"print(\\\"I'm the py-execute-" ele))))
      (insert "-test\\\")\"))")
      (insert (concat "
    (py-execute-" ele ")"))
      (insert (concat "    (set-buffer (py--fetch-first-python-buffer))
    (goto-char (point-max))
    (and (should (search-backward \"py-execute-" ele " -test" nil t 1))
	 (py-kill-buffer-unconditional (current-buffer))))

  (insert "\n\n(provide 'python-executes-ert-tests)
;;; python-executes-ert-tests.el ends here\n ")
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

(defun write-extended-execute-forms-endings ()
  "Internally used by write-extended-execute-forms"

  (if (string-match "dedicated" pyo)
      (insert " t")
    (insert " nil"))
  (cond ((or (string= "switch" pyo)
             (string= "dedicated-switch" pyo))
         (insert " 'switch"))
        ((string= "no-switch" pyo)
         (insert " 'no-switch"))
        (t (insert " nil")))
  (cond ((string= "region" ele)
         (insert " beg end"))
        ((string= "buffer" ele)
         (insert " (point-min) (point-max)))")))
  (insert "))\n\n"))

(defun write-unified-extended-execute-forms (&optional path-to-shell command option)
  "Write `py-execute-statement, ...' etc.

Include default forms "
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
	;; (py-shells py-commands)
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-extended-executes.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; Extended executes")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      ;; (insert "python-extended-executes.el")
      )
    (insert " --- more execute forms")
    (insert arkopf)

    (insert "
;; created by `write-unified-extended-execute-forms'
\(defun py--execute-prepare (form &optional shell dedicated switch beg end file)
  \"Used by python-extended-executes .\"
  (save-excursion
    (let* ((beg (unless file
		  (prog1
		      (or beg (funcall (intern-soft (concat \"py--beginning-of-\" form \"-p\")))

			  (funcall (intern-soft (concat \"py-beginning-of-\" form)))
			  (push-mark)))))
	   (end (unless file
		  (or end (funcall (intern-soft (concat \"py-end-of-\" form))))))
	   (shell (or shell py-shell-name))
	   (py-dedicated-process-p dedicated)
	   (py-switch-buffers-on-execute-p (cond ((eq 'switch switch)
						  t)
						 ((eq 'no-switch switch)
						  nil)
						 (t py-switch-buffers-on-execute-p)))
	   filename)
      (if file
          (progn
            (setq filename (expand-file-name form))
            (if (file-readable-p filename)
                (setq erg (py--execute-file-base nil filename nil nil (or (and (boundp 'py-orig-buffer-or-file) py-orig-buffer-or-file) filename)))
              (message \"%s not readable. %s\" file \"Do you have write permissions?\")))
        (py--execute-base beg end shell)))))\n\n")

    ;; see also `py-checker-command-names'
    (dolist (ele py-bounds-command-names)
      (dolist (elt py-shells)
	(setq elt (prin1-to-string elt)) 
	(setq kurz elt)
        (dolist (pyo py-options)
          (if (string= "default" elt)
              (insert (concat "\n(defun py-execute-" ele))
            (insert (concat "(defun py-execute-" ele  "-" kurz)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (if (string= "region" ele)
              (insert " (beg end)")
            (insert " ()"))
          (insert (concat "
  \"Send " ele " at point to "))
          (cond ((string-match "ipython" kurz)
		 (insert "IPython"))
		((string= "python" kurz)
		 (insert "default"))
		(t (insert (capitalize kurz))))
          (cond ((string= pyo "dedicated")
                 (insert " unique interpreter. "))
                ((string= pyo "dedicated-switch")
                 (insert " unique interpreter and switch to result. "))
                (t (insert " interpreter. ")))
          (cond ((string= pyo "switch")
                 (insert "\n\nSwitch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "))
                ((string= pyo "no-switch")
                 (insert "\n\nKeep current buffer. Ignores `py-switch-buffers-on-execute-p' "))

		)
	  (when (string= "python" kurz)
		 (insert "\n\nFor `default' see value of `py-shell-name'"))
          (insert "\"\n")
          (cond
	   ((string= "region" ele)
	    (insert "  (interactive \"r\")\n")
	    (if (string= "default" elt)
		(insert (concat "  (py--execute-prepare \"" ele "\""))
	      (insert (concat "  (py--execute-prepare \"" ele "\" '" elt "")))
	    (write-extended-execute-forms-endings))
	   ((string= "buffer" ele)
	    (insert "  (interactive)
  \(save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py--execute-prepare \"" ele "\" '" elt)
                 (write-extended-execute-forms-endings))
                (t (insert (concat "  (interactive)
  (py--execute-prepare \"" ele "\" '" elt))
		   (write-extended-execute-forms-endings)))

	  ))))
  (if path-to-shell
      (insert (concat "(provide '" path-to-shell) ")
;;; " path-to-shell ".el ends here\n")
    (insert "(provide 'python-extended-executes)
;;; python-extended-executes.el ends here\n "))
  (emacs-lisp-mode))

(defun py--fetch-first-python-buffer-from-list ()
  "Returns first (I)Python-buffer found in `buffer-list'"
  (interactive)
  (let ((buli (buffer-list))
        erg)
    (while (and buli (not erg))
      (if (string-match "Python" (prin1-to-string (car buli)))
          (setq erg (car buli))
        (setq buli (cdr buli))))
    erg))

(defun write-extended-execute-forms-test (&optional command path-to-shell option)
  "Write `py-execute-block...' etc. "
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
        (py-test-shells (if path-to-shell (list path-to-shell) py-shells))
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-extended-executes-test.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; ")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-extended-executes-test.el"))
    (insert " --- extended-executes test")
    (insert arkopf)
    (dolist (ele py-bounds-command-names)
      (dolist (elt py-shells)
        (dolist (pyo py-options)
          (if (string= "" elt)
              (insert (concat "\n\n(defun py-execute-" ele))
            (insert (concat "\n\n(defun py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-test")
          (insert " (&optional arg load-branch-function)")
          (insert (concat "
  (interactive \"p\")
  (let ((teststring \""))
          (cond ((or (string-match "block" ele)(string-match "clause" ele))
                 (insert (concat "if True: print(\\\"I'm the py-execute-" ele)))
                ((string-match "def" ele)
                 (insert (concat "def foo (): print(\\\"I'm the py-execute-" ele)))
                ((string= "class" ele)
                 (insert (concat "class foo (): print(\\\"I'm the py-execute-" ele)))
                (t (insert (concat "print(\\\"I'm the py-execute-" ele))))
          (unless (string= "" elt) (insert (concat "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-test\\\")\"))")
          (if (string= "" elt)
              (insert (concat "
  (py-bug-tests-intern 'py-execute-" ele))
            (insert (concat "
  (py-bug-tests-intern 'py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert (concat "-base arg teststring)))\n"))
          (if (string= "" elt)
              (insert (concat "\n(defun py-execute-" ele))
            (insert (concat "\n\(defun py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-base ()\n")
          (if (string= "" elt)
              (insert (concat "  (assert (progn (py-execute-" ele))
            (insert (concat "  (assert (progn (py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))

	  (cond ((string= "region" ele)
               (insert " (line-beginning-position) (line-end-position)")))
          (insert ")")

	  (if (string= "" elt)
	      (progn
		(insert (concat "(set-buffer (py--fetch-first-python-buffer))(goto-char (point-min))(search-forward \"the py-execute-"))
		(unless (string= pyo "")(insert (concat "-" pyo)))
		(insert "-test\" nil nil 1))"))
	    (insert (concat "(set-buffer (py--fetch-first-python-buffer))(goto-char (point-min))(search-forward \"the py-execute-" ele "-" elt))
	    (unless (string= pyo "")(insert (concat "-" pyo)))

	    (insert "-test\" nil nil 1))"))

          (if (string= "" elt)
              (insert (concat "
           nil \"py-execute-" ele))
            (insert (concat " nil \"py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-test failed\"))"))))
    (insert "\n\n(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here\n "))
  (emacs-lisp-mode))

(defun write-all-bounds-forms ()
  (interactive)
  (write-bounds-forms py-bounds-command-names))

(defun write-bounds-forms (&optional commands)
  "Write `py-bounds-of-block' etc. "
  (interactive)
  (set-buffer (get-buffer-create "Bounds-forms"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; Bounds\n")
  (dolist (ele (or commands py-bounds-command-names))
    (if (string= ele "region")
        (insert (concat "(defun py-bounds-of-" ele " ()
  \"Returns bounds of " ele " at point.

Returns a list, whose car is beg, cdr - end.\"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (and beg end)
            (when (interactive-p) (message \"%s\" (list beg end)))
          (list beg end))))))\n\n"))
      (insert (concat "(defun py-bounds-of-" ele " (&optional position)
  \"Returns bounds of " ele " at point.

\With optional POSITION, a number, report bounds of " ele " at POSITION.
\Returns a list, whose car is beg, cdr - end.\"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-" ele "-position))
            (end (py-end-of-" ele "-position)))
        (if (and beg end)
            (when (interactive-p) (message \"%s\" (list beg end)))
          (list beg end))))))\n\n
\n"))))
  (emacs-lisp-mode))

(defun write-all-py-menu ()
  (interactive)
  (write-py-menu py-bounds-command-names))

(defun write-py-executes-menu (&optional commands)
  "Reads py-shells. "
  (interactive)
  (let ((menu-buffer "Python Executes Menu Buffer")
        done)
    (set-buffer (get-buffer-create menu-buffer))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; Menu py-execute forms
    \(easy-menu-define py-menu map \"Execute Python\"
          `(\"PyExec\"
            :help \"Python-specific features\"\n")
    (dolist (ccc (or commands py-core-command-name))
      ;; ["Execute statement" py-execute-statement
      ;;              :help "`py-execute-statement'
      ;; Send statement at point to Python interpreter. "]
      (insert (concat "
            [\"Execute " ccc "\" py-execute-" ccc "
             :help \"`py-execute-" ccc "'
       Send " ccc " at point to Python interpreter. \"]\n")))
    (dolist (ccc (or commands py-core-command-name))
      (insert (concat "            ;; " ccc "\n
            (\"Execute " ccc " ... \"
            :help \"Execute " ccc " functions\"\n"))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "
            \[\"py-execute-" ccc "-" ele "\" py-execute-" ccc "-" ele "
            :help \"Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
        With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))
      (insert "            ;; dedicated\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "
            \[\"py-execute-" ccc "-" ele "-dedicated\" py-execute-" ccc "-" ele "-dedicated
:help \"Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert " IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. \"]\n")))
      ;; (unless done
            (insert "            (\"Ignoring defaults ... \"
             :help \"Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'\"")
            ;; (setq done t))
      (insert "            ;; switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "
            \[\"py-execute-" ccc "-" ele "-switch\" py-execute-" ccc "-" ele "-switch
:help \"Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))
      (insert "            ;; dedicated-switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "
            \[\"py-execute-" ccc "-" ele "-dedicated-switch\" py-execute-" ccc "-" ele "-dedicated-switch
:help \"Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' \"]\n")))
      (insert "))"))
    (insert "))")

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun write-py-menu-doppel ()
  "Reads py-shells. "
  (interactive)
  (let ((menu-buffer "*Python Executes Menu Buffer*"))
    (set-buffer (get-buffer-create menu-buffer))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert "(easy-menu-define py-menu map \"Execute Python\"
          `(\"PyExec\"
            :help \"Python-specific features\"\n")
    (dolist (ccc py-core-command-name)
      (insert (concat ";; " ccc "\n"))
      ;; ["Execute statement" py-execute-statement
      ;;              :help "`py-execute-statement'
      ;; Send statement at point to Python interpreter. "]
      (insert (concat "[\"Execute " ccc "\" py-execute-" ccc "
             :help \"`py-execute-" ccc "'
Send statement at point to Python interpreter. \"]\n
             (\"Execute " ccc " ... \"
             :help \"Execute " ccc " functions\"\n"))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "\" py-execute-" ccc "-" ele "\n
:help \"  Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))

      (insert ";; dedicated\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-dedicated\" py-execute-" ccc "-" ele "-dedicated
:help \"  Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert " IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. \"]\n")))

      (insert ";; switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-switch\" py-execute-" ccc "-" ele "-switch
:help \"  Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))
      (insert ";; dedicated-switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-dedicated-switch\" py-execute-" ccc "-" ele "-dedicated-switch
:help \"  Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

Switch to output buffer; ignores `py-switch-buffers-on-execute-p'. \"]\n")))
      (insert ")"))
    (insert "))")

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-make-shell-menu ()
  "Reads py-shells, menu entries for these shells. "
  (interactive)
  (let ((temp-buffer "*Python Shell Menu Buffer*"))
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer)
    (insert ";; Python shell menu")
    (newline)
    (switch-to-buffer (current-buffer))
    (insert "(easy-menu-define py-menu map \"Python Shells\"
'(\"Py-Shell\"
  :help \"Python Shells\"\n
  \[\"Switch to interpreter\" py-shell
   :help \"Switch to Python process in separate buffer\"]\n")
    (let ((liste py-shells)
          erg)
      (while liste
        (setq ele (car liste))
        (setq erg (documentation (intern-soft (car liste))))
        (when (string-match "Optional DEDICATED SWITCH are provided for use from programs. " erg)
          (setq erg (replace-regexp-in-string "\n *Optional DEDICATED SWITCH are provided for use from programs. " "" erg)))
        ;; '("Python"
        ;;       :help "Python-specific features"
        ;;       ["Execute statement" py-execute-statement
        ;;        :help "Send statement at point to Python interpreter. "]
        (insert (concat " \[\"" ele "\" " ele "
  :help \"" erg "\"]\n"))
        (setq liste (cdr liste))))
    (insert "\"-\"")
    ;; dedicated
    (let ((liste py-shells)
          erg)
      (while liste
        (setq ele (concat (car liste) "-dedicated"))
        (setq erg (documentation (intern-soft ele)))
        ;; '("Python"
        ;;       :help "Python-specific features"
        ;;       ["Execute statement" py-execute-statement
        ;;        :help "Send statement at point to Python interpreter. "]
        (insert (concat " \[\"" ele "\" " ele "
  :help \"" erg "\"]\n"))
        (setq liste (cdr liste))))
    (insert "))")))

(defun py-provide-executes-with-resp-to-installed-python ()
  "Reads py-shells. "
  (interactive)
  (let ((temp-buffer "*Python Executes Install Buffer*")
        (menu-buffer "*Python Executes Menu Buffer*"))
    (set-buffer (get-buffer-create menu-buffer))
    (erase-buffer)
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer)
    (insert ";;; Python execute with")
    (newline)
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele " (&optional dedicated switch)
  \"Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert " IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"
  (interactive \"P\")
  (let ((wholebuf t))
  (py--execute-buffer-base \"" ele "\" dedicated switch)))\n\n"))
      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "\" py-execute-buffer-" ele "
:help \"  Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"]\n")))

    (set-buffer temp-buffer)
    (goto-char (point-max))
    (insert ";; dedicated\n")
    (switch-to-buffer (current-buffer))
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele "-dedicated (&optional switch)
  \"Execute buffer through an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter.

Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. \"
  (interactive \"P\")
  (let ((wholebuf t))
  (py--execute-buffer-base \"" ele "\" t switch)))\n\n"))

      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "-dedicated\" py-execute-buffer-" ele "-dedicated
:help \"  Execute buffer through a unique"))
      (if (string= "ipython" ele)
          (insert " IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. \"]\n")))
    (set-buffer temp-buffer)
    (goto-char (point-max))

    (insert ";; switch\n")
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele "-switch (&optional dedicated)
  \"Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter and switch to output buffer.

Ignores `py-switch-buffers-on-execute-p'.
Optional \\\\[universal-argument] makes"))
      (if (string= "ipython" ele)
          (insert " IPython")
        (insert (concat " " (capitalize ele))))
      (insert " run as an unique process. \"
  (interactive \"P\")
  (let ((wholebuf t))
  (py--execute-buffer-base \"" ele "\" dedicated 'switch)))\n\n")

      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "-switch\" py-execute-buffer-" ele "-switch
:help \"  Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"]\n")))

    (set-buffer temp-buffer)
    (goto-char (point-max))
    (insert ";; dedicated-switch\n")
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele "-dedicated-switch (&optional dedicated)
  \"Execute buffer through an unique"))
      (if (string= "ipython" ele)
          (insert " IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

Switch to output buffer; ignores `py-switch-buffers-on-execute-p'. \"
  (interactive)
  (let ((wholebuf t))
  (py--execute-buffer-base \"" ele "\" t 'switch)))\n\n"))

      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "-dedicated-switch\" py-execute-buffer-" ele "-dedicated-switch
:help \"  Execute buffer through a unique"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

Switch to output buffer; ignores `py-switch-buffers-on-execute-p'. \"]\n")))
    (set-buffer temp-buffer)
    (goto-char (point-max))

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-provide-installed-shells-commands ()
  "Reads py-shells, provides commands opening these shell. "
  (interactive)  
  (let ((temp-buffer "python-components-named-shells.el"))
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer)
    (insert ";;; Python named shells")
    (insert arkopf)
    (newline)
    (dolist (ele py-shells)
      (setq ele (prin1-to-string ele))
      (insert (concat "(defun " ele " (&optional argprompt)
  \"Start an "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter.

Optional \\\\[universal-argument] prompts for path to the"))
      (insert (concat " interpreter. \"
  (interactive \"P\")
  (py-shell argprompt nil \"" ele "\"))\n\n")))
    (insert ";; dedicated\n")
    (dolist (ele py-shells)
      (setq ele (prin1-to-string ele))
      (insert (concat "(defun " ele "-dedicated (&optional argprompt switch)
  \"Start an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.

Optional \\\\[universal-argument] prompts for path to the"))
            (insert (concat " interpreter. \"
  (interactive \"p\")"))
      (insert "\n  (let ((py-dedicated-process-p t))\n")
      (insert (concat "    (py-shell argprompt t \"" ele "\")))\n\n")))
    (insert ";; switch\n")
    (dolist (ele py-shells)
      (setq ele (prin1-to-string ele))
      (insert (concat "(defun " ele "-switch (&optional argprompt)
  \"Switch to "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.

Optional \\\\[universal-argument] prompts for path to the"))
      ;; (if (string= "ipython" ele)
      ;;     (insert "IPython")
      ;;   (insert (capitalize ele)))
      (insert (concat " interpreter. \"
  (interactive \"p\")"))
      (insert "\n  (let ((py-switch-buffers-on-execute-p t))\n")
      (insert (concat "    (py-shell argprompt nil \"" ele "\")))\n\n")))
    (insert ";; no-switch\n")
    (dolist (ele py-shells)
      (setq ele (prin1-to-string ele))
      (insert (concat "(defun " ele "-no-switch (&optional argprompt)
  \"Open an "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window, but do not switch to it.

Optional \\\\[universal-argument] prompts for path to the"))
      ;; (if (string= "ipython" ele)
      ;;     (insert "IPython")
      ;;   (insert (capitalize ele)))
      (insert (concat " interpreter. \"
  (interactive \"p\")"))
      (insert "\n  (let (py-switch-buffers-on-execute-p)\n")
      (insert (concat "    (py-shell argprompt nil \"" ele "\")))\n\n")))
    (insert ";; dedicated switch\n")
    (dolist (ele py-shells)
      (setq ele (prin1-to-string ele))
      (insert (concat "(defalias '" ele "-dedicated-switch '" ele "-switch-dedicated)\n"))
      (insert (concat "(defun " ele "-switch-dedicated (&optional argprompt)
  \"Switch to an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.

Optional \\\\[universal-argument] prompts for path to the"))
      ;; (if (string= "ipython" ele)
      ;;     (insert "IPython")
      ;;   (insert (capitalize ele)))
      (insert " interpreter. \"
  \(interactive \"p\")")
  (insert "\n  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))\n")
  (insert (concat "    (py-shell argprompt t \"" ele "\")))\n\n")))
    (insert "
\(defalias 'Python 'python)
\(defalias 'pyhotn 'python)
\(defalias 'pyhton 'python)
\(defalias 'pyt 'python)
\
\(defalias 'Python2 'python2)
\(defalias 'Python3 'python3)
\
\(defalias 'IPython 'ipython)
\(defalias 'Ipython 'ipython)
\(defalias 'iyp 'ipython)
\(defalias 'ipy 'ipython)


\(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
  ))

(defun py-write-re-beg-end-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-re-forms.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-re-forms.el --- Forms start described by a regular-expression \n")
  (insert arkopf)
  (insert ";;; Beg-end forms\n
\(defun py-beginning-of-top-level ()
  \"Go to beginning of block until level of indentation is null.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive)
  (let ((erg (ignore-errors (cdr (py--go-to-keyword py-block-re 0)))))
    erg))\n")

  (insert"
\(defun py--beginning-of-form-intern (regexp &optional iact indent)
 \"Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive \"P\")
  (let ((erg (if indent
                 (ignore-errors
                   (cdr (py--go-to-keyword regexp
                                          (- (progn (if (py--beginning-of-statement-p) (current-indentation) (save-excursion (py-beginning-of-statement) (current-indentation)))) py-indent-offset))))
               (ignore-errors
                 (cdr (py--go-to-keyword regexp indent))))))
    (when (and py-verbose-p iact) (message \"%s\" erg))
    erg))

\(defun py-beginning (&optional indent)
 \"Go to beginning of compound statement or definition at point.

With \\\\[universal-argument], go to beginning one level above.
Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive \"P\")
  (py--beginning-of-form-intern py-extended-block-or-clause-re (interactive-p) indent))

\(defun py-end (&optional indent)
 \"Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive \"P\")
    (let\* ((orig (point))
           (erg (py--end-base 'py-extended-block-or-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
      erg))

\(defun py-up (&optional indent)
 \"Go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive \"P\")
  (py--beginning-of-form-intern py-extended-block-or-clause-re (interactive-p) t))

\(defun py-down (&optional indent)

 \"Go to beginning one level below of compound statement or definition at point.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive \"P\")
    (let\* ((orig (point))
           (erg (py--end-base 'py-extended-block-or-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
      erg))
")
  (dolist (ele py-re-forms-names)
      (insert (concat "
\(defun py-beginning-of-" ele " (&optional indent)"
    "\n \"Go to beginning of " ele ".

With \\\\[universal-argument], go to beginning one level above.
Returns beginning of " ele " if successful, nil otherwise\n\n"))
    (when (string-match "def\\|class" ele)
      (insert "When `py-mark-decorators' is non-nil, decorators are considered too.\n\n"))
    (insert "Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"\n")
    (insert (concat "  (interactive \"P\")
  (py--beginning-of-form-intern py-" ele "-re (interactive-p) indent))\n"))
    (insert (concat "
\(defun py-end-of-" ele " (&optional indent)"))
    (insert (concat "\n \"Go to end of " ele ".\n
Returns end of " ele " if successful, nil otherwise\n\n"))
    (when (string-match "def\\|class" ele)
      (insert "With \\\\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.\n\n"))
    (insert "Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"\n")

    ;; (if (string-match "def\\|class" ele)
    (insert "  (interactive \"P\")")
    ;; (insert "  (interactive)"))
    (insert (concat "
    (let* ((orig (point))
           (erg (py--end-base 'py-" ele "-re orig)))
      (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
      erg))\n"))
    )
  (insert "

;; Buffer
\(defun py-beginning-of-buffer ()
  \"Go to beginning-of-buffer, return position. \"
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

\(defun py-end-of-buffer ()
  \"Go to end-of-buffer, return position.

  If already at end-of-buffer and not at EOB, go to end of next line. \"
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

\(defalias 'py-forward-block 'py-end-of-block)
\(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
\(defalias 'py-forward-class 'py-end-of-class)
\(defalias 'py-forward-clause 'py-end-of-clause)
\(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
\(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
\
\(defalias 'py-previous-block 'py-beginning-of-block)
\(defalias 'py-goto-block-up 'py-beginning-of-block)
\(defalias 'py-backward-block 'py-beginning-of-block)
\(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
\(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
\(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
\(defalias 'beginning-of-class 'py-beginning-of-class)
\(defalias 'py-backward-class 'py-beginning-of-class)
\(defalias 'py-previous-class 'py-beginning-of-class)
\(defalias 'py-previous-clause 'py-beginning-of-clause)
\(defalias 'py-goto-clause-up 'py-beginning-of-clause)
\(defalias 'py-backward-clause 'py-beginning-of-clause)
\(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
\(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
")
  (insert "\n(provide 'python-components-re-forms)
;;; python-components-re-forms.el ends here\n ")
  (emacs-lisp-mode))

(defun py-write-beg-end-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "Positions"))
  (erase-buffer)
  (insert ";;; Positions")
  (dolist (ele py-positions-forms)
    (insert (concat "
\(defun py-beginning-of-" ele "-position ()
  \"Returns beginning of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-" ele ")))
      (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
      erg)))

\(defun py-end-of-" ele "-position ()
  \"Returns end of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at \"[ \\\\t\\\\r\\\\n\\\\f]\*\$\")
                   (skip-chars-backward \" \\t\\r\\n\\f\")
                   (forward-char -1))
                 (py-end-of-" ele "))))
      (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
      erg)))
"))
    (switch-to-buffer (current-buffer))
    (emacs-lisp-mode)))

(defun py-write-end-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "py-write-end-position-forms"))
  (erase-buffer)
  (dolist (ele py-shift-forms)
    (insert "
\(defun py-end-of-" ele "-position ()
  \"Returns end of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-" ele ")))
      (when (interactive-p) (message \"%s\" erg))
      erg)))
")))

(defun py-write-shift-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "python-components-shift-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-shift-forms.el --- Move forms left or right\n")
  (insert arkopf)

  (insert "
\(defalias 'py-shift-region-left 'py-shift-left)
\(defun py-shift-left (&optional count start end)
  \"Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. \"
  (interactive \"p\")
  (let ((erg (py--shift-intern (- count) start end)))
    (when (and (interactive-p) py-verbose-p) (message \"%s\" erg))
    erg))

\(defalias 'py-shift-region-right 'py-shift-right)
\(defun py-shift-right (&optional count beg end)
  \"Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. \"
  (interactive \"p\")
  (let ((erg (py--shift-intern count beg end)))
    (when (and (interactive-p) py-verbose-p) (message \"%s\" erg))
    erg))

\(defun py--shift-intern (count &optional start end)
  (save-excursion
    (let\* ((inhibit-point-motion-hooks t)
           deactivate-mark
           (beg (cond (start)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-beginning))))
                      (t (line-beginning-position))))
           (end (cond (end)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-end))))
                      (t (line-end-position))))
           (orig end))
      (setq beg (copy-marker beg))
      (setq end (copy-marker end))
      (if (< 0 count)
          (indent-rigidly beg end py-indent-offset)
        (indent-rigidly beg end (- py-indent-offset)))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward \" \\t\\r\\n\\f\"))
    (py-indentation-of-statement)))

\(defun py--shift-forms-base (form arg &optional beg end)
  (let\* ((begform (intern-soft (concat \"py-beginning-of-\" form)))
         (endform (intern-soft (concat \"py-end-of-\" form)))
         (orig (copy-marker (point)))
         (beg (cond (beg)
                    ((region-active-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
                    (t (save-excursion
                         (funcall begform)
                         (line-beginning-position)))))
         (end (cond (end)
                    ((region-active-p)
                     (region-end))
                    (t (funcall endform))))
         (erg (py--shift-intern arg beg end)))
    (goto-char orig)
    erg))
")
  (dolist (ele py-shift-forms)
    (insert (concat "
\(defun py-shift-" ele "-right (&optional arg)
  \"Indent " ele " by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \\[universal-argument] to specify a different value.

Returns outmost indentation reached. \"
  (interactive \"\*P\")
  (let ((erg (py--shift-forms-base \"" ele "\" (or arg py-indent-offset))))
        (when (interactive-p) (message \"%s\" erg))
    erg))

\(defun py-shift-" ele "-left (&optional arg)
  \"Dedent " ele " by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \\[universal-argument] to specify a different value.

Returns outmost indentation reached. \"
  (interactive \"\*P\")
  (let ((erg (py--shift-forms-base \"" ele "\" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message \"%s\" erg))
    erg))
")))
      (insert "\n(provide 'python-components-shift-forms)
;;; python-components-shift-forms.el ends here\n ")

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer)))

(defun py-write-down-forms-bol ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-end-of-form-bol-commands.txt"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "py-end-of-" ele "-bol\n")))
  (set-buffer (get-buffer-create "py-end-of-form-bol.el"))
  (erase-buffer)
  (insert ";;; Complementary left corner end of form commands")
  (dolist (ele py-down-forms)
    (insert (concat "
\(defalias 'py-down-" ele "-bol 'py-end-of-" ele "-bol)
\(defun py-end-of-" ele "-bol ()
  \"Goto beginning of line following end of " ele ".
  Returns position reached, if successful, nil otherwise.

A complementary command travelling at beginning of line, whilst `py-end-of-" ele "' stops at right corner.
See also `py-down-" ele "': down from current definition to next beginning of " ele " below. \"
  (interactive)
  (let ((erg (py-end-of-" ele ")))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-up-forms-bol ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-beginning-of-form-bol-commands.txt"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "py-beginning-of-" ele "-bol\n")))
  (set-buffer (get-buffer-create "py-beginning-of-forms-bol.el"))
  (erase-buffer)
  (insert ";;; Complementary left corner beginning of form commands")
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-beginning-of-" ele "-bol ()
  \"Goto beginning of line where " ele " starts.
  Returns position reached, if successful, nil otherwise.

A complementary command travelling at beginning of line, whilst `py-beginning-of-" ele "' stops at indentation.
See also `py-up-" ele "': up from current definition to next beginning of " ele " above. \"
  (interactive)
  (let ((erg (py-beginning-of-" ele ")))
    (when erg
      (unless (eobp)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-down-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-down-forms.el"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-down-" ele " ()
  \"Go to the beginning of next " ele " below in buffer.

Returns indentation if " ele " found, nil otherwise. \"
  (interactive)
  (let\* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(or (py-in-string-or-comment-p)(not (looking-at py-" ele "-re))))))
    (when (interactive-p) (message \"%s\" erg))
    erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-up-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-up-forms"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-up-" ele " ()
  \"Goto end of line preceding beginning of " ele ".
  Returns position reached, if successful, nil otherwise.

A complementary command travelling right, whilst `py-beginning-of-" ele "' stops at left corner. \"
  (interactive)
  (let ((erg (py-beginning-of-" ele ")))
    (when erg
      (unless (bobp)
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward \" \\t\\r\\n\\f\")
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-specifying-shell-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "specifying-shell-forms"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "
\(defun py-execute-region-" ele " (start end)
  \"Send the region to a common shell calling the " ele " interpreter. \"
  (interactive \"r\\nP\")
  (py--execute-base start end \"" ele "\"))

\(defun py-execute-region-" ele "-switch (start end)
  \"Send the region to a common shell calling the " ele " interpreter.
  Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. \"
  (interactive \"r\\nP\")
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-base start end async \"" ele "\")))

\(defun py-execute-region-" ele "-no-switch (start end)
  \"Send the region to a common shell calling the " ele " interpreter.
  Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to.\"
  (interactive \"r\\nP\")
  (let ((py-switch-buffers-on-execute-p))
    (py--execute-base start end async \"" ele "\")))"))))

(defun xemacs-remove-help-strings ()
  "menu :help not supported presently at XEmacs. "
  (interactive "*")
  (let (erg)
    (goto-char (point-min))
    (while
        (and (search-forward ":help" nil t 1)(not (ar-in-string-or-comment-p)))
      (save-match-data
        (skip-chars-forward "[[:blank:]\"]+")
        (ar-kill-string-atpt)
        (setq erg (point))
        (push-mark))
      (goto-char (match-beginning 0))
      (delete-region (point) erg)
      (if (empty-line-p)
          (delete-region (line-beginning-position) (1+ (line-end-position)))
        (push-mark)
        (setq erg (point))
        (skip-chars-backward " \t\r\n\f")
        (delete-region (point) erg))))
  (message "%s" "fertig"))

(setq py-noregexp-forms (list "paragraph" "line" "statement" "expression" "partial-expression"))

(setq py-regexp-forms (list "block" "clause" "block-or-clause" "def" "class" "def-or-class"))

(defun py-write-beginning-of-p-forms ()
  (interactive)
  (set-buffer (get-buffer-create "Beginning-of"))
  (erase-buffer)
  (insert ";;; Beginning-of- p
\(defun py--beginning-of-line-p ()
  \"Returns position, if cursor is at the beginning of a line, nil otherwise. \"
  (when (bolp)(point)))

\(defun py--beginning-of-buffer-p ()
  \"Returns position, if cursor is at the beginning of buffer, nil otherwise. \"
  (when (bobp)(point)))\n")

  (dolist (ele py-noregexp-forms)
    (unless (string= "line" ele)
      (insert (concat "
\(defun py-beginning-of-" ele "-p ()
  \"Returns position, if cursor is at the beginning of a " ele ", nil otherwise. \"
  (let ((orig (point))
         erg)"))
      (when (string= "paragraph" ele)
        (insert "
     (if (and (bolp) (looking-at paragraph-separate))
         (setq erg (point))"))
      (insert (concat "
     (save-excursion
       (py-end-of-" ele ")
       (py-beginning-of-" ele ")
       (when (eq orig (point))
         (setq erg orig))"))
      (when (string= "paragraph" ele)
        (insert ")"))
      (insert (concat "
       erg)))
"))))
  (dolist (ele py-regexp-forms)
    (insert (concat "
\(defun py-beginning-of-" ele "-p ()
  \"Returns position, if cursor is at the beginning of a " ele ", nil otherwise. \"
    (when (and (looking-at py-" ele "-re)
               (not (py-in-string-or-comment-p)))
      (point)))
")))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-beginning-of-p-forms ()
  (interactive)
  (set-buffer (get-buffer-create "End-of"))
  (erase-buffer)
  (insert ";;; End-of- p
\(defun py--end-of-line-p ()
  \"Returns position, if cursor is at the end of a line, nil otherwise. \"
  (when (eolp)(point)))

\(defun py--end-of-buffer-p ()
  \"Returns position, if cursor is at the end of buffer, nil otherwise. \"
  (when (eobp)(point)))\n")

  (dolist (ele py-noregexp-forms)
    (unless (string= "line" ele)
      (insert (concat "
\(defun py-end-of-" ele "-p ()
  \"Returns position, if cursor is at the end of a " ele ", nil otherwise. \"
  (let ((orig (point))
         erg)"))
      (when (string= "paragraph" ele)
        (insert "
     (if (and (eolp) (looking-at paragraph-separate))
         (setq erg (point))"))
      (insert (concat "
     (save-excursion
       (py-beginning-of-" ele ")
       (py-end-of-" ele ")
       (when (eq orig (point))
         (setq erg orig))"))
      (when (string= "paragraph" ele)
        (insert ")"))
      (insert (concat "
       erg)))
"))))
  (dolist (ele py-regexp-forms)
    (insert (concat "
\(defun py-end-of-" ele "-p ()
  \"Returns position, if cursor is at the end of a " ele ", nil otherwise. \"
    (when (and (looking-at py-" ele "-re)
               (not (py-in-string-or-comment-p)))
      (point)))
")))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-mark-forms ()
  (interactive)
  (set-buffer (get-buffer-create "Mark-forms"))
  (erase-buffer)
  (insert ";;; Mark forms
\(defun py-mark-base (form &optional py-mark-decorators)
  (let\* ((begform (intern-soft (concat \"py-beginning-of-\" form)))
         (endform (intern-soft (concat \"py-end-of-\" form)))
         (begcheckform (intern-soft (concat \"py-beginning-of-\" form \"-p\")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message \"%s %s\" beg end))
    (cons beg end)))\n")
  (dolist (ele py-shift-forms)
    (if (string-match "def\\|class" ele)
        (insert (concat "
\(defun py-mark-" ele " (&optional arg)"))
      (insert (concat "
\(defun py-mark-" ele " ()")))
    (insert (concat "
  \"Mark " ele " at point.\n\n"))
    (when (string-match "def\\|class" ele)
      (insert "With \\\\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
"))

    (insert (concat "Returns beginning and end positions of marked area, a cons. \""))
    (if (string-match "def\\|class" ele)
        (insert "\n  (interactive \"P\")")
      (insert "\n  (interactive)"))
    (if (string-match "def\\|class" ele)
        (insert (concat "\n  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base \"" ele "\" py-mark-decorators)"))
      (insert "\n  (let (erg)
    (setq erg (py-mark-base \"" ele "\"))"))
    (insert "
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))")
    (newline))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-toggle-forms (&optional arg)
  "Write toggle-forms according to (car kill-ring) "
  (interactive "P")
  (let ((liste (if (eq 4 (prefix-numeric-value arg))
                   (car kill-ring)
                 py-toggle-form-vars))
        done buffer-out first menu-buffer)
    (if (listp liste)
        (dolist (ele liste)
          (write-toggle-forms-intern ele))
      (write-toggle-forms-intern liste))))

(defun write-toggle-forms-intern (ele)
      (if done
          (set-buffer buffer-out)
        (setq buffer-out (capitalize ele))
        (set-buffer (get-buffer-create buffer-out))
        (erase-buffer)
        (insert (concat ";;; " ele " forms\n\n"))
        (setq done t))
      (message "Writing for; %s" ele)
      (insert (concat "
\(defun toggle-" ele " (&optional arg)
  \"If `" ele "' should be on or off.

  Returns value of `" ele "' switched to. \"
  (interactive)
  (let ((arg (or arg (if " ele " -1 1))))
    (if (< 0 arg)
        (setq " ele " t)
      (setq " ele " nil))
    (when (or py-verbose-p (interactive-p)) (message \"" ele ": %s\" " ele "))
    " ele "))

\(defun " ele "-on (&optional arg)
  \"Make sure, " ele "' is on.

Returns value of `" ele "'. \"
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-" ele " arg))
  (when (or py-verbose-p (interactive-p)) (message \"" ele ": %s\" " ele "))
  " ele ")

\(defun " ele "-off ()
  \"Make sure, `" ele "' is off.

Returns value of `" ele "'. \"
  (interactive)
  (toggle-" ele " -1)
  (when (or py-verbose-p (interactive-p)) (message \"" ele ": %s\" " ele "))
  " ele ")"))
      (newline)
      (emacs-lisp-mode)
      (eval-buffer)
      (if first
          (set-buffer menu-buffer)
        (setq menu-buffer (concat "Menu " ele))
        (set-buffer (get-buffer-create menu-buffer))
        (erase-buffer)
        (insert (concat ";;; " ele " forms\n\n"))
        (setq first t))
      (switch-emen ele)
      ;; (set-buffer buffer-out)
      ;; (switch-to-buffer (current-buffer))
      )

(defun write-commandp-forms ()
  "Write forms according to `py-bounds-command-names' "
  (interactive)
  (let ((erg py-bounds-command-names))

    (set-buffer (get-buffer-create "Commandp tests"))
    (erase-buffer)
    (dolist (ele erg)
      (insert (concat "--funcall " ele "-commandp-test \\\n")))
    (insert ";;; Commandp tests")
    ;; (dolist (ele py-bounds-command-names)
    (dolist (ele erg)
      (insert (concat "
\(defun " ele "-commandp-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \"\"))
  (py-bug-tests-intern '" ele "-commandp-base arg teststring)))

\(defun " ele "-commandp-base ()
    (assert (commandp '" ele ") nil \"" ele "-commandp-test failed\"))"))
      (newline)))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-invoke-py-shell-forms ()
  "Write forms according to `py-shells' "
  (interactive)
  (set-buffer (get-buffer-create "Py-shell interactive calls"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "'py-shell-invoking-" ele "-lp:835151-test\n")))

  (set-buffer (get-buffer-create "Py-shell batch-commands"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "--funcall py-shell-invoking-" ele "-lp:835151-test \\\n")))

  (set-buffer (get-buffer-create "Py-shell tests"))
  (erase-buffer)
  (insert ";;; Py-shell tests")
  ;; (dolist (ele py-bounds-command-names)
  (dolist (ele py-shells)
    (insert (concat "
\(defun py-shell-invoking-" ele "-lp:835151-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \"print(\\\"py-shell-name: " ele "\\\")\"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shell-invoking-" ele "-lp:835151-base arg teststring)))

\(defun py-shell-invoking-" ele "-lp:835151-base ()
  (setq py-shell-name \"" ele "\")
  (assert (markerp (py-execute-buffer)) nil \"py-shell-invoking-" ele "-lp:835151-test failed\"))\n")))
  (newline)
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defalias 'fehler-python-tests 'lookup-failing-command)
(defun lookup-failing-command ()
  "From ./python-mode-tests.sh buffer, jump to definition of command.

Needs `elisp-find-definition' from
http://repo.or.cz/w/elbb.git/blob/HEAD:/code/Go-to-Emacs-Lisp-Definition.el
"
  (interactive)
  (let (erg)
    (search-backward " pass")
    (forward-char -1)
    (setq erg (prin1-to-string (symbol-at-point)))
    (find-file "~/arbeit/emacs/python-modes/components-python-mode/test/python-mode-tests.sh")
    (goto-char (point-min))
    (when (search-forward erg)
      (search-forward "--funcall ")
      (setq erg (prin1-to-string (symbol-at-point)))
      (elisp-find-definition erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun list-python-mode-test-forms ()
  (interactive)
  (let* ((oldbuf (current-buffer))
         (batchbuffer (concat (capitalize (buffer-name oldbuf)) "-test-batch-calls"))
         (symbolbuffer (concat (capitalize (buffer-name oldbuf)) "-test-symbols"))
         tests)
    (set-buffer (get-buffer-create symbolbuffer))
    (erase-buffer)
    (set-buffer (get-buffer-create batchbuffer))
    (erase-buffer)
    (set-buffer oldbuf)
    (goto-char (point-min))
    (while (and (not (eobp))(re-search-forward "^(defun [[:alpha:]]" nil t 1))
      (let* ((name  (prin1-to-string (symbol-at-point))))
        (unless (string-match "-base" name) (add-to-list 'tests name))
        (forward-line 1)))
    (setq tests (nreverse tests))
    (set-buffer batchbuffer)
    (dolist (ele tests)
      (insert (concat "'" ele "\n")))
    (set-buffer symbolbuffer)
    (dolist (ele tests)
      (insert (concat "--funcall " ele " \\\n")))))

(defun write-script-completion-tests (&optional pyshellname-list)
  (interactive)
  (let ((pyshellname-list (or pyshellname-list py-test-pyshellname-list))
        (sepchar (py-separator-char))
        (symbolbuffer "script-completion-test-symbols")
        (batchbuffer "script-completion-test-funcalls"))
    (set-buffer (get-buffer-create batchbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string "/+" "-" (replace-regexp-in-string "^[/~]+" "" ele)))
      (insert (concat "--funcall " ele "-complete-test \\\n")))
    (set-buffer (get-buffer-create symbolbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string "/+" "-" (replace-regexp-in-string "^[/~]+" "" ele)))
      (insert (concat "'" ele "-complete-test\n")))
    (set-buffer (get-buffer-create "py-script-completion-tests.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; py-script-completion-tests.el --- Test completion for available Python shell\n")
    (insert arkopf)
    ;; (insert ";;; \n\n")
    (insert "(setq python-mode-script-complete-tests
        (list \n")
    (insert-buffer symbolbuffer)
    (goto-char (point-max))
    (insert "))\n
\(defun py-run-script-complete-tests ()
  (interactive)
  (dolist (ele python-mode-script-complete-tests)
    (funcall ele)))\n\n")

    (dolist (ele pyshellname-list)
      (setq elt ele)
      (when (string-match "^~" ele)
        (setq ele (replace-regexp-in-string "^~" (concat "/home/" user-login-name) ele)))
      (setq ele (replace-regexp-in-string "/+" "-" (replace-regexp-in-string "^/" "" ele)))
      (insert (concat "\(defun " ele "-complete-test (&optional arg)
  (interactive \"p\")
  (let ((teststring \""))
      (if (string-match sepchar elt)
          (progn
            (when (string-match "^~" elt)
              (setq elt (replace-regexp-in-string "^~" (concat "/home/" user-login-name) elt)))
            (insert (concat "#! " elt "\n")))
        (insert (concat "#! /usr/bin/env " elt "\n")))
      (insert (concat "pri\"))
    (py-bug-tests-intern '" ele "-complete-base arg teststring)))

\(defun " ele "-complete-base ()
  (save-excursion (py-shell-complete))
  ;; (sit-for 0.1)
  (assert (looking-at \"print\") nil \"" ele "complete-test failed\"))\n\n"))))
  (insert "\n(provide 'py-script-completion-tests)
;;; py-script-completion-tests ends here\n ")

  (emacs-lisp-mode))

(defun write-shell-completion-tests (&optional pyshellname-list)
  (interactive)
  (let ((pyshellname-list (or pyshellname-list py-test-pyshellname-list))
        (sepchar (py-separator-char))
        (symbolbuffer "completion-test-symbols")
        (batchbuffer "completion-test-funcalls"))
    (set-buffer (get-buffer-create  batchbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string  "/+" "-" (replace-regexp-in-string  "^[/~]+" "" ele)))
      (insert (concat "--funcall " ele "-shell-complete-test \\\n")))
    (set-buffer (get-buffer-create symbolbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string  "/+" "-" (replace-regexp-in-string  "^[/~]+" "" ele)))
      (insert (concat "'" ele "-shell-complete-test\n")))
    (set-buffer (get-buffer-create "py-shell-completion-tests.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; py-shell-completion-tests.el --- Test completion for available Python shell\n")
    (insert arkopf)
    (insert "(setq python-mode-shell-complete-tests
        (list\n\n")
    (insert-buffer symbolbuffer)
    (goto-char (point-max))
    (insert "))\n\n")
    (forward-line -4)
    (richten)
    (goto-char (point-max))
    (insert "(defun py-run-shell-complete-tests ()
  (interactive)
  (dolist (ele python-mode-shell-complete-tests)
    (funcall ele)))\n\n")

    ;; (insert ";;; \n\n")
    (dolist (ele pyshellname-list)
      (setq elt ele)
      (setq ele (replace-regexp-in-string  "/+" "-" (replace-regexp-in-string  "^[/~]+" "" ele)))
      (insert (concat "\(defun " ele "-shell-complete-test ()
  (interactive)
  (let ((py-completion-delay 0)
        py-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t \"" elt "\" nil \"" sepchar "\"))
    (sit-for 0.2 t)
    (goto-char (point-max))
    (insert \"pri\")
    (py-shell-complete)
    (beginning-of-line)
    (assert (looking-at \"print\") nil \"" ele "-shell-complete-test failed\")
    (when py-verbose-p (message \"%s\" \"" ele "-shell-complete-test passed\"))))
\n\n")))
    (insert "\n(provide 'py-shell-completion-tests)
;;; py-shell-completion-tests ends here\n ")
    (emacs-lisp-mode)))

(defvar python-mode-el-dir ""
  "Directory, where python-mode.el to edit resides. Used by related-diff")

(defalias 'rel 'related-diff)
(defun related-diff (&optional file)
  "Calls ediff from symbol in Components-branch agains trunk pytho-mode.el

Var `python-mode-el-dir' needs to be set.  "
  (interactive)
  (let ((buffer1 (current-buffer))
        (file2 (cond (file)
                     ((string-match "components" (buffer-file-name))
                      (concat (py--normalize-directory (expand-file-name python-mode-el-dir)) "python-mode.el"))))
        (name (ar-symbol-name-atpt))
        (keyword (progn (re-search-backward "^([^ ]+" nil (quote move) 1)(match-string-no-properties 0))))
    (toggle-read-only -1)
    (save-restriction
      (push-mark)
      (narrow-to-region (point) (1+ (forward-list)))
      (when (buffer-live-p "python-mode.el")
        (kill-buffer "python-mode.el"))
      (when (buffer-live-p "python-mode.el<2>")
        (kill-buffer "python-mode.el<2>"))
      (find-file file2)
      (save-restriction
        (widen)
        (when hs-minor-mode (hs-show-all))
        (goto-char (point-min))
        (when (re-search-forward (concat "^" keyword " +" name "[ \n\t]") nil (quote move) 1)
          (goto-char (match-beginning 0))
          (push-mark)
          (narrow-to-region (point) (1+ (forward-list)))
          (ediff-buffers (current-buffer) buffer1))))))

(defun py-write-bol-forms ()
  (interactive)
    (set-buffer (get-buffer-create "bol-menu.el"))
    (erase-buffer)
    (dolist (ele py-down-forms)
      (insert (concat "(\" " (capitalize ele) " bol ... \"
             [\"Beginning of " ele " bol\" py-beginning-of-" ele "-bol
              :help \"`py-beginning-of-" ele "-bol'
Go to beginning of line at beginning of " ele ".

Returns position reached, if successful, nil otherwise. \"]\n"))

  (insert (concat "
             [\"End of " ele " bol\" py-end-of-" ele "-bol
              :help \"`py-end-of-" ele "-bol'
Go to beginning of line following end of " ele ".

Returns position reached, if successful, nil otherwise. \"]

             [\"Up " ele " bol\" py-up-" ele "-bol
              :help \"`py-up-" ele "-bol'
Go to next " ele " upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. \"]

             [\"Down " ele " bol\" py-down-" ele "-bol
              :help \"`py-down-" ele "-bol'
Go to next " ele " downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. \"]

             [\"Mark " ele " bol\" py-mark-" ele "-bol
              :help \"`py-mark-" ele "-bol'
Mark " ele " at point. \"]

             [\"Copy " ele " bol\" py-copy-" ele "-bol
              :help \"`py-copy-" ele "-bol'
Copy " ele " at point. \"]

             [\"Kill " ele " bol\" py-kill-" ele "-bol
              :help \"`py-kill-" ele "-bol'
Kill " ele " at point. \"]

             [\"Delete " ele " bol\" py-delete-" ele "-bol
              :help \"`py-delete-" ele "-bol'
Delete " ele " at point. \"]\n)\n")))

  (set-buffer (get-buffer-create "python-components-bol-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-bol-forms.el -- Forms start/end at beginning of line\n")
  (insert arkopf)
  (insert ";;; Beginning of line forms
\(defun py--mark-base-bol (form &optional py-mark-decorators)
  (let\* ((begform (intern-soft (concat \"py-beginning-of-\" form \"-bol\")))
         (endform (intern-soft (concat \"py-end-of-\" form \"-bol\")))
         (begcheckform (intern-soft (concat \"py-beginning-of-\" form \"-bol-p\")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator-bol))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message \"%s %s\" beg end))
    (cons beg end)))\n")
  (dolist (ele py-down-forms)
;; beg-end check forms
    (insert (concat "
\(defun py-beginning-of-" ele "-bol-p ()
  \"Returns position, if cursor is at the beginning of " ele ", at beginning of line, nil otherwise. \"
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-" ele "-bol)
      (py-beginning-of-" ele "-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

\(defalias 'py-beginning-of-" ele "-lc 'py-beginning-of-" ele "-bol)
\(defun py-beginning-of-" ele "-bol (&optional indent)
  \"Goto beginning of line where " ele " starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-" ele "': up from current definition to next beginning of " ele " above. \"
  (interactive)
  (let* ((indent (or indent (when (eq 'py-end-of-" ele "-bol (car py-bol-forms-last-indent))(cdr py-bol-forms-last-indent))))
          erg)
         (if indent
                 (while (and (setq erg (py-beginning-of-" ele ")) (< indent (current-indentation))(not (bobp))))
               (setq erg (py-beginning-of-" ele ")))
    ;; reset
    (setq py-bol-forms-last-indent nil)
    (when erg
      (unless (eobp)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))

\(defalias 'py-down-" ele "-lc 'py-end-of-" ele "-bol)
\(defun py-end-of-" ele "-bol ()
  \"Goto beginning of line following end of " ele ".
  Returns position reached, if successful, nil otherwise.

See also `py-down-" ele "': down from current definition to next beginning of " ele " below. \"
  (interactive)
  (let ((erg (py-end-of-" ele ")))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))

    ;; Mark
    (if (string-match "def\\|class" ele)
        (insert (concat "
\(defun py-mark-" ele "-bol (&optional arg)"))
      (insert (concat "
\(defun py-mark-" ele "-bol ()")))
    (insert (concat "
  \"Mark " ele ", take beginning of line positions. \n\n"))
    (when (string-match "def\\|class" ele)
      (insert "With \\\\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
"))

    (insert (concat "Returns beginning and end positions of region, a cons. \""))
    (if (string-match "def\\|class" ele)
        (insert "\n  (interactive \"P\")")
      (insert "\n  (interactive)"))
    (if (string-match "def\\|class" ele)
        (insert (concat "\n  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py--mark-base-bol \"" ele "\" py-mark-decorators)"))
      (insert "\n  (let (erg)
    (setq erg (py--mark-base-bol \"" ele "\"))"))
    (insert "
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))\n")

    ;; Copy
    (insert (concat "
\(defun py-copy-" ele "-bol ()
  \"Delete " ele " bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. \"
  (interactive \"\*\")
  (let ((erg (py--mark-base-bol \"" ele "\")))
    (copy-region-as-kill (car erg) (cdr erg))))

\(defun py-kill-" ele "-bol ()
  \"Delete " ele " bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. \"
  (interactive \"\*\")
  (let ((erg (py--mark-base-bol \"block\")))
    (kill-region (car erg) (cdr erg))))

\(defun py-delete-" ele "-bol ()
  \"Delete " ele " bol at point.

Don't store data in kill ring. \"
  (interactive \"\*\")
  (let ((erg (py--mark-base-bol \"block\")))
    (delete-region (car erg) (cdr erg))))
")))
  (insert "\n;; python-components-bol-forms.el ends here
\(provide 'python-components-bol-forms)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-forms ()
  (interactive)
    (set-buffer (get-buffer-create "form-menu.el"))
    (erase-buffer)
    (dolist (ele py-down-forms)
      (insert (concat "(\" " (capitalize ele) "  ... \"
             [\"Beginning of " ele " \" py-beginning-of-" ele "
              :help \"`py-beginning-of-" ele "'
Go to beginning of line at beginning of " ele ".

Returns position reached, if successful, nil otherwise. \"]\n"))

  (insert (concat "
             [\"End of " ele " \" py-end-of-" ele "
              :help \"`py-end-of-" ele "'
Go to beginning of line following end of " ele ".

Returns position reached, if successful, nil otherwise. \"]

             [\"Up " ele " \" py-up-" ele "
              :help \"`py-up-" ele "'
Go to next " ele " upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. \"]

             [\"Down " ele " \" py-down-" ele "
              :help \"`py-down-" ele "'
Go to next " ele " downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. \"]

             [\"Mark " ele " \" py-mark-" ele "
              :help \"`py-mark-" ele "'
Mark " ele " at point. \"]

             [\"Copy " ele " \" py-copy-" ele "
              :help \"`py-copy-" ele "'
Copy " ele " at point. \"]

             [\"Kill " ele " \" py-kill-" ele "
              :help \"`py-kill-" ele "'
Kill " ele " at point. \"]

             [\"Delete " ele " \" py-delete-" ele "
              :help \"`py-delete-" ele "'
Delete " ele " at point. \"]\n)\n")))

  (set-buffer (get-buffer-create "python-components-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-forms.el -- Forms start/end at beginning of line\n")
  (insert arkopf)
  (insert ";;; Beginning of line forms
\(defun py-mark-base (form &optional py-mark-decorators)
  (let\* ((begform (intern-soft (concat \"py-beginning-of-\" form)))
         (endform (intern-soft (concat \"py-end-of-\" form)))
         (begcheckform (intern-soft (concat \"py-beginning-of-\" form \"-p\")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message \"%s %s\" beg end))
    (cons beg end)))\n")
  (dolist (ele py-down-forms)
;; beg-end check forms
    (insert (concat "
\(defun py-beginning-of-" ele "-p ()
  \"Returns position, if cursor is at the beginning of a " ele ", nil otherwise. \"
  (when (and (looking-at py-" ele "-re)
             (not (py-in-string-or-comment-p)))
    (point)))

\(defalias 'py-beginning-of-" ele "-lc 'py-beginning-of-" ele ")
\(defun py-beginning-of-" ele " (&optional indent)
  \"Goto beginning of line where " ele " starts.
  Returns position reached, if successful, nil otherwise.\"
  (interactive)
  (let ((indent (and (looking-at py-" ele "-re)
                     (current-indentation))))
    (py--beginning-of-form-intern py-" ele "-re (interactive-p) indent)))

\(defalias 'py-down-" ele "-lc 'py-end-of-" ele ")
\(defun py-end-of-" ele " (&optional indent)
  \"Go to end of " ele ".

Returns end of " ele " if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive \"P\")
  (let\* ((orig (point))
         (erg (py--end-base 'py-" ele "-re orig)))
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))

\(defalias 'py-down-" ele "-lc 'py-end-of-" ele "-lc)
\(defun py-end-of-" ele "-lc ()
  \"Goto beginning of line following end of " ele ".
  Returns position reached, if successful, nil otherwise.

See also `py-down-" ele "': down from current definition to next beginning of " ele " below. \"
  (interactive)
  (let ((erg (py-end-of-" ele ")))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))

    ;; Mark
    (if (string-match "def\\|class" ele)
        (insert (concat "
\(defun py-mark-" ele " ()
  \"Mark " ele " at point.

Returns beginning and end positions of marked area, a cons. \"
  (interactive)
  (let (erg)
    (setq erg (py-mark-base \"" ele "\"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))

\(defun py-copy-" ele " ()
  \"Mark " ele " at point.

Returns beginning and end positions of marked area, a cons. \"
  (interactive)
  (let ((erg (py-mark-base \"" ele "\")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

\(defun py-kill-" ele " ()
  \"Delete " ele "  at point.

Stores data in kill ring. Might be yanked back using `C-y'. \"
  (interactive \"\*\")
  (let ((erg (py-mark-base \"ele\")))
    (kill-region (car erg) (cdr erg))))

\(defun py-delete-" ele " ()
  \"Delete " ele "  at point.

Don't store data in kill ring. \"
  (interactive \"\*\")
  (let ((erg (py-mark-base \"ele\")))
    (delete-region (car erg) (cdr erg))))
"))))
  (insert "\n;; python-components-forms.el ends here
\(provide 'python-components-forms)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-up-down-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-up-down.el"))
  (erase-buffer)
  (insert ";;; python-components-up-down.el -- Searching up/downwards in buffer\n")
  (insert arkopf)
  (insert "
\(defun py-up-statement ()
  \"Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. \"
  (interactive)
  (let ((orig (point))
        erg)
  (if (py--beginning-of-statement-p)
      (setq erg (py-beginning-of-statement))
    (setq erg (and (py-beginning-of-statement) (py-beginning-of-statement))))
  (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
  erg))

\(defun py-down-statement ()
  \"Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise. \"
  (interactive)
  (let\* ((orig (point))
           (erg
            (cond ((py--end-of-statement-p)
                   (setq erg (and (py-end-of-statement) (py-beginning-of-statement))))
                  ((< orig (progn (py-end-of-statement) (py-beginning-of-statement)))
                   (point))
                  (t (and (py-end-of-statement) (py-end-of-statement)(py-beginning-of-statement))))))
            (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
            erg)
    erg))

\(defun py-up-base (regexp)
  \"Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise. \"
  (let\* ((orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (syntax-ppss))))
      (back-to-indentation)
      (when (looking-at regexp) (setq erg (point)))
      (when py-verbose-p (message \"%s\" erg))
      erg)))

\(defun py-down-base (regexp)
  \"Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise. \"
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let\* ((orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
        (back-to-indentation)
        (when (looking-at regexp) (setq erg (point)))
        (when py-verbose-p (message \"%s\" erg))
        erg))))

\(defun py-up-base-bol (regexp)
  \"Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise. \"
  (let\* ((orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (syntax-ppss))))
      (beginning-of-line)
      (when (looking-at regexp) (setq erg (point)))
      (when py-verbose-p (message \"%s\" erg))
      erg)))

\(defun py-down-base-bol (regexp)
  \"Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise. \"
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let\* ((orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
        (beginning-of-line)
        (when (looking-at regexp) (setq erg (point)))
        (when py-verbose-p (message \"%s\" erg))
        erg))))\n")
  ;; up
  (dolist (ele py-down-forms)
    (if (string= "statement" ele)
        (insert "\n(defalias 'py-up-statement 'py-beginning-of-statement)\n")
      (insert (concat "
\(defun py-up-" ele " ()
  \"Go to the beginning of next " ele " upwards in buffer.

Return position if " ele " found, nil otherwise. \"
  (interactive)
  (py-up-base py-" ele "-re))\n"))))
  ;; down
  (dolist (ele py-down-forms)
    (if (string= "statement" ele)
        (insert "\n(defalias 'py-down-statement 'py-end-of-statement)\n")
      (insert (concat "
\(defun py-down-" ele " ()
  \"Go to the beginning of next " ele " below in buffer.

Return position if " ele " found, nil otherwise. \"
  (interactive)
  (py-down-base py-" ele "-re))\n"))))
  ;; up bol
  (dolist (ele py-down-forms)
    (if (string= "statement" ele)
        (insert "\n(defalias 'py-up-statement-bol 'py-beginning-of-statement-bol)\n")
      (insert (concat "
\(defun py-up-" ele "-bol ()
  \"Go to the beginning of next " ele " upwards in buffer.

Go to beginning of line.
Return position if " ele " found, nil otherwise. \"
  (interactive)
  (py-up-base-bol py-" ele "-re))\n"))))
  ;; down bol
  (dolist (ele py-down-forms)
    (if (string= "statement" ele)
        (insert "\n(defalias 'py-down-statement-bol 'py-end-of-statement-bol)\n")
      (insert (concat "
\(defun py-down-" ele "-bol ()
  \"Go to the beginning of next " ele " below in buffer.

Go to beginning of line
Return position if " ele " found, nil otherwise \"
  (interactive)
  (py-down-base-bol py-" ele "-re))\n"))))
  (insert "\n;; python-components-up-down ends here
\(provide 'python-components-up-down)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-variables-test ()
  (interactive)
  (set-buffer (get-buffer-create "Py-Variables-Test"))
  (erase-buffer)
  (dolist (elt py-variables)
    (setq elt (prin1-to-string elt))
    ;; -eval "(assert (commandp 'py-execute-file-bpython-dedicated-switch) nil \"py-execute-file-bpython-dedicated-switch not detected as command\")" \
    (insert (concat "-eval \"(assert (boundp '" elt ") nil \\\"" elt " not a variable\\\")\" \\\n")))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-docstring-style-test ()
  (interactive)
  (set-buffer (get-buffer-create "Py-Docstring-Style-Test"))
  (erase-buffer)
  (dolist (elt docstring-styles)
    ;; (setq elt (prin1-to-string elt))
    ;; py-nil-docstring-style-on
    (insert (concat "
  \(py-" elt "-docstring-style-on)
  \(assert (eq '" elt " py-docstring-style) nil \"" elt " not py-docstring-style\")\n")))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun emen (&optional symbol)
  "Provide menu draft. "
  (interactive "*")
  (let* ((erg (or symbol (car kill-ring)))
         (name (intern-soft erg))
         (doku (if (functionp name)
                   (documentation name)
                 (documentation-property name 'variable-documentation))))
    ;; (goto-char (point-max))
    (switch-to-buffer (current-buffer))
    (save-excursion
      (insert (concat "\n\[\"" (replace-regexp-in-string "-" " " (replace-regexp-in-string "py-" "" erg)) "\" " erg "
 :help \" `" erg "'
\n"))
      (when doku (insert (regexp-quote doku)))

      (insert (concat
               "\"]\n")))
    (skip-chars-forward "[[:punct:]]")
    (capitalize-word 1)))

(defun temen (&optional symbol)
  "Provide menu for toggle-commands using checkbox. "
  (interactive "*")
  (let* ((erg (or symbol (car kill-ring)))
         (name (intern-soft erg))
         (doku (if (functionp name)
                   (documentation name)
                 (documentation-property name 'variable-documentation)))
         (banner1 (replace-regexp-in-string "-" " " (replace-regexp-in-string "py-" "" erg)))
         (banner2 (replace-regexp-in-string " p$" " " (replace-regexp-in-string "py-" "" banner1))))
    ;; (goto-char (point-max))
    (switch-to-buffer (current-buffer))
    (save-excursion
      (insert (concat "\n\[\"" banner2 "\"
  (setq " erg "
     (not " erg "))
 :help \""))
      (when doku (insert (regexp-quote doku)))
      (insert (concat "Use `M-x customize-variable' to set it permanently\"
 :style toggle :selected " erg "]\n")))
    (skip-chars-forward "[[:punct:]]")
    (capitalize-word 1)))
    ;; ["Hide comments when hiding all"
    ;;  (setq hs-hide-comments-when-hiding-all
    ;; 	   (not hs-hide-comments-when-hiding-all))
    ;;  :help "If t also hide comment blocks when doing `hs-hide-all'"
    ;;  :style toggle :selected hs-hide-comments-when-hiding-all]

(defun switch-emen (&optional symbol)
  "Provide menu draft for switches. "
  (interactive "*")
  (let* ((erg (or symbol (car kill-ring)))
         (name (intern-soft erg))
         (doku (if (functionp name)
                   (documentation name)
                 (documentation-property name 'variable-documentation))))
    (switch-to-buffer (current-buffer))
    (save-excursion
      ;; ("py-switch-buffers-on-execute-p"
      ;; :help "Toggle `py-switch-buffers-on-execute-p'"
      (insert (concat "(\"" (replace-regexp-in-string "-" " " (replace-regexp-in-string "py-" "" erg)) "\"
 :help \"Toggle `" erg "'\"
")))
    (capitalize-word 1)
    (goto-char (point-max))
    (emen (concat "toggle-" symbol))
    (goto-char (point-max))
    (emen (concat symbol "-on"))
    (goto-char (point-max))
    (emen (concat symbol "-off"))
    (goto-char (point-max))
    (insert "\n)\n"))
  (emacs-lisp-mode))

(defun write-py-comment-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-comment.el"))
  (erase-buffer)
  (insert ";;; python-components-comment.el -- Comment/uncomment python constructs at point\n")
  (insert arkopf)
  (insert "
\(defun py-comment-region (beg end &optional arg)
  \"Like `comment-region' but uses double hash (`#') comment starter.\"
  (interactive \"r\\nP\")
  (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start)))
    (comment-region beg end arg)))\n
")
  (dolist (ele py-comment-forms)
    (insert (concat "(defun py-comment-" ele " (&optional beg end arg)
  \"Comments " ele " at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default\"
  (interactive \"\*\")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-" ele "-position)))
          (end (or end (py-end-of-" ele "-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))\n\n")))
  (insert "\n;; python-components-comment ends here
\(provide 'python-components-comment)")

  ;; (switch-to-buffer (current-buffer))
    (eval-buffer)
  (emacs-lisp-mode)

  (set-buffer (get-buffer-create "Menu-Python-Components-Comments"))
  (erase-buffer)
  (insert "(\"Comment ... \"
            :help \"Comment forms\"\n\n")

  (switch-to-buffer (current-buffer))
  (dolist (ele py-comment-forms)
    (setq name (concat "py-comment-" ele))
    (write-menu-entry name))
  (insert "      ))")
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

;;;

(defun py-write-mark-bol ()
  (interactive)
    (set-buffer (get-buffer-create "mark-bol.el"))
    (erase-buffer)
    (dolist (ele py-re-forms-names)
      (insert (concat "
\(defun py-mark-" ele "-bol ()
  \"Mark " ele ", take beginning of line positions.

Returns beginning and end positions of region, a cons. \"
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol \"" ele "\"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))
")))
    (switch-to-buffer (current-buffer))
    (emacs-lisp-mode))

(defun py-write-mark-bol-menu ()
  (interactive)
    (set-buffer (get-buffer-create "mark-bol-menu.el"))
    (erase-buffer)
    (dolist (ele py-re-forms-names)
      (insert (concat "
             [\"Mark " ele " bol\" py-mark-" ele "-bol
              :help \"`py-mark-" ele "-bol'
Mark " ele " at point reaching beginning-of-line. \"]
")))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

;; (defun py-beginning-of-block (&optional indent)
;;   "Goto beginning of statement where block starts.
;;   Returns position reached, if successful, nil otherwise."
;;   (interactive)
;;   (py--beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p)))

(defun py-write-beginning-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-beginning-forms.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-beginning-forms.el --- Forms start described by a regular-expression \n")
  (insert arkopf)
  (dolist (ele py-re-forms-names)
    (insert (concat "
\(defun py-beginning-of-" ele " (&optional indent)"
"\n \"Go to beginning " ele ", skip whitespace at BOL.

Returns beginning of " ele " if successful, nil otherwise\n
"))
    (when (string-match "def\\|class" ele)
      (insert "When `py-mark-decorators' is non-nil, decorators are considered too.\n\n"))
    (insert "Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"\n")
    (insert "  (interactive)")
    (cond ((string-match "def\\|class" ele)
           (insert (concat "
  (py--beginning-of-prepare indent 'py-" ele "-re 'py-extended-block-or-clause-re (interactive-p)))\n")))
          ((string-match "clause" ele)
           (insert (concat "
  (py--beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))\n")))
          (t (insert (concat "
  (py--beginning-of-prepare indent 'py-" ele "-re 'py-clause-re (interactive-p)))\n")))))
  ;; lc forms
  (dolist (ele py-re-forms-names)
    (insert (concat "
\(defalias 'py-beginning-of-" ele "-lc 'py-beginning-of-" ele"-bol)
\(defun py-beginning-of-" ele "-bol (&optional indent)"
"\n \"Go to beginning " ele ", go to BOL.

Returns beginning of " ele " if successful, nil otherwise\n
"))
    (when (string-match "def\\|class" ele)
      (insert "When `py-mark-decorators' is non-nil, decorators are considered too.\n\n"))
    (insert "Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"\n")
    (insert "  (interactive)")
    (cond ((string-match "def\\|class" ele)
           (insert (concat "
  (py--beginning-of-prepare indent 'py-" ele "-re 'py-extended-block-or-clause-re (interactive-p) t))\n")))
          ((string-match "clause" ele)
           (insert (concat "
  (py--beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))\n")))
          (t (insert (concat "
  (py--beginning-of-prepare indent 'py-" ele "-re 'py-clause-re (interactive-p) t))\n")))))
  (insert "\n(provide 'python-components-beginning-forms)
;; python-components-beginning-forms.el ends here\n")

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-delete-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-delete.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-delete.el --- Delete \n")
  (insert arkopf)
  (dolist (ele py-delete-forms)
    (insert (concat "
\(defun py-delete-" ele " ()
  \"Delete " (upcase ele) " at point.

Don't store data in kill ring. \"
  (interactive \"\*\")
  (let ((erg (py-mark-base \"" ele "\")))
    (delete-region (car erg) (cdr erg))))
")))
  (insert "\n;; python-components-delete ends here
\(provide 'python-components-delete)")

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-delete-menu ()
  (interactive)
    (set-buffer (get-buffer-create "Delete-menu.el"))
    (erase-buffer)
    (insert "         (\"Delete\"")
    (dolist (ele py-delete-forms)
      (insert (concat "
             [\"Delete " ele " \" py-delete-" ele "
              :help \"`py-delete-" ele "'
Delete " (upcase ele) " at point, don't store in kill-ring. \"]
")))
    (insert "          )\n        \"-\"\n")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-copy-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-copy.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-copy.el --- Copy \n")
  (insert arkopf)
  (dolist (ele py-copy-forms)
    (insert (concat "
\(defun py-copy-" ele " ()
  \"Copy " ele " at point.

Store data in kill ring, so it might yanked back. \"
  (interactive \"\*\")
  (let ((erg (py-mark-base \"" ele "\")))
    (copy-region-as-kill (car erg) (cdr erg))))
")))
  (insert "\n;; python-components-copy ends here
\(provide 'python-components-copy)")

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-forms-code ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-forms-code.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-forms-code.el --- Return Python forms' code \n")
  (insert arkopf)
  (dolist (ele py-copy-forms)
    (insert (concat "
\(defun py-" ele " ()
  \"" (capitalize ele) " at point.

Return code of `py-" ele "' at point, a string. \"
  (interactive)
  (let ((erg (py--base \"" ele "\")))
    (buffer-substring-no-properties (car erg) (cdr erg))))
")))
  (insert "\n;; python-components-forms-code.el ends here
\(provide 'python-components-forms-code)")

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-copy-bol-menu ()
  (interactive "*")
  (dolist (ele py-bol-copy-forms)
    (emen (prin1-to-string ele))
    (skip-chars-forward "^]")
    (forward-char 1)
    (newline)))

(defun py-write-bol-menu ()
  (interactive "*")
  (dolist (ele py-bol-forms)
    (emen (prin1-to-string ele))
    (skip-chars-forward "^]")
    (forward-char 1)
    (newline)))

(defun py-write-bol-end-menu ()
  (interactive "*")
  (dolist (ele py-bol-end-forms)
    (emen (prin1-to-string ele))
    (skip-chars-forward "^]")
    (forward-char 1)
    (newline)))

(defun py-write-fast-menu ()
  (interactive "*")
  (dolist (ele py-fast-forms)
    (push-mark)
    (emen (prin1-to-string ele))
    (skip-chars-forward "^]")
    (forward-char 1)
    (newline)
    (indent-region (mark) (point))))


(defun py-write-hide-forms ()
  (interactive "*")
  (set-buffer (get-buffer-create "python-components-hide-show.el"))
  (erase-buffer)
  (insert ";;; python-components-hide-show.el --- Provide hs-minor-mode forms\n")
  (insert arkopf)
  (insert"
\;; (setq hs-block-start-regexp 'py-extended-block-or-clause-re)
\;; (setq hs-forward-sexp-func 'py-end-of-block)

\(defun py-hide-base (form &optional beg end)
  \"Hide visibility of existing form at point. \"
  (hs-minor-mode 1)
  (save-excursion
    (let\* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat \"py-beginning-of-\" form \"-p\")))
			    (funcall (intern-soft (concat \"py-beginning-of-\" form))))))
	   (end (or end (funcall (intern-soft (concat \"py-end-of-\" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-make-overlay beg end 'code)
	    (set-buffer-modified-p modified))
	(error (concat \"No \" (format \"%s\" form) \" at point!\"))))))

\(defun py-show-base (form &optional beg end)
  \"Remove invisibility of existing form at point. \"
  (save-excursion
    (let\* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat \"py-beginning-of-\" form \"-p\")))
			    (funcall (intern-soft (concat \"py-beginning-of-\" form))))))
	   (end (or end (funcall (intern-soft (concat \"py-end-of-\" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-discard-overlays beg end)
	    (set-buffer-modified-p modified))
	(error (concat \"No \" (format \"%s\" form) \" at point!\"))))))

\(defun py-hide-show (&optional form beg end)
  \"Toggle visibility of existing forms at point. \"
  (interactive)
  (save-excursion
    (let\* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat \"py-beginning-of-\" form \"-p\")))
			    (funcall (intern-soft (concat \"py-beginning-of-\" form))))))
	   (end (or end (funcall (intern-soft (concat \"py-end-of-\" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (if (overlays-in beg end)
	      (hs-discard-overlays beg end)
	    (hs-make-overlay beg end 'code))
	(error (concat \"No \" (format \"%s\" form) \" at point!\")))
      (set-buffer-modified-p modified))))

\(defun py-hide-region (beg end)
  \"Hide active region. \"
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-hide-base 'region beg end))

\(defun py-show-region (beg end)
  \"Un-hide active region. \"
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-show-base 'region beg end))
")
  (dolist (ele py-hide-names)
    (insert (concat "
\(defun py-hide-" ele " ()
  \"Hide " ele " at point. \"
  (interactive)
  (py-hide-base '" ele "))

\(defun py-show-" ele " ()
  \"Show " ele " at point. \"
  (interactive)
  (py-show-base '" ele "))
")))

  (insert "\n;; python-components-hide-show.el ends here
\(provide 'python-components-hide-show)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))


(defun py-write-hide-menu ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-hide-Menu.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
  (insert "(\"Hide-Show\"")

  (dolist (ele py-hide-names)
    (emen (concat "py-hide-" ele))
    (goto-char (point-max))
    )
  (insert "(\"Show\"\n")

  (dolist (ele py-hide-names)
    (emen (concat "py-show-" ele))
    (goto-char (point-max))

    )

  (insert "))\n")
  
  (richten nil (point-min) (point-max))

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

;;; Copying
