;;; python-mode-test.el --- tests for Emacs python-mode.el

;; Copyright (C) 2011  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: lisp, languages

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

;; A couple of test cases for python-mode.el

;;; Code:
(require 'py-bug-numbered-tests)

(defvar py-variables (list 'py-version 'py-indent-offset 'pdb-path 'py-verbose-p 'py-load-pymacs-p 'py-smart-operator-mode-p 'py-sexp-function 'py-autopair-mode 'py-no-completion-calls-dabbrev-expand-p 'py-indent-no-completion-p 'py-fontify-shell-buffer-p 'py-modeline-display-full-path-p 'py-modeline-acronym-display-home-p 'py-install-directory 'py-guess-py-install-directory-p 'py-extensions 'py-hide-show-minor-mode-p 'empty-comment-line-separates-paragraph-p 'py-org-cycle-p 'py-outline-minor-mode-p 'py-outline-mode-keywords 'py-start-run-py-shell 'py-start-run-ipython-shell 'py-close-provides-newline 'py-dedent-keep-relative-column 'py-indent-honors-inline-comment 'py-closing-list-dedents-bos 'py-electric-colon-active-p 'py-electric-colon-greedy-p 'py-electric-colon-newline-and-indent-p 'py-electric-comment-p 'py-electric-comment-add-space-p 'py-mark-decorators 'py-tab-indent 'py-complete-function 'py-encoding-string 'py-shebang-startstring 'py-python-command-args 'py-jython-command-args 'py-cleanup-temporary 'py-lhs-inbound-indent 'py-continuation-offset 'py-indent-tabs-mode 'py-smart-indentation 'py-block-comment-prefix 'py-indent-comments 'py-separator-char 'py-custom-temp-directory 'py-jump-on-exception 'py-ask-about-save 'py-pdbtrack-do-tracking-p 'py-pdbtrack-filename-mapping 'py-pdbtrack-minor-mode-string 'py-import-check-point-max 'py-jython-packages 'py-current-defun-show 'py-current-defun-delay 'py-honor-IPYTHONDIR-p 'py-ipython-history 'py-honor-PYTHONHISTORY-p 'py-master-file 'py-pychecker-command 'py-pychecker-command-args 'py-pep8-command 'py-pep8-command-args 'py-pyflakespep8-command 'py-pep8-command 'py-pep8-command-args 'py-pyflakespep8-command-args 'py-pyflakes-command 'py-pyflakes-command-args 'py-pep8-command-args 'py-pylint-command 'py-pylint-command-args 'py-shell-input-prompt-1-regexp 'py-shell-input-prompt-2-regexp 'py-shell-prompt-read-only 'py-switch-buffers-on-execute-p 'py-split-windows-on-execute-p 'py-max-split-windows 'py-split-windows-on-execute-function 'py-hide-show-keywords 'py-hide-show-hide-docstrings 'python-mode-hook 'py--imenu-create-index-p 'py-imenu-create-index-function 'py-shell-name 'py-shell-toggle-1 'py-shell-toggle-2 'py-match-paren-mode 'py-kill-empty-line 'py-remove-cwd-from-path 'py-imenu-show-method-args-p 'py-history-filter-regexp 'py-use-local-default 'py-shell-local-path 'py-underscore-word-syntax-p 'py-edit-only-p 'py-force-py-shell-name-p 'python-mode-v5-behavior-p 'py-trailing-whitespace-smart-delete-p 'py--warn-tmp-files-left-p 'py-ipython-execute-delay 'strip-chars-before 'strip-chars-after 'py-fill-docstring-style 'py-number-face 'py-XXX-tag-face 'py-pseudo-keyword-face 'py-variable-name-face 'py-decorators-face 'py-builtins-face 'py-class-name-face 'py-exception-name-face 'python-mode-message-string 'py-local-command 'py-local-versioned-command 'py-shell-complete-debug 'py-encoding-string-re 'symbol-definition-start-re 'symbol-definition-start-re 'py-shebang-regexp 'py-separator-char 'py-temp-directory 'py-exec-command 'py-exec-string-command 'py-which-bufname 'py-pychecker-history 'py-pep8-history 'py-pyflakespep8-history 'py-pyflakes-history 'py-pylint-history 'ipython-de-input-prompt-regexp 'ipython-de-input-prompt-regexp 'py-force-local-shell-p 'py-bol-forms-last-indent 'python-mode-syntax-table 'outline-heading-end-regexp 'eldoc-documentation-function 'py-completion-last-window-configuration 'ipython-version 'py-shell-template 'py-execute-directory 'py-use-current-dir-when-execute-p 'py-exception-buffer 'py-output-buffer 'py-string-delim-re 'py-labelled-re 'py-expression-skip-regexp 'py-expression-skip-chars 'py-expression-looking-re 'py-not-expression-regexp 'py-not-expression-chars 'py-not-expression-chars 'py-partial-expression-skip-chars 'py-partial-expression-forward-regexp 'py-partial-expression-skip-backward-chars 'py-not-partial-expression-skip-chars 'py-partial-expression-looking-regexp 'py-not-partial-expression-regexp 'py-operator-regexp 'py-assignment-regexp 'py-delimiter-regexp 'py-line-number-offset 'match-paren-no-use-syntax-pps 'py-traceback-line-re 'py-traceback-line-re 'py-mode-syntax-table 'py-file-queue 'python-mode-abbrev-table 'py-pdbtrack-input-prompt 'py-pydbtrack-input-prompt 'py-pdbtrack-is-tracking-p 'py-shell-map 'py-font-lock-keywords 'py-dotted-expression-syntax-table 'jython-mode-hook 'py-shell-hook 'ipython-completion-command-string 'ipython0.10-completion-command-string 'ipython0.11-completion-command-string 'py-last-exeption-buffer 'python-preoutput-result 'py-imenu-class-regexp 'py-imenu-method-regexp 'py-imenu-method-no-arg-parens 'py-imenu-method-arg-parens 'py-imenu-generic-expression 'py-imenu-generic-regexp 'py-imenu-generic-parens 'py-mode-output-map 'py-menu 'py-already-guessed-indent-offset 'python-mode-map 'skeleton-further-elements 'virtualenv-workon-home 'virtualenv-name 'python-mode-syntax-table 'python-dotty-syntax-table 'py-shell-template 'py-block-closing-keywords-re 'py-finally-re 'py-except-re 'py-else-re 'py-no-outdent-re 'py-assignment-re 'py-block-re 'py-minor-block-re 'py-try-block-re 'py-class-re 'py-def-or-class-re 'py-def-re 'py-block-or-clause-re 'py-extended-block-or-clause-re 'py-clause-re 'py-elif-re 'py-if-re 'py-try-re 'py-mode-syntax-table 'py-pdbtrack-stack-entry-regexp 'py-pdbtrack-input-prompt 'py-pydbtrack-input-prompt 'py-pdbtrack-marker-regexp-file-group 'py-pdbtrack-marker-regexp-line-group 'py-pdbtrack-marker-regexp-funcname-group 'py-pdbtrack-track-range 'py-compilation-regexp-alist 'py-font-lock-syntactic-keywords 'virtualenv-name )
  "Used for bug-tracking by `py-list-settings'")

(defun py-list-settings ()
  "List py-variables with it's current value.

  For debugging "
  (interactive)
  (set-buffer (get-buffer-create "Python-mode-el-settings"))
  (erase-buffer)
  (load (concat (py--normalize-directory py-install-directory) "devel/python-mode-vars.el") nil t)
  (dolist (elt py-variables)
    (insert (concat (prin1-to-string elt) " ==> "))
    (if (stringp (ignore-errors (symbol-value elt)))
        (insert (concat (symbol-value elt) "\n\n"))
      (insert (concat (prin1-to-string (ignore-errors (symbol-value elt))) "\n\n"))))
  ;; (switch-to-buffer (current-buffer))
  )

(setq python-mode-tests
      (list
       'docstring-style-switches-test
       'py-nested-block-or-clause-test
       'py-down-statement-test
       'py-fill-string-django-test
       'py-fill-string-onetwo-test
       'py-fill-string-pep-257-test
       'py-fill-string-pep-257-nn-test
       ;;  fails for unknown reasons, interactive call works
       ;; 'py-fill-string-symmetric-test
       'py-highlight-indentation-test
       'py-smart-indentation-test
       'autopair-mode-test
       'py-smart-operator-test
       'py-guess-indent-offset-test
       'py-bol-moves-test
       ;; broken
       ;; 'augmented-assigment-test
       'key-binding-tests
       'py-end-of-statement-test
       'py-compute-indentation-test
       'py-end-of-def-inline-comment-test
       'before-inline-comment-test
       'toggle-force-py-shell-name-p-test
       'py-execute-statement-python-test
       'py-execute-statement-python-switch-test
       'py-execute-statement-python-noswitch-test
       'py-execute-statement-python-dedicated-test
       'py-execute-statement-python-dedicated-switch-test
       'py-execute-statement-ipython-test
       'py-execute-statement-ipython-switch-test
       'py-execute-statement-ipython-noswitch-test
       'py-execute-statement-ipython-dedicated-test
       'py-execute-statement-ipython-dedicated-switch-test
       'py-execute-statement-python3-test
       'py-execute-statement-python3-switch-test
       'py-execute-statement-python3-noswitch-test
       'py-execute-statement-python3-dedicated-test
       'py-execute-statement-python3-dedicated-switch-test
       'py-execute-statement-python2-test
       'py-execute-statement-python2-switch-test
       'py-execute-statement-python2-noswitch-test
       'py-execute-statement-python2-dedicated-test
       'py-execute-statement-python2-dedicated-switch-test
       'py-execute-statement-python2.7-test
       'py-execute-statement-python2.7-switch-test
       'py-execute-statement-python2.7-noswitch-test
       'py-execute-statement-python2.7-dedicated-test
       'py-execute-statement-python2.7-dedicated-switch-test
       'py-execute-statement-jython-test
       'py-execute-statement-jython-switch-test
       'py-execute-statement-jython-noswitch-test
       'py-execute-statement-jython-dedicated-test
       'py-execute-statement-jython-dedicated-switch-test
       'py-separator-char-test
       'py-execute-block-python-switch-test
       'py-execute-block-python-noswitch-test
       'py-execute-block-python-dedicated-test
       'py-execute-block-python-dedicated-switch-test
       'py-execute-block-ipython-test
       'py-execute-block-ipython-switch-test
       'py-execute-block-ipython-noswitch-test
       'py-execute-block-ipython-dedicated-test
       'py-execute-block-ipython-dedicated-switch-test
       'py-execute-block-python3-test
       'py-execute-block-python3-switch-test
       'py-execute-block-python3-noswitch-test
       'py-execute-block-python3-dedicated-test
       'py-execute-block-python3-dedicated-switch-test
       'py-execute-block-python2-test
       'py-execute-block-python2-switch-test
       'py-execute-block-python2-noswitch-test
       'py-execute-block-python2-dedicated-test
       'py-execute-block-python2-dedicated-switch-test
       'py-execute-block-python2.7-test
       'py-execute-block-python2.7-switch-test
       'py-execute-block-python2.7-noswitch-test
       'py-execute-block-python2.7-dedicated-test
       'py-execute-block-python2.7-dedicated-switch-test
       'py-execute-block-jython-test
       'py-execute-block-jython-switch-test
       'py-execute-block-jython-noswitch-test
       'py-execute-block-jython-dedicated-test
       'py-execute-block-jython-dedicated-switch-test
       'py-execute-block-or-clause-python-test
       'py-execute-block-or-clause-python-switch-test
       'py-execute-block-or-clause-python-noswitch-test
       'py-execute-block-or-clause-python-dedicated-test
       'py-execute-block-or-clause-python-dedicated-switch-test
       'py-execute-block-or-clause-ipython-test
       'py-execute-block-or-clause-ipython-switch-test
       'py-execute-block-or-clause-ipython-noswitch-test
       'py-execute-block-or-clause-ipython-dedicated-test
       'py-execute-block-or-clause-ipython-dedicated-switch-test
       'py-execute-block-or-clause-python3-test
       'py-execute-block-or-clause-python3-switch-test
       'py-execute-block-or-clause-python3-noswitch-test
       'py-execute-block-or-clause-python3-dedicated-test
       'py-execute-block-or-clause-python3-dedicated-switch-test
       'py-execute-block-or-clause-python2-test
       'py-execute-block-or-clause-python2-switch-test
       'py-execute-block-or-clause-python2-noswitch-test
       'py-execute-block-or-clause-python2-dedicated-test
       'py-execute-block-or-clause-python2-dedicated-switch-test
       'py-execute-block-or-clause-python2.7-test
       'py-execute-block-or-clause-python2.7-switch-test
       'py-execute-block-or-clause-python2.7-noswitch-test
       'py-execute-block-or-clause-python2.7-dedicated-test
       'py-execute-block-or-clause-python2.7-dedicated-switch-test
       'py-execute-block-or-clause-jython-test
       'py-execute-block-or-clause-jython-switch-test
       'py-execute-block-or-clause-jython-noswitch-test
       'py-execute-block-or-clause-jython-dedicated-test
       'py-execute-block-or-clause-jython-dedicated-switch-test
       'py-execute-def-python-test
       'py-execute-def-python-switch-test
       'py-execute-def-python-noswitch-test
       'py-execute-def-python-dedicated-test
       'py-execute-def-python-dedicated-switch-test
       'py-execute-def-ipython-test
       'py-execute-def-ipython-switch-test
       'py-execute-def-ipython-noswitch-test
       'py-execute-def-ipython-dedicated-test
       'py-execute-def-ipython-dedicated-switch-test
       'py-execute-def-python3-test
       'py-execute-def-python3-switch-test
       'py-execute-def-python3-noswitch-test
       'py-execute-def-python3-dedicated-test
       'py-execute-def-python3-dedicated-switch-test
       'py-execute-def-python2-test
       'py-execute-def-python2-switch-test
       'py-execute-def-python2-noswitch-test
       'py-execute-def-python2-dedicated-test
       'py-execute-def-python2-dedicated-switch-test
       'py-execute-def-python2.7-test
       'py-execute-def-python2.7-switch-test
       'py-execute-def-python2.7-noswitch-test
       'py-execute-def-python2.7-dedicated-test
       'py-execute-def-python2.7-dedicated-switch-test
       'py-execute-def-jython-test
       'py-execute-def-jython-switch-test
       'py-execute-def-jython-noswitch-test
       'py-execute-def-jython-dedicated-test
       'py-execute-def-jython-dedicated-switch-test
       'py-execute-class-python-test
       'py-execute-class-python-switch-test
       'py-execute-class-python-noswitch-test
       'py-execute-class-python-dedicated-test
       'py-execute-class-python-dedicated-switch-test
       'py-execute-class-ipython-test
       'py-execute-class-ipython-switch-test
       'py-execute-class-ipython-noswitch-test
       'py-execute-class-ipython-dedicated-test
       'py-execute-class-ipython-dedicated-switch-test
       'py-execute-class-python3-test
       'py-execute-class-python3-switch-test
       'py-execute-class-python3-noswitch-test
       'py-execute-class-python3-dedicated-test
       'py-execute-class-python3-dedicated-switch-test
       'py-execute-class-python2-test
       'py-execute-class-python2-switch-test
       'py-execute-class-python2-noswitch-test
       'py-execute-class-python2-dedicated-test
       'py-execute-class-python2-dedicated-switch-test
       'py-execute-class-python2.7-test
       'py-execute-class-python2.7-switch-test
       'py-execute-class-python2.7-noswitch-test
       'py-execute-class-python2.7-dedicated-test
       'py-execute-class-python2.7-dedicated-switch-test
       'py-execute-class-jython-test
       'py-execute-class-jython-switch-test
       'py-execute-class-jython-noswitch-test
       'py-execute-class-jython-dedicated-test
       'py-execute-class-jython-dedicated-switch-test
       'py-execute-region-python-test
       'py-execute-region-python-switch-test
       'py-execute-region-python-noswitch-test
       'py-execute-region-python-dedicated-test
       'py-execute-region-python-dedicated-switch-test
       'py-execute-region-ipython-test
       'py-execute-region-ipython-switch-test
       'py-execute-region-ipython-noswitch-test
       'py-execute-region-ipython-dedicated-test
       'py-execute-region-ipython-dedicated-switch-test
       'py-execute-region-python3-test
       'py-execute-region-python3-switch-test
       'py-execute-region-python3-noswitch-test
       'py-execute-region-python3-dedicated-test
       'py-execute-region-python3-dedicated-switch-test
       'py-execute-region-python2-test
       'py-execute-region-python2-switch-test
       'py-execute-region-python2-noswitch-test
       'py-execute-region-python2-dedicated-test
       'py-execute-region-python2-dedicated-switch-test
       'py-execute-region-python2.7-test
       'py-execute-region-python2.7-switch-test
       'py-execute-region-python2.7-noswitch-test
       'py-execute-region-python2.7-dedicated-test
       'py-execute-region-python2.7-dedicated-switch-test
       'py-execute-region-jython-test
       'py-execute-region-jython-switch-test
       'py-execute-region-jython-noswitch-test
       'py-execute-region-jython-dedicated-test
       'py-execute-region-jython-dedicated-switch-test
       'py-execute-buffer-python-test
       'py-execute-buffer-python-switch-test
       'py-execute-buffer-python-noswitch-test
       'py-execute-buffer-python-dedicated-test
       'py-execute-buffer-python-dedicated-switch-test
       'py-execute-buffer-ipython-test
       'py-execute-buffer-ipython-switch-test
       'py-execute-buffer-ipython-noswitch-test
       'py-execute-buffer-ipython-dedicated-test
       'py-execute-buffer-ipython-dedicated-switch-test
       'py-execute-buffer-python3-test
       'py-execute-buffer-python3-switch-test
       'py-execute-buffer-python3-noswitch-test
       'py-execute-buffer-python3-dedicated-test
       'py-execute-buffer-python3-dedicated-switch-test
       'py-execute-buffer-python2-test
       'py-execute-buffer-python2-switch-test
       'py-execute-buffer-python2-noswitch-test
       'py-execute-buffer-python2-dedicated-test
       'py-execute-buffer-python2-dedicated-switch-test
       'py-execute-buffer-python2.7-test
       'py-execute-buffer-python2.7-switch-test
       'py-execute-buffer-python2.7-noswitch-test
       'py-execute-buffer-python2.7-dedicated-test
       'py-execute-buffer-python2.7-dedicated-switch-test
       'py-execute-buffer-jython-test
       'py-execute-buffer-jython-switch-test
       'py-execute-buffer-jython-noswitch-test
       'py-execute-buffer-jython-dedicated-test
       'py-execute-buffer-jython-dedicated-switch-test
       'py-execute-expression-python-test
       'py-execute-expression-python-switch-test
       'py-execute-expression-python-noswitch-test
       'py-execute-expression-python-dedicated-test
       'py-execute-expression-python-dedicated-switch-test
       'py-execute-expression-ipython-test
       'py-execute-expression-ipython-switch-test
       'py-execute-expression-ipython-noswitch-test
       'py-execute-expression-ipython-dedicated-test
       'py-execute-expression-ipython-dedicated-switch-test
       'py-execute-expression-python3-test
       'py-execute-expression-python3-switch-test
       'py-execute-expression-python3-noswitch-test
       'py-execute-expression-python3-dedicated-test
       'py-execute-expression-python3-dedicated-switch-test
       'py-execute-expression-python2-test
       'py-execute-expression-python2-switch-test
       'py-execute-expression-python2-noswitch-test
       'py-execute-expression-python2-dedicated-test
       'py-execute-expression-python2-dedicated-switch-test
       'py-execute-expression-python2.7-test
       'py-execute-expression-python2.7-switch-test
       'py-execute-expression-python2.7-noswitch-test
       'py-execute-expression-python2.7-dedicated-test
       'py-execute-expression-python2.7-dedicated-switch-test
       'py-execute-expression-jython-test
       'py-execute-expression-jython-switch-test
       'py-execute-expression-jython-noswitch-test
       'py-execute-expression-jython-dedicated-test
       'py-execute-expression-jython-dedicated-switch-test
       'py-execute-partial-expression-python-test
       'py-execute-partial-expression-python-switch-test
       'py-execute-partial-expression-python-noswitch-test
       'py-execute-partial-expression-python-dedicated-test
       'py-execute-partial-expression-python-dedicated-switch-test
       'py-execute-partial-expression-ipython-test
       'py-execute-partial-expression-ipython-switch-test
       'py-execute-partial-expression-ipython-noswitch-test
       'py-execute-partial-expression-ipython-dedicated-test
       'py-execute-partial-expression-ipython-dedicated-switch-test
       'py-execute-partial-expression-python3-test
       'py-execute-partial-expression-python3-switch-test
       'py-execute-partial-expression-python3-noswitch-test
       'py-execute-partial-expression-python3-dedicated-test
       'py-execute-partial-expression-python3-dedicated-switch-test
       'py-execute-partial-expression-python2-test
       'py-execute-partial-expression-python2-switch-test
       'py-execute-partial-expression-python2-noswitch-test
       'py-execute-partial-expression-python2-dedicated-test
       'py-execute-partial-expression-python2-dedicated-switch-test
       'py-execute-partial-expression-python2.7-test
       'py-execute-partial-expression-python2.7-switch-test
       'py-execute-partial-expression-python2.7-noswitch-test
       'py-execute-partial-expression-python2.7-dedicated-test
       'py-execute-partial-expression-python2.7-dedicated-switch-test
       'py-execute-partial-expression-jython-test
       'py-execute-partial-expression-jython-switch-test
       'py-execute-partial-expression-jython-noswitch-test
       'py-execute-partial-expression-jython-dedicated-test
       'py-execute-partial-expression-jython-dedicated-switch-test
       'py-execute-line-python-test
       'py-execute-line-python-switch-test
       'py-execute-line-python-noswitch-test
       'py-execute-line-python-dedicated-test
       'py-execute-line-python-dedicated-switch-test
       'py-execute-line-ipython-test
       'py-execute-line-ipython-switch-test
       'py-execute-line-ipython-noswitch-test
       'py-execute-line-ipython-dedicated-test
       'py-execute-line-ipython-dedicated-switch-test
       'py-execute-line-python3-test
       'py-execute-line-python3-switch-test
       'py-execute-line-python3-noswitch-test
       'py-execute-line-python3-dedicated-test
       'py-execute-line-python3-dedicated-switch-test
       'py-execute-line-python2-test
       'py-execute-line-python2-switch-test
       'py-execute-line-python2-noswitch-test
       'py-execute-line-python2-dedicated-test
       'py-execute-line-python2-dedicated-switch-test
       'py-execute-line-python2.7-test
       'py-execute-line-python2.7-switch-test
       'py-execute-line-python2.7-noswitch-test
       'py-execute-line-python2.7-dedicated-test
       'py-execute-line-python2.7-dedicated-switch-test
       'py-execute-line-jython-test
       'py-execute-line-jython-switch-test
       'py-execute-line-jython-noswitch-test
       'py-execute-line-jython-dedicated-test
       'py-execute-line-jython-dedicated-switch-test

       'py-beginning-of-block-test
       'py-end-of-block-test
       'py-beginning-of-block-or-clause-test
       'py-end-of-block-or-clause-test
       'py-beginning-of-def-test
       'py-end-of-def-test
       'py-beginning-of-def-or-class-test
       'py-end-of-def-or-class-test
       'py-electric-backspace-test
       'py-electric-delete-test
       'dict-error-test
       ;;         'py-expand-abbrev-pst-pdb.set_trace-test
       'near-bob-beginning-of-statement-test
       'bob-beginning-of-statement-test
       'honor-comments-indent-test
       'assignment-indent-test
       'if-elif-test
       'if-elif-bob-test
       'try-else-clause-test
       'try-except-test
       'assignment-after-block-test
       'py-beginning-of-clause-test
       'py-end-of-clause-test
       'py-beginning-of-expression-test
       'py-end-of-expression-test
       'py-expression-index-test
       'py-indent-after-assigment-test
       'leave-dict-test
       'eofs-attribut-test
       'py-insert-super-python2-test
       'py-insert-super-python3-test
       'args-list-first-line-indent-test
       'py-partial-expression-test
       'py-execute-block-test
       'multiline-list-indent-test
       'close-block-test
       'py-shift-block-test
       'nesting-if-test
       'py-end-of-print-statement-test
       'nested-try-test
       'nested-if-test
       'nested-try-finally-test
       'py-shell-complete-test
       'python-dedicated-test
       'tqs-list-error-test
       'py-mark-def-commandp-test
       'split-windows-on-execute-p-test
       'switch-windows-on-execute-p-test
       'py-install-directory-path-test
       'UnicodeEncodeError-python3-test
       'py-execute-block-python-test

       ))

(defun py-run-tests (&optional arg)
  (interactive "p")
  (dolist (ele python-mode-tests)
    (funcall ele arg)))

(defvar python-mode-teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
        elif b:
            pass
        else b:
            pass

            ])

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
  "String used for tests by python-mode-test.el")

(setq python-mode-teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
        elif b:
            pass
        else b:
            pass

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
")

(defun py-beginning-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-block-test-base arg teststring)))

(defun py-beginning-of-block-test-base ()
  (goto-char 627)
  (py-beginning-of-block)
  (assert (eq (point) 325) nil "py-beginning-of-block-test failed"))

(defun py-end-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if foo:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])

        elif bar:
            pass
        else:
            pass
 "))
    (py-bug-tests-intern 'py-end-of-block-base arg teststring)))

(defun py-end-of-block-base ()
  (goto-char 326)
  (assert (eq 562 (py-end-of-clause)) nil "py-end-of-block-test #1 failed")
  (assert (eq 598 (py-end-of-clause)) nil "py-end-of-block-test #2 failed")
  (assert (eq 629 (py-end-of-block)) nil "py-end-of-block-test #3 failed"))

(defun py-beginning-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-block-or-clause-base arg teststring)))

(defun py-beginning-of-block-or-clause-base ()
  (goto-char 627)
  (py-beginning-of-block-or-clause)
  (assert (looking-at "else") nil "py-beginning-of-block-or-clause-test failed")
  (py-beginning-of-block-or-clause)
  (assert (looking-at "elif") nil "py-beginning-of-block-or-clause-test failed")
  (py-beginning-of-block-or-clause)
  (assert (looking-at "if") nil "py-beginning-of-block-or-clause-test failed")

  )

(defun py-end-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-end-of-block-or-clause-base arg teststring)))

(defun py-end-of-block-or-clause-base ()
  (goto-char 602)
  (py-end-of-block-or-clause)
  (assert (eq (point) 626) nil "py-end-of-block-or-clause-test failed"))

(defun py-beginning-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-def-base arg teststring)))

(defun py-beginning-of-def-base ()
  (goto-char 627)
  (py-beginning-of-def)
  (assert (eq (point) 238) nil "py-beginning-of-def-test failed")
  )

(defun py-end-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-end-of-def-base arg teststring)))

(defun py-end-of-def-base ()
  (goto-char 627)
  (py-beginning-of-def)
  (py-end-of-def)
  (assert (eq (point) 626) nil "py-end-of-def-test failed")
  )

(defun py-beginning-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-def-or-class-base arg teststring)))

(defun py-beginning-of-def-or-class-base ()
  (goto-char 627)
  (py-beginning-of-def-or-class 4)
  (assert (eq (point) 238) nil "py-beginning-of-def-or-class-test failed"))

(defun py-end-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-end-of-def-or-class-base arg teststring)))

(defun py-end-of-def-or-class-base ()
  (goto-char 627)
  (assert (eq 238 (py-beginning-of-def-or-class)) nil "py-end-of-def-or-class-test #1 failed")
  (assert (eq 146 (py-beginning-of-def-or-class)) nil "py-end-of-def-or-class-test #2 failed")
  (goto-char 201)
  (assert (eq 232 (py-end-of-def-or-class)) nil "py-end-of-def-or-class-test #3 failed")
  (assert (eq 626 (py-end-of-def-or-class '(4))) nil "py-end-of-def-or-class-test #4 failed"))

(defun py-electric-backspace-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-electric-backspace-base arg teststring)))

(defun py-electric-backspace-base ()
  (goto-char 232)
  (py-newline-and-indent)
  (sit-for 0.1)
  (assert (eq 241 (point)) nil "py-electric-backspace-test #1 failed")
  (py-electric-backspace)
  (assert (eq 4 (current-column)) nil "py-electric-backspace-test #2 failed")
  (py-electric-backspace)
  (assert (eq 0 (current-column)) nil "py-electric-backspace-test #3 failed")
  (py-electric-backspace)
  (assert (eq 232 (point)) nil "py-electric-backspace-test #4 failed"))

(defun py-electric-delete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-electric-delete-base arg teststring)))

(defun py-electric-delete-base ()
  (goto-char 202)
  (py-electric-delete)
  (assert (eq 4 (length (progn (looking-at "[ \t]+")(match-string-no-properties 0)))) nil "py-electric-delete-test #1 failed")
  (py-electric-delete)
  (assert (not (looking-at "[ \t]+")) nil "py-electric-delete-test #2 failed")
  (py-electric-delete)
  (assert (looking-at "ict") nil "py-electric-delete-test #2 failed")
  )

(defun UnicodeEncodeError-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((py-shell-name "python3")
	(teststring (concat "#! /usr/bin/env python3
# -\*- coding: utf-8 -\*-\n
print(\'\\xA9\')
")))
    (py-bug-tests-intern 'UnicodeEncodeError-python3-base arg teststring)))

(defun UnicodeEncodeError-python3-base (&optional arg)
  (delete-other-windows)
  (py-execute-region 50 63)
  (set-buffer "*Python3*")
  ;; (switch-to-buffer (current-buffer))
  (goto-char comint-last-output-start)
  (assert (eq (char-after) ?\©) nil "UnicodeEncodeError-python3-test failed"))

(defun dict-error-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
 # -*- coding: utf-8 -*-

class foo(bar):
	\"\"\"baz\"\"\"
       	_some_extensions = {

		'38': 'asd', #  whatever
		'43': 'ddd',
		'45': 'ddd',
	}
")))
    (py-bug-tests-intern 'dict-error-base arg teststring)))

(defun dict-error-base ()
  (goto-char 78)
  (assert (eq 166 (py-end-of-statement)) nil "dict-error-test failed"))

(defun py-expand-abbrev-pst-pdb.set_trace-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print('\xA9')
pst
")))
    (py-bug-tests-intern 'py-expand-abbrev-pst-pdb.set_trace-base arg teststring)))

(defun py-expand-abbrev-pst-pdb.set_trace-base ()
  (forward-char -1)
  (expand-abbrev)
  (sit-for 1)
  ;;  (assert (string= (expand-abbrev) "pst") nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))
  ;; (assert (expand-abbrev) nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))
  (progn (looking-back "pdb.set_trace()")
         ;; (message "Looking back: %s" (match-string-no-properties 0))
         )
  (assert (looking-back "pdb.set_trace()")
          ;;          (message "%s" (match-string-no-properties 1))
          nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))

(defun near-bob-beginning-of-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
 # -*- coding: utf-8 -*-

print u'\xA9'
")))
    (py-bug-tests-intern 'near-bob-beginning-of-statement-base arg teststring)))

(defun near-bob-beginning-of-statement-base ()
  (goto-char 50)
  (assert (eq 0 (py-compute-indentation)) nil "near-bob-beginning-of-statement-test failed"))

(defun bob-beginning-of-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Foo.py
"))
    (py-bug-tests-intern 'bob-beginning-of-statement-base arg teststring)))

(defun bob-beginning-of-statement-base ()
  (py-beginning-of-statement)
  (assert (eq 1 (point))  "bob-beginning-of-statement-test failed"))

(defun honor-comments-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Something.py
    # The purpose of this program is uncertain.
"))
    (py-bug-tests-intern 'honor-comments-indent-base arg teststring)))

(defun honor-comments-indent-base ()
  (goto-char 19)
  (assert (eq 4 (py-compute-indentation)) nil "honor-comments-indent-test failed"))

(defun first-line-offset-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Something.py
    # The purpose of this program is uncertain.
"))
    (py-bug-tests-intern 'first-line-offset-base arg teststring)))

(defun first-line-offset-base ()
  (goto-char 18)
  (assert (eq 4 (py-compute-indentation)) nil "first-line-offset-test failed"))

(defun assignment-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
sammlung = []
"))
    (py-bug-tests-intern 'assignment-indent-base arg teststring)))

(defun assignment-indent-base ()
  (goto-char 12)
  (assert (eq 4 (py-compute-indentation)) nil "assignment-indent-test failed"))

(defun if-elif-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if bar in baz:
    print \"0, baz\"
    abc[1] = \"x\"

elif barr in bazz:
    print \"\"
"))
    (py-bug-tests-intern 'if-elif-base arg teststring)))

(defun if-elif-base ()
  (goto-char 76)
  (assert (eq 4 (py-compute-indentation)) nil "if-elif.py-test failed"))

(defun if-elif-bob-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if bar in baz:
    print \"0, baz\"
"))
    (py-bug-tests-intern 'if-elif-bob-base arg teststring)))

(defun if-elif-bob-base ()
  (goto-char (point-min))
  (assert (eq 0 (py-compute-indentation)) nil "if-elif-bob.py-test failed"))

(defun try-else-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
# an example from http://www.thomas-guettler.de
# © 2002-2008 Thomas Güttler. Der Text darf nach belieben kopiert und modifiziert werden, solange dieser Hinweis zum Copyright und ein Links zu dem Original unter www.thomas-guettler.de erhalten bleibt. Es wäre nett, wenn Sie mir Verbesserungsvorschläge mitteilen: guettli@thomas-guettler.de

def _commit_on_success(*args, **kw):
    begin()
    try:
        res = func(*args, **kw)
    except Exception, e:
        rollback()
        raise # Re-raise (aufgefangene Exception erneut werfen)
    else:
        commit()
    return res
"))
    (py-bug-tests-intern 'try-else-clause-base arg teststring)))

(defun try-else-clause-base ()
  (goto-char 541)
  (assert (eq 4 (py-compute-indentation)) nil "try-else-clause-test failed"))

(defun try-except-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
# an example from http://www.thomas-guettler.de
# © 2002-2008 Thomas Güttler. Der Text darf nach belieben kopiert und modifiziert werden, solange dieser Hinweis zum Copyright und ein Links zu dem Original unter www.thomas-guettler.de erhalten bleibt. Es wäre nett, wenn Sie mir Verbesserungsvorschläge mitteilen: guettli@thomas-guettler.de

def _commit_on_success(*args, **kw):
    begin()
    try:
        res = func(*args, **kw)
    except Exception, e:
        rollback()
        raise # Re-raise (aufgefangene Exception erneut werfen)
    else:
        commit()
    return res
"))
    (py-bug-tests-intern 'try-except-base arg teststring)))

(defun try-except-base ()
  (goto-char 434)
  (assert (eq 4 (py-compute-indentation)) nil "try-except-test failed"))

(defun assignment-after-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"

a = \"asdf\"
b = \"asdf\"
"))
    (py-bug-tests-intern 'assignment-after-block-base arg teststring)))

(defun assignment-after-block-base ()
  (forward-line -1)
  (assert (eq 0 (py-compute-indentation)) nil "assignment-after-block-test failed"))

(defun py-beginning-of-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg
"))
    (py-bug-tests-intern 'py-beginning-of-clause-base arg teststring)))

(defun py-beginning-of-clause-base ()
  (goto-char 364)
  (assert (eq 346 (py-beginning-of-clause)) "py-beginning-of-clause-test failed"))

(defun py-end-of-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg
"))
    (py-bug-tests-intern 'py-end-of-clause-base arg teststring)))

(defun py-end-of-clause-base ()
  (goto-char 364)
  (assert (eq 412 (py-end-of-clause)) "py-end-of-clause-test failed"))

(defun py-beginning-of-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
"))
    (py-bug-tests-intern 'py-beginning-of-expression-base arg teststring)))

(defun py-beginning-of-expression-base ()
  (goto-char 227)
  (assert (eq 221 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #1 failed")
  (assert (eq 205 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #2 failed")
  (assert (eq 177 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #3 failed")
  (assert (eq 170 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #4 failed")
  (assert (eq 116 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #5 failed")
  (assert (eq 103 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #6 failed")
  (assert (eq 90 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #7 failed")
  (assert (eq 75 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #8 failed")
  (assert (eq 65 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #9 failed")
  (assert (eq 49 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #10 failed")
  (assert (eq 45 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #11 failed"))

(defun py-end-of-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
"))
    (py-bug-tests-intern 'py-end-of-expression-base arg teststring)))

(defun py-end-of-expression-base ()
  (goto-char 49)
  (assert (eq 60 (py-end-of-expression)) nil "py-end-of-expression-test #1 failed")
  (assert (eq 72 (py-end-of-expression)) nil "py-end-of-expression-test #2 failed)")
  (assert (eq 85 (py-end-of-expression)) nil "py-end-of-expression-test #3 failed)")
  (assert (eq 94 (py-end-of-expression)) nil "py-end-of-expression-test #4 failed)")
  (assert (eq 113 (py-end-of-expression)) nil "py-end-of-expression-test #5 failed)")
  (assert (eq 165 (py-end-of-expression)) nil "py-end-of-expression-test #6 failed)")
  (assert (eq 176 (py-end-of-expression)) nil "py-end-of-expression-test #7 failed)")
  (assert (eq 196 (py-end-of-expression)) nil "py-end-of-expression-test #8 failed)")
  (assert (eq 212 (py-end-of-expression)) nil "py-end-of-expression-test #9 failed)")
  (assert (eq 232 (py-end-of-expression)) nil "py-end-of-expression-test #10 failed)"))

(defun py-expression-index-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
 # -\*- coding: utf-8 -\*-
b = a[0].split(':')[1]
")))
    (py-bug-tests-intern 'py-expression-index-base arg teststring)))

(defun py-expression-index-base ()
  (goto-char 58)
  (assert (eq 71 (py-end-of-expression)) nil "py-expression-index-test failed")
)

(defun py-insert-super-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -\*- coding: utf-8 -\*-
# As example given in Python v3.1 documentation » The Python Standard Library »
#
# class C(B):
#     def method(self, arg):
#         super().method(arg) # This does the same thing as:
#                                # super(C, self).method(arg)\"

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
        ")))
    (py-bug-tests-intern 'py-insert-super-python2-base arg teststring)))

(defun py-insert-super-python2-base ()
  (ignore-errors (py-insert-super))
  (sit-for 0.1)
  (assert (looking-back "super(OrderedDict1, self).__init__(d={})") nil "py-insert-super-python2-test failed"))

(defun py-insert-super-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let* ((py-test-shebang "#! /usr/bin/env python3")
         (teststring (concat py-test-shebang "
# -\*- coding: utf-8 -\*-
# As example given in Python v3.1 documentation » The Python Standard Library »
#
# class C(B):
#     def method(self, arg):
#         super().method(arg) # This does the same thing as:
#                                # super(C, self).method(arg)\"

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
        ")))
    (py-bug-tests-intern 'py-insert-super-python3-base arg teststring)))

(defun py-insert-super-python3-base ()
  (save-excursion
    (py-insert-super))
  (sit-for 0.2)
  (assert (looking-at "super().__init__(d={})") nil "py-insert-super-python3-test failed"))

(defun py-indent-after-assigment-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

#####################################
def foo( self, bar=False ):  # version 12345
    title = self.barz.attrs['file'].split('.')[ -1 ]
    if asdf:
")))
    (py-bug-tests-intern 'indent-after-assigment-base arg teststring)))

(defun indent-after-assigment-base ()
  (goto-char 185)
  (assert (eq 4 (py-compute-indentation)) nil "py-indent-after-assigment-test failed"))

(defun leave-dict-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
foo = {
    b\"yyyyt\": \"bxxk\",
    \"bxxk\": { \"yyer\": [\"wxrddef\", \"yytem\", \"hym\",],
              \"wfter\": [], \"xbject\": BxxkTwg, },
    \"yytem\": { \"yyer\": [], \"wfter\": [\"yytem\"], \"xbject\": ItemTwg, },
    \"hym\": { \"yyer\": [], \"wfter\": [\"hym\"], \"xbject\": ItemTwg, },
    \"yyfx\": { \"yyer\": [], \"wfter\": [\"yytem\", \"hym\"], \"xbject\": IfxTwg, },
    \"wxrddef\": { \"yyer\": [], \"wfter\": [\"yyfx\", \"yytem\", \"hym\"], \"xbject\": WxrddefTwg, },
}
"))
    (py-bug-tests-intern 'leave-dict-base arg teststring)))

(defun leave-dict-base ()
  (goto-char (point-min))
  (py-end-of-statement)
  (assert (eq 431 (point)) nil "leave-dict-test failed"))

(defun eofs-attribut-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo( baz ):  # version
    return baz.replace(\"\+\",\"§\").replace(\"_\", \" \").replace(\"ﬁ\",\"fr\").replace(
        \"ﬂ\", \"fg\").replace(\"--\", \"ü\")
"))
    (py-bug-tests-intern 'eofs-attribut-base arg teststring)))

(defun eofs-attribut-base ()
  (forward-line -2)
  (assert (eq 142 (py-end-of-statement))  nil "eofs-attribut-test failed"))

(defun args-list-first-line-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if foo:
    bar.append(
        ht(
            T.a('Sorted Foo', href='#Blub', ),
            ' -- foo bar baz--',
            self.Tasdf( afsd ),
            self.Tasdf( asdf ),
            )
    )
")))
    (py-bug-tests-intern 'args-list-first-line-indent-base arg teststring)))

(defun args-list-first-line-indent-base ()
  (goto-char 72)
  (assert (eq 4 (py-compute-indentation)) nil "args-list-first-line-indent-test failed"))

(defun py-partial-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if foo:
    bar.append(
        ht(
            T.a('Sorted Foo', href='#Blub', ),
            ' -- foo bar baz--',
            self.Tasdf( afsd ),
            self.Tasdf( asdf ),
            )
        )
")))
    (py-bug-tests-intern 'py-partial-expression-base arg teststring)))

(defun py-partial-expression-base ()
  (goto-char 104)
  (assert (eq 102 (py-beginning-of-partial-expression)) nil "py-partial-expression-test #1 failed")
  (assert (eq 108 (py-end-of-partial-expression)) nil "py-partial-expression-test #2 failed")
  (goto-char 178)
  (assert (eq 177 (py-beginning-of-partial-expression)) nil "py-partial-expression-test #3 failed")
  (assert (eq 195 (py-end-of-partial-expression)) nil "py-partial-expression-test #3 failed")
  )

(defun multiline-list-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print [1, 2,
    3, 4]"))
    (py-bug-tests-intern 'multiline-list-indent-base arg teststring)))

(defun multiline-list-indent-base ()
  (assert (eq 7 (py-compute-indentation)) nil "multiline-list-indent-test failed"))

(defun no-switch-no-split-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

print u'\\xA9'
")))
    (py-bug-tests-intern 'no-switch-no-split-base arg teststring)))

(defun no-switch-no-split-base ()
  (let ((oldbuf (current-buffer))
        py-split-windows-on-execute py-switch-buffers-on-execute-p)
    (goto-char 49)
    (push-mark)
    (end-of-line)
    (py-execute-region (line-beginning-position) (point))
    (assert (window-full-height-p) "no-switch-no-split-test failed")
    (assert (eq (current-buffer) oldbuf))))

(defun close-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
")))
    (py-bug-tests-intern 'close-block-base arg teststring)))

(defun close-block-base ()
  (goto-char 102)
  (assert (eq 4 (py-close-block)) nil "close-block-test failed"))

(defun py-shift-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
         ")))
    (py-bug-tests-intern 'py-shift-block-base arg teststring)))

(defun py-shift-block-base ()
  (let (py-smart-indentation)
    (goto-char 237)
    (assert (eq 12 (py-shift-block-right)) nil "py-shift-block-test #1 failed")
    (assert (eq 8 (py-shift-block-left)) nil "py-shift-block-test #1 failed")))

(defun nesting-if-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if foo:
    if bar:
        pass
    else:
        pass
else:
    pass

")))
    (py-bug-tests-intern 'nesting-if-test-base arg teststring)))

(defun nesting-if-test-base ()
  (goto-char 105)
  (assert (eq 0 (py-compute-indentation)) nil "nesting-if-test failed"))

(defun py-end-of-print-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

def usage():
    print \"\"\"Error: %s
somme errors
\"\"\" % (
          os.path.basename(sys.argv[0]))

def usage():
    print '''Error: %s
somme errors
''' % (
          os.path.basename(sys.argv[0]))

")))
    (py-bug-tests-intern 'py-end-of-print-statement-base arg teststring)))

(defun py-end-of-print-statement-base ()
  (goto-char 66)
  (sit-for 0.1)
  (assert (eq 146 (py-end-of-statement)) nil "py-end-of-print-statement-test #1 failed")

  (assert (eq 160 (py-end-of-statement)) nil "py-end-of-print-statement-test #2 failed")

  (assert (eq 245 (py-end-of-statement)) nil "py-end-of-print-statement-test #3 failed")

  )

(defun nested-try-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

def main(argv):
    grammar = \"foo.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        try:
            bla
        except getopt.GetoptError:
            asdf()
        finally:
            return \"blub\"
    finally:
        print \"asdf\"

")))
    (py-bug-tests-intern 'nested-try-base arg teststring)))

(defun nested-try-base ()
  (goto-char 306)
  (assert (eq 8 (py-compute-indentation)) nil "nested-try-test failed"))

(defun nested-if-test-1 (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if abr:
    if x > 0:
        if foo:
            print \"foo\"
        elif bar:
            print \"abasdf\"
        elif baz:
            for i in range(100):
                print i
            else:
                print \\\"All done\\\"
    elif x < 0:
        print \\\"x is negative\\\"
else:
    print \"asbd\"

")))
    (py-bug-tests-intern 'nested-if-base-1 arg teststring)))

(defun nested-if-base-1 ()
  (goto-char 299)
  (assert (eq 8 (py-compute-indentation)) nil "nested-if-test-1 failed"))

(defun nested-try-finally-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

# Example from:
# To: python-ideas@python.org
# From: Nikolaus Rath <Nikolaus@rath.org>
# Date: Tue, 18 Oct 2011 22:14:56 -0400
# Message-ID: <87pqhtafrz.fsf@vostro.rath.org>

def my_fun():
    allocate_res1()
    try:
        # do stuff
        allocate_res2()
        try:
            # do stuff
            allocate_res3()
            try:
                do stuff
            finally:
                cleanup_res3()
        finally:
            cleanup_res2()
    finally:
        cleanup_res1()

    return

")))
    (py-bug-tests-intern 'nested-try-finally-base arg teststring)))

(defun nested-try-finally-base ()
  (goto-char 431)
  (assert (eq 12 (py-compute-indentation)) nil "nested-try-finally-test failed"))

(defun tqs-list-error-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
class foo(bar, baz):
    \"\"\"
    foo is an ABC for matrix containers; i.e.,
    \\\"\\\"\\\"containers of a finite number of orig
\"\"\"
    pass
")))
    (py-bug-tests-intern 'tqs-list-error-base 2 teststring)))

(defun tqs-list-error-base ()
  (goto-char 90)
  (assert (eq 175   (py-end-of-statement)) nil "tqs-list-error-test failed"))

(defun py-smart-indent-eight-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((py-smart-indentation t)
        (teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
for x in y:
    for z in l:
        for r in t:
        pass
                # <--- indents here. Pressing <backspace> dedents eight spaces (i.e. you can go to column 0 in two presess)
")))
    (py-bug-tests-intern 'py-smart-indent-eight-base arg teststring)))

(defun py-smart-indent-eight-base ()
  (goto-char 104)
  (assert (eq 4 (py-guess-indent-offset)) nil "py-smart-indent-eight-test #1 failed")
  (assert (eq 12 (py-compute-indentation)) nil "py-smart-indent-eight-test #2 failed")
)

(defun py-install-directory-path-test (&optional arg)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
")))
    (py-bug-tests-intern 'py-install-directory-path-base arg teststring)))

(defun py-install-directory-path-base ()
  "See if `py-install-directory' is set when required. "
  (assert (py-install-directory-check) nil "`py-install-directory' not valid. See INSTALL. "))

;;;
(defun switch-windows-on-execute-p-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print(\"I'm the switch-windows-on-execute-p-test\")
")))
    (py-bug-tests-intern 'switch-windows-on-execute-p-base arg teststring)))

(defun switch-windows-on-execute-p-base ()
  (let ((py-switch-buffers-on-execute-p t)
        (erg (buffer-name)))
    (py-execute-buffer)
    (sit-for 0.1)
    (message "current-buffer: %s" (current-buffer) )
    (assert (string-match "*Python*" (buffer-name (current-buffer))) nil "switch-windows-on-execute-p-test failed")))

(defun split-windows-on-execute-p-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print(\"I'm the `split-windows-on-execute-p-test'\")
")))
    (py-bug-tests-intern 'split-windows-on-execute-p-base arg teststring)))

(defun split-windows-on-execute-p-base ()
  (delete-other-windows)
  (let ((py-split-windows-on-execute-p t)
        (py-split-windows-on-execute-function 'split-window-vertically)
        (py-switch-buffers-on-execute-p t)
        (erg (current-window-configuration)))
    (py-execute-buffer)
    (assert (not (window-full-height-p)) nil "split-windows-on-execute-p-test failed")))

;; this test is not valable, as python-mode-map often changes
(defun py-menu-pyshell-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print(\"I'm the `py-menu-pyshell-test'\")
")))
    (py-bug-tests-intern 'py-menu-pyshell-base arg teststring)))

(defun py-menu-pyshell-base ()
  (assert (string= "PyShell" (prin1-to-string
                              (car (nth 1 (cdr (nth 17 python-mode-map))))
                              ;; (car (nth 2 (nth 1 (cdr python-mode-map))))
                              )) nil "py-menu-pyshell-test failed"))

(defun python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-dedicated-base arg teststring)))

(defun python-dedicated-base ()
  (set-buffer (python-dedicated))
  (sit-for 0.1)
  (assert (string-match "^\*Python[0-9.]*-[:alnum:]+*" (buffer-name)) nil "python-dedicated-test failed"))

(defun py-separator-char-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-separator-char-base arg teststring)))

(defun py-separator-char-base ()
  (assert (stringp (py-separator-char)) nil "py-separator-char-test failed"))

(defun py-shell-complete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
impo")))
    (py-bug-tests-intern 'py-shell-complete-base 2 teststring)))

(defun py-shell-complete-base ()
  (py-shell-complete)
  (sit-for 0.1)
  (assert (looking-back "import") nil "py-shell-complete-test failed"))

(defun toggle-force-py-shell-name-p-test (&optional arg)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'toggle-force-py-shell-name-p-base arg teststring)))

(defun toggle-force-py-shell-name-p-base ()
  (let ((old py-force-py-shell-name-p))
    (assert (not (eq old (toggle-force-py-shell-name-p))) nil "toggle-force-py-shell-name-p-test failed")
    (setq py-force-py-shell-name-p old)))

(defun before-inline-comment-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
self._blah_blup = 0xe000 # aka: foo bar baz
self.nult = {}
self.nult['_foobar'] = []
"))
    (py-bug-tests-intern 'before-inline-comment-base arg teststring)))

(defun before-inline-comment-base ()
  (goto-char 72)
  (py-end-of-statement)
  (sit-for 0.1)
  (assert (eq 106 (point)) nil "before-inline-comment-test failed"))

(defun py-end-of-def-inline-comment-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

    #####################################
#####################################
def fooBaz( bar ):  # version 2003/9/7
  if \"Foo:\" == bar \\
          or  \"[Foo:]\" == bar \\
          or \"Foo:]\" == bar \\
          or \"Baz:\" == bar:
      return False
  return True
"))
    (py-bug-tests-intern 'py-end-of-def-inline-comment-base arg teststring)))

(defun py-end-of-def-inline-comment-base ()
  (let ((py-smart-indentation t))
    (goto-char 49)
    (py-end-of-def-or-class)
    (assert (eq 311 (point)) nil "py-end-of-def-inline-comment-test failed")))

(defun py-compute-indentation-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-
with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")

def foo()
"))
    (py-bug-tests-intern 'py-compute-indentation-base arg teststring)))

(defun py-compute-indentation-base ()
  (goto-char 99)
  (assert (eq 4 (py-compute-indentation)) nil "py-compute-indentation-test #1 failed")
  (goto-char 127)
  (assert (eq 8 (py-compute-indentation)) nil "py-compute-indentation-test #2 failed"))

(defun py-end-of-statement-test-1 (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/python
# -*- coding: utf-8 -*-
print dir()
c = Cat()
c.hello() #causes error, but emacs tracking fails
import sys, os; os.remove('do/something/nasty') # lp:1025000
"))
    (py-bug-tests-intern 'py-end-of-statement-1-base arg teststring)))

(defun py-end-of-statement-1-base ()
  (goto-char (point-min))
  (assert (eq 55 (py-end-of-statement)) nil "py-end-of-statement-test-1 #1 failed")
  (assert (eq 65 (py-end-of-statement)) nil "py-end-of-statement-test-1 #2 failed")
  (assert (eq 75 (py-end-of-statement)) nil "py-end-of-statement-test-1 #2 failed")
  (assert (eq 131 (py-end-of-statement)) nil "py-end-of-statement-test-1 #3 failed")
  (py-end-of-statement)
  (assert (eq 163 (point)) nil "py-end-of-statement-test-1 #4 failed"))

(defun py-end-of-statement-test-2 (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/python
# -*- coding: utf-8 -*-
pdb.set_trace()
# r'\\fB\$\{\\f([BI])([^}]+)\\f\1([^\\]+)\\fB}(.+)$')
result = w_behandlung(aus, i, line, klammer1=w.group(1), klammer2=w.group(2), klammer3=w.group(3), klammer4=w.group(4))
aus.write(result + \"\\n\")
"))
    (py-bug-tests-intern 'py-end-of-statement-2-base arg teststring)))

(defun py-end-of-statement-2-base ()
  (goto-char 59)
  (py-end-of-statement)
  (assert (eq 225 (point)) nil "py-end-of-statement-test-2 #1 failed"))

(defun py-beginning-of-statement-test-1 (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/python
# -*- coding: utf-8 -*-
print dir()
c = Cat()
c.hello() #causes error, but emacs tracking fails
import sys, os; os.remove('do/something/nasty') # lp:1025000
"))
    (py-bug-tests-intern 'py-beginning-of-statement-1-base arg teststring)))

(defun py-beginning-of-statement-1-base ()
  (py-beginning-of-statement)
  (assert (eq 132 (point)) nil "py-beginning-of-statement-test-1 #1 failed")
  (assert (eq 116 (py-beginning-of-statement)) nil "py-beginning-of-statement-test-1 #2 failed")
  (assert (eq 66 (py-beginning-of-statement)) nil "py-beginning-of-statement-test-1 #3 failed")
  (assert (eq 56 (py-beginning-of-statement)) nil "py-beginning-of-statement-test-1 #4 failed")
  (assert (eq 44 (py-beginning-of-statement)) nil "py-beginning-of-statement-test-1 #5 failed"))

(defun key-binding-tests (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
"))
    (py-bug-tests-intern 'key-binding-base arg teststring)))

(defun key-binding-base ()
  (assert (eq (key-binding [(:)]) 'py-electric-colon) nil "py-electric-colon key-binding test failed")

  (assert (eq (key-binding [(\#)]) 'py-electric-comment) nil "py-electric-comment key-binding test failed")
  (assert (eq (key-binding [(delete)]) 'py-electric-delete) nil "py-electric-delete key-binding test failed")
  (assert (eq (key-binding [(backspace)]) 'py-electric-backspace) nil "py-electric-backspace key-binding test failed")
  (assert (eq (key-binding [(control backspace)]) 'py-hungry-delete-backwards) nil "py-hungry-delete-backwards key-binding test failed")
  (assert (eq (key-binding [(control c) (delete)]) 'py-hungry-delete-forward) nil "py-hungry-delete-forward key-binding test failed")
  (assert (eq (key-binding [(control meta a)]) 'py-beginning-of-def-or-class) nil "py-beginning-of-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control meta e)]) 'py-end-of-def-or-class) nil "py-end-of-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control c)(control l)]) 'py-shift-left) nil "py-shift-left key-binding test failed")
  (assert (eq (key-binding [(control c)(control r)]) 'py-shift-right) nil "py-shift-right key-binding test failed")
  (assert (eq (key-binding [(control c)(<)]) 'py-shift-left) nil "py-shift-left key-binding test failed")
  (assert (eq (key-binding [(control c)(>)]) 'py-shift-right) nil "py-shift-right key-binding test failed")
  (assert (eq (key-binding [(control c)(tab)]) 'py-indent-region) nil "py-indent-region key-binding test failed")
  (assert (eq (key-binding [(control c)(:)]) 'py-guess-indent-offset) nil "py-guess-indent-offset key-binding test failed")

  (assert (eq (key-binding [(control c)(control c)]) 'py-execute-buffer) nil "py-execute-buffer key-binding test failed")
  (assert (eq (key-binding [(control c)(control m)]) 'py-execute-import-or-reload) nil "py-execute-import-or-reload key-binding test failed")
  (assert (eq (key-binding [(control c)(control s)]) 'py-execute-string) nil "py-execute-string key-binding test failed")
  (assert (eq (key-binding [(control c)(|)]) 'py-execute-region) nil "py-execute-region key-binding test failed")
  (assert (eq (key-binding [(control meta x)]) 'py-execute-def-or-class) nil "py-execute-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control c)(!)]) 'py-shell) nil "py-shell key-binding test failed")
  (assert (eq (key-binding [(control c)(control t)]) 'py-toggle-shell) nil "py-toggle-shell key-binding test failed")
  (assert (eq (key-binding [(control meta h)]) 'py-mark-def-or-class) nil "py-mark-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control c)(control k)]) 'py-mark-block-or-clause) nil "py-mark-block-or-clause key-binding test failed")

  (assert (eq (key-binding [(control c)(\.)]) 'py-expression) nil "py-expression key-binding test failed")

  (assert (eq (key-binding [(control c)(control d)]) 'py-pdbtrack-toggle-stack-tracking) nil "py-pdbtrack-toggle-stack-tracking key-binding test failed")
  (assert (eq (key-binding [(control c)(control f)]) 'py-sort-imports) nil "py-sort-imports key-binding test failed")
  (assert (eq (key-binding [(control c)(\#)]) 'py-comment-region) nil "py-comment-region key-binding test failed")
  (assert (eq (key-binding [(control c)(\?)]) 'py-describe-mode) nil "py-describe-mode key-binding test failed")

  (assert (eq (key-binding [(control c)(control e)]) 'py-help-at-point) nil "py-describe-symbol key-binding test failed")
  (assert (eq (key-binding [(control c)(-)]) 'py-up-exception) nil "py-up-exception key-binding test failed")
  (assert (eq (key-binding [(control c)(=)]) 'py-down-exception) nil "py-down-exception key-binding test failed")
  (assert (eq (key-binding [(control x) (n) (d)]) 'py-narrow-to-defun) nil "py-narrow-to-defun key-binding test failed")
  (assert (eq (key-binding [(control c)(control b)]) 'py-submit-bug-report) nil "py-submit-bug-report key-binding test failed")
  (assert (eq (key-binding [(control c)(control v)]) 'py-version) nil "py-version key-binding test failed")
  (assert (eq (key-binding [(control c)(control w)]) 'py-pychecker-run) nil "py-pychecker-run key-binding test failed")
  (assert (eq (key-binding (kbd "TAB")) 'py-indent-or-complete) nil "py-indent-or-complete key-binding test failed")
  (assert (eq (key-binding [(control c)(control p)]) 'py-beginning-of-statement) nil "py-beginning-of-statement key-binding test failed")
  (assert (eq (key-binding [(control c)(control n)]) 'py-end-of-statement) nil "py-end-of-statement key-binding test failed")
  (assert (eq (key-binding [(control j)]) 'py-newline-and-indent) nil "py-newline-and-indent key-binding test failed")
  (assert (eq (key-binding (kbd "RET")) 'py-newline-and-indent) nil "py-newline-and-indent key-binding test failed"))

(defun py-smart-operator-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
foo "))
    (py-bug-tests-intern 'py-smart-operator-base arg teststring)))

(defun py-smart-operator-base ()
  (python-mode)
  (let ((py-smart-operator-mode-p t))
    (py-smart-operator-mode-p-on)
    (goto-char 52)
    (save-excursion
      (smart-operator-<))
    (assert (looking-at " < ") nil "smart-operator-test \"smart-operator-<\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion
      (smart-operator->))
    (assert (looking-at " > ") nil "smart-operator-test \"smart-operator->\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-%))
    (assert (looking-at " % ") nil "smart-operator-test \"smart-operator-%\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-+))
    (assert (looking-at " \\+ ") nil "smart-operator-test \"smart-operator-+\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator--))
    (assert (looking-at " - ") nil "smart-operator-test \"smart-operator--\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-*))
    (assert (looking-at " * ") nil "smart-operator-test \"smart-operator-*\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-&))
    (assert (looking-at " & ") nil "smart-operator-test \"smart-operator-&\" failed")
    ;; (delete-region (point) (line-end-position))
    ;; (save-excursion (smart-operator-!))
    ;; (assert (looking-at "! ") nil "smart-operator-test \"smart-operator-!\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-?))
    (assert (looking-at "? ") nil "smart-operator-test \"smart-operator-?\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-\,))
    (assert (looking-at ", ") nil "smart-operator-test \"smart-operator-\,\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-.))
    (assert (looking-at ".") nil "smart-operator-test \"smart-operator-.\" failed")
    (when py-verbose-p (message "%s" "smart-operator-test passed"))))

;; broken
(defun augmented-assigment-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
foo"))
    (py-bug-tests-intern 'augmented-assigment-base arg teststring)))

(defun augmented-assigment-base ()
  (let ((py-smart-operator-mode-p t))
    (smart-operator-mode-on)
    (goto-char 52)
    (save-excursion
      (smart-operator-<)
      (insert "="))

    (assert (looking-at " <= ") nil "augmented-assigment-test \"smart-operator-<\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion
      (smart-operator->))
    (assert (looking-at " >= ") nil "augmented-assigment-test \"smart-operator->\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-%))
    (assert (looking-at " %= ") nil "augmented-assigment-test \"smart-operator-%\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-+))
    (assert (looking-at " \\+= ") nil "augmented-assigment-test \"smart-operator-+\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator--))
    (assert (looking-at " -= ") nil "augmented-assigment-test \"smart-operator--\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-*))
    (assert (looking-at " \\*= ") nil "augmented-assigment-test \"smart-operator-*\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-&))
    (assert (looking-at " &= ") nil "augmented-assigment-test \"smart-operator-&\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-!))
    (assert (looking-at " != ") nil "augmented-assigment-test \"smart-operator-!\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (smart-operator-?))
    (assert (looking-at " \\?= ") nil "augmented-assigment-test \"smart-operator-?\" failed")
    ;; (delete-region (point) (line-end-position))
    ;; (save-excursion (smart-operator-\,))
    ;; (assert (looking-at " ,= ") nil "augmented-assigment-test \"smart-operator-\,\" failed")
    ;; (delete-region (point) (line-end-position))
    ;; (save-excursion (smart-operator-.))
    ;; (assert (looking-at " .= ") nil "augmented-assigment-test \"smart-operator-.\" failed")
    ;; (assert nil "smart-operator-test failed")
    (when py-verbose-p (message "%s" "augmented-assigment-test passed"))))

(defun py-smart-operator-repeat-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
foo "))
    (py-bug-tests-intern 'py-smart-operator-repeat-base arg teststring)))

(defun py-smart-operator-repeat-base ()
  (let ((py-smart-operator-mode-p t))
    (py-smart-operator-mode-on)
    (goto-char 52)
    (setq last-command nil)
    (save-excursion
      (call-interactively 'smart-operator-> t)
      (setq last-command 'smart-operator->)
      (setq this-command 'smart-operator->)
      ;; (message "%s" this-command-keys-vector)
      (call-interactively 'smart-operator->))
    (assert (looking-at " >> ") nil "smart-operator-test \"smart-operator->\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion
      (call-interactively 'smart-operator-<)
      (setq last-command 'smart-operator-<)
      (setq this-command 'smart-operator-<)
      (call-interactively 'smart-operator-<))
    (assert (looking-at " << ") nil "smart-operator-test \"smart-operator-<\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-%)(setq this-command 'smart-operator-%)(setq last-command 'smart-operator-%)(smart-operator-%))
    (assert (looking-at " %% ") nil "smart-operator-test \"smart-operator-%\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-+)(setq this-command 'smart-operator-+)(setq last-command 'smart-operator-+)(smart-operator-+))
    (assert (looking-at " \\+\\+ ") nil "smart-operator-test \"smart-operator-+\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator--)(setq this-command 'smart-operator--)(setq last-command 'smart-operator--)(smart-operator--))
    (assert (looking-at " -- ") nil "smart-operator-test \"smart-operator--\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-*)(setq this-command 'smart-operator-*)(setq last-command 'smart-operator-*)(smart-operator-*))
    (assert (looking-at " ** ") nil "smart-operator-test \"smart-operator-*\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-&)(setq this-command 'smart-operator-&)(setq last-command 'smart-operator-&)(smart-operator-&))
    (assert (looking-at " && ") nil "smart-operator-test \"smart-operator-&\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-!)(setq this-command 'smart-operator-!)(setq last-command 'smart-operator-!)(smart-operator-!))
    (assert (looking-at "!! ") nil "smart-operator-test \"smart-operator-!\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-?)(setq this-command 'smart-operator-?)(setq last-command 'smart-operator-?)(smart-operator-?))
    (assert (looking-at "\\?\\? ") nil "smart-operator-test \"smart-operator-?\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-\,)(setq this-command 'smart-operator-\,)(setq last-command 'smart-operator-\,)(smart-operator-\,))
    (assert (looking-at ",, ") nil "smart-operator-test \"smart-operator-\,\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (smart-operator-.)(setq this-command 'smart-operator-.)(setq last-command 'smart-operator-.)(smart-operator-.))
    (assert (looking-at "..") nil "smart-operator-test \"smart-operator-.\" failed")
    (when py-verbose-p (message "%s" "py-smart-operator-test passed"))))

(defun py-switch-imenu-index-function-test (&optional arg)
  (interactive "p")
  (let ((teststring python-mode-teststring))
  (py-bug-tests-intern 'py-switch-imenu-index-function-base arg teststring)))

(defun py-switch-imenu-index-function-base ()
  (assert (listp imenu--index-alist) nil "py-switch-imenu-index-function-test failed")
  (assert (py-switch-imenu-index-function) nil "py-switch-imenu-index-function-test failed")
  (assert (listp imenu--index-alist) nil "py-switch-imenu-index-function-test failed"))

(defun py-bol-moves-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-bol-moves-base arg teststring)))

(defun py-bol-moves-base ()
  (message "comment-start: %s" comment-start)
  (goto-char 592)
  ;; (sit-for 0.1)
  (assert (eq 561 (py-up-clause-bol)) nil "py-up-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-clause-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-up-block-or-clause-bol)) nil "py-up-block-or-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-block-or-clause-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 234 (py-up-def-bol)) nil "py-up-def-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-def-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 1 (py-up-class-bol)) nil "py-up-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-class-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 234 (py-up-def-or-class-bol)) nil "py-up-def-or-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-def-or-class-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-up-block-bol)) nil "py-up-block-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-block-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-up-minor-block-bol)) nil "py-up-minor-block-bol-test of `py-moves-test' failed")
  (message "%s" "py-up-minor-block-bol-test of `py-moves-test'  done")
  (goto-char 592)
  ;; (sit-for 0.1)
  (assert (eq 325 (py-up-block)) nil "py-up-block-test of `py-moves-test' failed")
  (message "%s" "py-up-block-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 325 (py-up-minor-block)) nil "py-up-minor-block-test of `py-moves-test' failed")
  (message "%s" "py-up-minor-block-test of `py-moves-test'  done")
  (goto-char 592)
  ;; (sit-for 0.1)
  (assert (eq 569 (py-up-clause)) nil "py-up-clause-test of `py-moves-test' failed")
  (message "%s" "py-up-clause-test of `py-moves-test'  done")
  (goto-char 592)
  ;; (sit-for 0.1)
  (assert (eq 569 (py-up-block-or-clause)) nil "py-up-block-or-clause-test of `py-moves-test' failed")
  (message "%s" "py-up-block-or-clause-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 238 (py-up-def)) nil "py-up-def-test of `py-moves-test' failed")
  (message "%s" "py-up-def-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 1 (py-up-class)) nil "py-up-class-test of `py-moves-test' failed")
  (message "%s" "py-up-class-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 238 (py-up-def-or-class)) nil "py-up-def-or-class-test of `py-moves-test' failed")
  (message "%s" "py-up-def-or-class-test of `py-moves-test'  done")
  (goto-char 264)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-down-block-bol)) nil "py-down-block-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-block-bol-test of `py-moves-test'  done")
  (goto-char 561)
  ;; (sit-for 0.1)
  (assert (eq 594 (py-down-clause-bol)) nil "py-down-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-clause-bol-test of `py-moves-test'  done")
  (goto-char 264)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-down-block-or-clause-bol)) nil "py-down-block-or-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-block-or-clause-bol-test of `py-moves-test'  done")
  (goto-char (point-min))
  (assert (eq 142 (py-down-def-bol)) nil "py-down-def-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-def-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (not (py-down-class-bol)) nil "py-down-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-class-bol-test of `py-moves-test'  done")
  (goto-char (point-min))
  (assert (eq 142 (py-down-def-or-class-bol)) nil "py-down-def-or-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-def-or-class-bol-test of `py-moves-test'  done")
  (goto-char 264)
  ;; (sit-for 0.1)
  (assert (eq 325 (py-down-block)) nil "py-down-block-test of `py-moves-test' failed")
  (message "%s" "py-down-block-test of `py-moves-test'  done")
  (goto-char 264)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-down-block-bol)) nil "py-down-block-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-block-bol-test of `py-moves-test'  done")

  (goto-char 264)
  ;; (sit-for 0.1)
  (assert (eq 325 (py-down-minor-block)) nil "py-down-minor-block-test of `py-moves-test' failed")
  (message "%s" "py-down-minor-block-test of `py-moves-test'  done")
  (goto-char 264)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-down-minor-block-bol)) nil "py-down-minor-block-bol-test of `py-moves-test' failed")
  (message "%s" "py-down-minor-block-bol-test of `py-moves-test'  done")

  (goto-char 569)
  ;; (sit-for 0.1)
  (assert (eq 602 (py-down-clause)) nil "py-down-clause-test of `py-moves-test' failed")
  (message "%s" "py-down-clause-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 569 (py-down-block-or-clause)) nil "py-down-block-or-clause-test of `py-moves-test' failed")
  (message "%s" "py-down-block-or-clause-test of `py-moves-test'  done")
  (goto-char (point-min))
  (assert (eq 146 (py-down-def)) nil "py-down-def-test of `py-moves-test' failed")
  (message "%s" "py-down-def-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (not (py-down-class)) nil "py-down-class-test of `py-moves-test' failed")
  (message "%s" "py-down-class-test of `py-moves-test'  done")
  (goto-char (point-min))
  (assert (eq 146 (py-down-def-or-class)) nil "py-down-def-or-class-test of `py-moves-test' failed")
  (message "%s" "py-down-def-or-class-test of `py-moves-test'  done")

  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 332 (py-beginning-of-statement-bol)) nil "py-beginning-of-statement-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-statement-bol-test of `py-moves-test'  done")
  (goto-char 410)
  (sit-for 0.1)
  (assert (eq 317 (py-beginning-of-block-bol)) nil "py-beginning-of-block-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-block-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-beginning-of-clause-bol)) nil "py-beginning-of-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-clause-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 317 (py-beginning-of-block-or-clause-bol)) nil "py-beginning-of-block-or-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-block-or-clause-bol-test of `py-moves-test'  done")
  (assert (eq 1 (py-beginning-of-class-bol)) nil "py-beginning-of-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-class-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 234 (py-beginning-of-def-or-class-bol)) nil "py-beginning-of-def-or-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-def-or-class-bol-test of `py-moves-test'  done")
  (message "%s" "py-end-of-block-bol-test of `py-moves-test'  done")
  (goto-char 576)
  ;; (sit-for 0.1)
  (assert (eq 594 (py-end-of-clause-bol)) nil "py-end-of-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-end-of-clause-bol-test of `py-moves-test'  done")
  (goto-char 576)
  ;; (sit-for 0.1)
  (assert (eq 594 (py-end-of-block-or-clause-bol)) nil "py-end-of-block-or-clause-bol-test of `py-moves-test' failed")
  (message "%s" "py-end-of-block-or-clause-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 627 (py-end-of-def-bol)) nil "py-end-of-def-bol-test of `py-moves-test' failed")
  (message "%s" "py-end-of-def-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 627 (py-end-of-class-bol)) nil "py-end-of-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-end-of-class-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 627 (py-end-of-def-or-class-bol)) nil "py-end-of-def-or-class-bol-test of `py-moves-test' failed")
  (message "%s" "py-end-of-def-or-class-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 561 (py-end-of-statement-bol)) nil "py-end-of-statement-bol-test of `py-moves-test' failed")
  (message "%s" "py-end-of-statement-bol-test of `py-moves-test'  done")
  (goto-char 410)
  ;; (sit-for 0.1)
  (assert (eq 234 (py-beginning-of-def-bol)) nil "py-beginning-of-def-bol-test of `py-moves-test' failed")
  (message "%s" "py-beginning-of-def-bol-test of `py-moves-test'  done")
  )

(defun py-guess-indent-offset-test (&optional arg)
  (interactive "p")
  (let (py-smart-indentation
        (teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

    #####################################
#####################################
def fooBaz( bar ):  # version 2003/9/7
  if \"Foo:\" == bar \\
          or  \"[Foo:]\" == bar \\
          or \"Foo:]\" == bar \\
          or \"Baz:\" == bar:
      return False
  return True
"))
    (py-bug-tests-intern 'py-guess-indent-offset-base arg teststring)))

(defun py-guess-indent-offset-base ()
  (goto-char 49)
  (assert (eq 4 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #1 failed")
  (message "%s" "py-guess-indent-offset-test #1 done")
  (goto-char 168)
  (assert (eq 2 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #2 failed")
  (message "%s" "py-guess-indent-offset-test #2 done")
  (goto-char 251)
  (assert (eq 4 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #3 failed")
  (message "%s" "py-guess-indent-offset-test #3 done")
  (goto-char 280)
  (assert (eq 4 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #4 failed")
  (message "%s" "py-guess-indent-offset-test #4 done")
  (goto-char 298)
  ;; indent might be eithe 4 or 2
  (assert (eq 2 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #5 failed")
  (message "%s" "py-guess-indent-offset-test #5 done"))

(defun autopair-mode-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
  (py-bug-tests-intern 'autopair-on-base arg teststring)))

(defun autopair-on-base ()
  (assert (py-autopair-mode-on) nil "autopair-mode-test #1 failed")
  (message "%s" "autopair-mode-test #1  done")
  (assert (not (py-toggle-autopair-mode)) nil "autopair-mode-test #2 failed"))
  (message "%s" "autopair-mode-test #2  done")

(defun py-smart-indentation-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
  (py-bug-tests-intern 'py-smart-indentation-base arg teststring)))

(defun py-smart-indentation-base ()
  (assert (py-smart-indentation-on) nil "smart-indentation-test #1 failed")
  (message "%s" "smart-indentation-test #1  done")
  (assert (not (py-smart-indentation-off)) nil "smart-indentation-test #2 failed")
  (message "%s" "smart-indentation-test #2  done")
  (assert (py-toggle-smart-indentation) nil "smart-indentation-test #3 failed"))
  (message "%s" "smart-indentation-test #3  done")

(defun py-highlight-indentation-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
  (py-bug-tests-intern 'py-highlight-indentation-base arg teststring)))

(defun py-highlight-indentation-base ()
  (py-highlight-indentation-on)
  (assert highlight-indent-active nil "highlight-indentation-test #1 failed")
  (message "%s" "highlight-indentation-test #1  done")
  (py-highlight-indentation-off)
  (assert (not highlight-indent-active) nil "highlight-indentation-test #2 failed")
  (message "%s" "highlight-indentation-test #2  done")
  (py-toggle-highlight-indentation)
  (assert highlight-indent-active nil "highlight-indentation-test #3 failed"))
  (message "%s" "highlight-indentation-test #3  done")

(defun py-fill-string-django-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo()
    ''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''
"))
    (py-bug-tests-intern 'py-fill-string-django-base arg teststring)))

(defun py-fill-string-django-base ()
  (sit-for 0.1)
  (goto-char 99)
  (py-fill-string-django)
  (beginning-of-line)
  (sit-for 0.1)
  (assert (nth 8 (syntax-ppss)) t nil "py-fill-string-django-test #1 failed")
  (message "%s" "py-fill-string-django-test #1  done")
  (goto-char (nth 8 (syntax-ppss)))
  (sit-for 1)
  (assert (looking-at (concat py-string-delim-re "$")) t nil "py-fill-string-django-test #2 failed")
  (message "%s" "$")
)

(defun py-fill-string-onetwo-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo()
    ''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''
"))
  (py-bug-tests-intern 'py-fill-string-onetwo-base arg teststring)))

(defun py-fill-string-onetwo-base ()
  (sit-for 0.1)
  (goto-char 99)
  (py-fill-string-onetwo)
  (forward-line 2)
  (assert (empty-line-p) nil "py-fill-string-onetwo-test #1 failed")
  (message "%s" "py-fill-string-onetwo-test #1  done")
  (goto-char (nth 8 (syntax-ppss)))
  (assert (looking-at (concat py-string-delim-re "$")) nil "py-fill-string-onetwo-test #2 failed"))
  (message "%s" "$")

(defun py-fill-string-pep-257-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo()
    ''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''
"))
  (py-bug-tests-intern 'py-fill-string-pep-257-base arg teststring)))

(defun py-fill-string-pep-257-base ()
  (sit-for 0.1)
  (goto-char 99)
  (py-fill-string-pep-257)
  (forward-line 1)
  (assert (nth 3 (syntax-ppss))  nil "py-fill-string-pep-257-test #1 failed")
  (message "%s" "py-fill-string-pep-257-test #1  done")
  (assert (empty-line-p)  nil "py-fill-string-pep-257-test #2 failed"))

(defun py-fill-string-pep-257-nn-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo()
    ''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''
"))
  (py-bug-tests-intern  'py-fill-string-pep-257-nn-base arg teststring)))

(defun py-fill-string-pep-257-nn-base ()
  (sit-for 0.1)
  (goto-char 99)
  (py-fill-string-pep-257-nn)
  (assert (nth 3 (syntax-ppss))  nil "py-fill-string-pep-257-nn-test #1 failed")
  (message "%s" "py-fill-string-pep-257-nn-test #1  done")
  (re-search-forward "py-string-delim-re" nil t 1)
  (assert (not (empty-line-p))  nil "py-fill-string-pep-257-non-nil-test #2 failed"))

(defun py-fill-string-symmetric-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo():
    ''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf

asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
    pass
"))
  (py-bug-tests-intern 'py-fill-string-symmetric-base arg teststring)))

(defun py-fill-string-symmetric-base ()
  (goto-char 84)
  (py-fill-string-symmetric)
  (sit-for 0.1)
  (forward-line -4)
  (assert (empty-line-p) nil "py-fill-string-symmetric-test failed")
  (message "%s" "py-fill-string-symmetric-test done")
  (re-search-forward py-string-delim-re nil t 3)
  (goto-char (match-beginning 0))
  (assert (looking-at (concat py-string-delim-re "$")) nil "py-fill-string-symmetric-test failed"))

(defun py-electric-yank-test (&optional arg)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-electric-yank-base arg teststring)))

(defun py-electric-yank-base ()
  (let ((py-electric-yank-active-p t)
        (kill-new "asdf"))
    (goto-char 610)
    (py-electric-delete)
    (assert (eq 8 (current-indentation))  nil "py-electric-yank-test #1 failed, `py-electric-delete' ")
  (message "%s" "py-electric-yank-test #1 failed, `py-electric-delete' ")
    (end-of-line)
    (py-electric-yank)
    (assert (eq 12 (current-indentation))  nil "py-electric-yank-test #2 failed")))


(defun py-down-statement-test (&optional arg)
  (interactive "p")
  (let ((teststring python-mode-teststring))
  (py-bug-tests-intern 'py-down-statement-base arg teststring)))

(defun py-down-statement-base ()
    (goto-char (point-min))
    (py-down-statement)
    (assert (eq 31 (point)) nil "py-down-statement-test failed"))

(defun py-nested-block-or-clause-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
if foo:
    if bar:
        if True:
            pass
        elif False:
            pass
        else:
            pass
    elif baz:
        pass
elif foo2:
    if bar2:
        pass
    elif baz2:
        pass
    else:
        pass
else:
    pass

"))
  (py-bug-tests-intern 'py-nested-block-or-clause-base arg teststring)))

(defun py-nested-block-or-clause-base ()
  (goto-char 48)
  (assert (eq 299 (py-end-of-block)) nil "py-nested-block-or-clause-test #1 failed")
  (message "%s" "py-nested-block-or-clause-test #1  done")
  (goto-char 60)
  (assert (eq 196 (py-end-of-block)) nil "py-nested-block-or-clause-test #2 failed")
  (message "%s" "py-nested-block-or-clause-test #2  done")
  (goto-char 76)
  (assert (eq 169 (py-end-of-block)) nil "py-nested-block-or-clause-test #3 failed")
  (message "%s" "py-nested-block-or-clause-test #3  done")
  (goto-char 48)
  (assert (eq 196 (py-end-of-clause)) nil "py-nested-block-or-clause-test #4 failed")
  (message "%s" "py-nested-block-or-clause-test #4  done")
  (goto-char 60)
  (assert (eq 169 (py-end-of-clause)) nil "py-nested-block-or-clause-test #5 failed")
  (message "%s" "py-nested-block-or-clause-test #5  done")
  (goto-char 85)
  (assert (eq 101 (py-end-of-clause)) nil "py-nested-block-or-clause-test #6 failed")
  (message "%s" "py-nested-block-or-clause-test #6  done")
  (goto-char 291)
  (assert (eq 285 (py-beginning-of-clause)) nil "py-nested-block-or-clause-test #7 failed")
  (message "%s" "py-nested-block-or-clause-test #7  done")
  (sit-for 0.1)
  (assert (eq 197 (py-beginning-of-clause)) nil "py-nested-block-or-clause-test #8 failed")
  (message "%s" "py-nested-block-or-clause-test #8  done")
  (assert (eq 48 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #9 failed")
  (message "%s" "py-nested-block-or-clause-test #9  done")
  (goto-char 284)
  (assert (eq 266 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #10 failed")
  (message "%s" "py-nested-block-or-clause-test #10  done")
  (assert (eq 238 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #11 failed")
  (message "%s" "py-nested-block-or-clause-test #11  done")
  (assert (eq 212 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #12 failed")
  (message "%s" "py-nested-block-or-clause-test #12  done")
  (assert (eq 197 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #13 failed")
  (message "%s" "py-nested-block-or-clause-test #13  done")
  (goto-char 196)
  (assert (eq 174 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #14 failed")
  (message "%s" "py-nested-block-or-clause-test #14  done")
  (goto-char 169)
  (assert (eq 147 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #15 failed")
  (message "%s" "py-nested-block-or-clause-test #15  done")
  (assert (eq 110 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #16 failed")
  (message "%s" "py-nested-block-or-clause-test #16  done")
  (assert (eq 76 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #17 failed")
  (message "%s" "py-nested-block-or-clause-test #17  done")
  (assert (eq 60 (py-beginning-of-block-or-clause)) nil "py-nested-block-or-clause-test #18 failed")
  (message "%s" "py-nested-block-or-clause-test #18  done")
  )

(setq py--travel-current-indent-test-start 12)

(defun py--travel-current-indent-test (&optional indent orig)
  (interactive)
  (let ((orig (point))
        (indent (or indent
                    py--travel-current-indent-test-start
                    (string-to-number (read-from-minibuffer "Indent to travel:")))))
    (py--travel-current-indent indent orig)))

(defun docstring-style-switches-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
  (py-bug-tests-intern 'docstring-style-switches-base arg teststring)))

(defun docstring-style-switches-base ()

  (py-set-django-docstring-style)
  (when
      (assert (eq 'django py-docstring-style) nil "django not py-docstring-style")
    (message "%s" "django not py-docstring-passed"))
  (py-set-onetwo-docstring-style)
  (when
      (assert (eq 'onetwo py-docstring-style) nil "onetwo not py-docstring-style")
    (message "%s" "onetwo not py-docstring-passed"))
  (py-set-pep-257-docstring-style)
  (when
      (assert (eq 'pep-257 py-docstring-style) nil "pep-257 not py-docstring-style")
    (message "%s" "pep-257 not py-docstring-passed"))
  (py-set-pep-257-nn-docstring-style)
  (when
      (assert (eq 'pep-257-nn py-docstring-style) nil "pep-257-nn not py-docstring-style")
    (message "%s" "pep-257-nn not py-docstring-passed"))
  (py-set-symmetric-docstring-style)
  (when
      (assert (eq 'symmetric py-docstring-style) nil "symmetric not py-docstring-style")
    (message "%s" "symmetric not py-docstring-passed")))

(defun forward-sexp-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def usage():
    print(\"\"\"Fehler: %s
Es muß die aufzurufende Ziehungszahl als Argument angegeben werden:
'python roulette.py 1, 'python roulette.py 2', ... 'python roulette.py n'.
\"\"\" % (
          os.path.basename(sys.argv[0])))
"))
  (py-bug-tests-intern 'forward-sexp-base arg teststring)))

(defun forward-sexp-base ()
  ;; (message "forward-sexp-function: %s" forward-sexp-function)
  (goto-char 71)
  (sit-for 0.1)
  (forward-sexp 1)
  (sit-for 0.1)
  (when
      (assert (eq 231 (point)) nil "forward-sexp-test failed"))
  (message "%s" "forward-sexp-test passed"))

(defun nested-if-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
if foo:
    bar = \"p\"
elif bar:
    if baz == True:
        print(\" \")
else:
    bar = baz
"))
  (py-bug-tests-intern 'nested-if-base arg teststring)))

(defun nested-if-base ()
  (goto-char 118)
  (py-beginning-of-block)
  (when
      (assert (eq (char-after) ?i) nil "nested-if-test #1 failed")
    (message "%s" "nested-if-test #1 passed"))
  (py-beginning-of-block)
  (when
      (assert (eq (char-after) ?i) nil "nested-if-test #2 failed")
    (message "%s" "nested-if-test #2 passed"))
  (when
      (assert (not (py-beginning-of-block)) nil "nested-if-test #3 failed"))
  (message "%s" "nested-if-test #3 passed"))

(defun py-execute-region-error-test (&optional arg)
  (interactive "p")
  (let ((teststring "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
            datei.write(str(spiel[i]) + \"\\n\")
    print(F)
"))
    (py-bug-tests-intern 'py-execute-region-base arg teststring)))

(defun py-execute-region-base ()
  (goto-char 152)
  (push-mark)
  (end-of-line)
  (py-execute-region (line-beginning-position) (line-end-position))
  (set-buffer "*Python*")
  (goto-char (point-max))
  (assert (and (re-search-backward py-shell-prompt-regexp nil t 2)
               (search-forward "line 5")) nil "py-execute-region-test failed"))

(defun py-execute-statement-error-test (&optional arg)
  (interactive "p")
  (let ((teststring "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
            datei.write(str(spiel[i]) + \"\\n\")
    print(F)
"))
    (py-bug-tests-intern 'py-execute-statement-error-base arg teststring)))

(defun py-execute-statement-error-base ()
  (goto-char 152)
  (push-mark)
  (end-of-line)
  (py-execute-statement)
  (set-buffer "*Python*")
  (goto-char (point-max))
  (when (assert (and (re-search-backward py-shell-prompt-regexp nil t 2)
		     (search-forward "line 5")) nil "py-execute-statement-error-test failed")
    (message "%s" "py-execute-statement-error-test passed")))

(defun beginning-of-block-fails-from-wrong-indent-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
 for i in range(anzahl):
        klauf.pylauf()
            datei.write(str(spiel[i]) + \"\\n\")
"))
  (py-bug-tests-intern 'beginning-of-block-fails-from-wrong-indent-base arg teststring)))

(defun beginning-of-block-fails-from-wrong-indent-base ()
    (goto-char 102)
    (assert (eq 48 (py-beginning-of-block)) nil "beginning-of-block-fails-from-wrong-indent-test failed"))

(provide 'python-mode-test)
