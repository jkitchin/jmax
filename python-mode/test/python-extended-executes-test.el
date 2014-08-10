;;; python-extended-executes-test.el --- extended-executes test
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



(defun py-execute-statement-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python-base arg teststring)))

(defun py-execute-statement-python-base ()
  (assert (progn (py-execute-statement-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-statement-python-test")) nil "py-execute-statement-python-test failed"))

(defun py-execute-statement-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python-switch-base arg teststring)))

(defun py-execute-statement-python-switch-base ()
  (assert (progn (py-execute-statement-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-statement-python-test")) nil "py-execute-statement-python-switch-test failed"))

(defun py-execute-statement-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python-no-switch-base arg teststring)))

(defun py-execute-statement-python-no-switch-base ()
  (assert (progn (py-execute-statement-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-statement-python-test")) nil "py-execute-statement-python-no-switch-test failed"))

(defun py-execute-statement-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python-dedicated-base arg teststring)))

(defun py-execute-statement-python-dedicated-base ()
  (assert (progn (py-execute-statement-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-statement-python-test")) nil "py-execute-statement-python-dedicated-test failed"))

(defun py-execute-statement-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python-dedicated-switch-base ()
  (assert (progn (py-execute-statement-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-statement-python-test")) nil "py-execute-statement-python-dedicated-switch-test failed"))

(defun py-execute-statement-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-statement-ipython-base arg teststring)))

(defun py-execute-statement-ipython-base ()
  (assert (progn (py-execute-statement-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-statement-ipython-test")) nil "py-execute-statement-ipython-test failed"))

(defun py-execute-statement-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-ipython-switch-base arg teststring)))

(defun py-execute-statement-ipython-switch-base ()
  (assert (progn (py-execute-statement-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-statement-ipython-test")) nil "py-execute-statement-ipython-switch-test failed"))

(defun py-execute-statement-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-ipython-no-switch-base arg teststring)))

(defun py-execute-statement-ipython-no-switch-base ()
  (assert (progn (py-execute-statement-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-statement-ipython-test")) nil "py-execute-statement-ipython-no-switch-test failed"))

(defun py-execute-statement-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-ipython-dedicated-base arg teststring)))

(defun py-execute-statement-ipython-dedicated-base ()
  (assert (progn (py-execute-statement-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-statement-ipython-test")) nil "py-execute-statement-ipython-dedicated-test failed"))

(defun py-execute-statement-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-statement-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-statement-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-statement-ipython-test")) nil "py-execute-statement-ipython-dedicated-switch-test failed"))

(defun py-execute-statement-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3-base arg teststring)))

(defun py-execute-statement-python3-base ()
  (assert (progn (py-execute-statement-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3-test")) nil "py-execute-statement-python3-test failed"))

(defun py-execute-statement-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3-switch-base arg teststring)))

(defun py-execute-statement-python3-switch-base ()
  (assert (progn (py-execute-statement-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3-test")) nil "py-execute-statement-python3-switch-test failed"))

(defun py-execute-statement-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3-no-switch-base arg teststring)))

(defun py-execute-statement-python3-no-switch-base ()
  (assert (progn (py-execute-statement-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3-test")) nil "py-execute-statement-python3-no-switch-test failed"))

(defun py-execute-statement-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3-dedicated-base arg teststring)))

(defun py-execute-statement-python3-dedicated-base ()
  (assert (progn (py-execute-statement-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3-test")) nil "py-execute-statement-python3-dedicated-test failed"))

(defun py-execute-statement-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python3-dedicated-switch-base ()
  (assert (progn (py-execute-statement-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3-test")) nil "py-execute-statement-python3-dedicated-switch-test failed"))

(defun py-execute-statement-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2-base arg teststring)))

(defun py-execute-statement-python2-base ()
  (assert (progn (py-execute-statement-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-statement-python2-test")) nil "py-execute-statement-python2-test failed"))

(defun py-execute-statement-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2-switch-base arg teststring)))

(defun py-execute-statement-python2-switch-base ()
  (assert (progn (py-execute-statement-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-statement-python2-test")) nil "py-execute-statement-python2-switch-test failed"))

(defun py-execute-statement-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2-no-switch-base arg teststring)))

(defun py-execute-statement-python2-no-switch-base ()
  (assert (progn (py-execute-statement-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-statement-python2-test")) nil "py-execute-statement-python2-no-switch-test failed"))

(defun py-execute-statement-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2-dedicated-base arg teststring)))

(defun py-execute-statement-python2-dedicated-base ()
  (assert (progn (py-execute-statement-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-statement-python2-test")) nil "py-execute-statement-python2-dedicated-test failed"))

(defun py-execute-statement-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python2-dedicated-switch-base ()
  (assert (progn (py-execute-statement-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-statement-python2-test")) nil "py-execute-statement-python2-dedicated-switch-test failed"))

(defun py-execute-statement-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2.7-base arg teststring)))

(defun py-execute-statement-python2.7-base ()
  (assert (progn (py-execute-statement-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-statement-python2.7-test")) nil "py-execute-statement-python2.7-test failed"))

(defun py-execute-statement-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2.7-switch-base arg teststring)))

(defun py-execute-statement-python2.7-switch-base ()
  (assert (progn (py-execute-statement-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-statement-python2.7-test")) nil "py-execute-statement-python2.7-switch-test failed"))

(defun py-execute-statement-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2.7-no-switch-base arg teststring)))

(defun py-execute-statement-python2.7-no-switch-base ()
  (assert (progn (py-execute-statement-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-statement-python2.7-test")) nil "py-execute-statement-python2.7-no-switch-test failed"))

(defun py-execute-statement-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2.7-dedicated-base arg teststring)))

(defun py-execute-statement-python2.7-dedicated-base ()
  (assert (progn (py-execute-statement-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-statement-python2.7-test")) nil "py-execute-statement-python2.7-dedicated-test failed"))

(defun py-execute-statement-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-statement-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-statement-python2.7-test")) nil "py-execute-statement-python2.7-dedicated-switch-test failed"))

(defun py-execute-statement-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-test\")"))
  (py-bug-tests-intern 'py-execute-statement-jython-base arg teststring)))

(defun py-execute-statement-jython-base ()
  (assert (progn (py-execute-statement-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-statement-jython-test")) nil "py-execute-statement-jython-test failed"))

(defun py-execute-statement-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-jython-switch-base arg teststring)))

(defun py-execute-statement-jython-switch-base ()
  (assert (progn (py-execute-statement-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-statement-jython-test")) nil "py-execute-statement-jython-switch-test failed"))

(defun py-execute-statement-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-jython-no-switch-base arg teststring)))

(defun py-execute-statement-jython-no-switch-base ()
  (assert (progn (py-execute-statement-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-statement-jython-test")) nil "py-execute-statement-jython-no-switch-test failed"))

(defun py-execute-statement-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-jython-dedicated-base arg teststring)))

(defun py-execute-statement-jython-dedicated-base ()
  (assert (progn (py-execute-statement-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-statement-jython-test")) nil "py-execute-statement-jython-dedicated-test failed"))

(defun py-execute-statement-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-jython-dedicated-switch-base arg teststring)))

(defun py-execute-statement-jython-dedicated-switch-base ()
  (assert (progn (py-execute-statement-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-statement-jython-test")) nil "py-execute-statement-jython-dedicated-switch-test failed"))

(defun py-execute-statement-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.2-base arg teststring)))

(defun py-execute-statement-python3.2-base ()
  (assert (progn (py-execute-statement-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.2-test")) nil "py-execute-statement-python3.2-test failed"))

(defun py-execute-statement-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.2-switch-base arg teststring)))

(defun py-execute-statement-python3.2-switch-base ()
  (assert (progn (py-execute-statement-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.2-test")) nil "py-execute-statement-python3.2-switch-test failed"))

(defun py-execute-statement-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.2-no-switch-base arg teststring)))

(defun py-execute-statement-python3.2-no-switch-base ()
  (assert (progn (py-execute-statement-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.2-test")) nil "py-execute-statement-python3.2-no-switch-test failed"))

(defun py-execute-statement-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.2-dedicated-base arg teststring)))

(defun py-execute-statement-python3.2-dedicated-base ()
  (assert (progn (py-execute-statement-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.2-test")) nil "py-execute-statement-python3.2-dedicated-test failed"))

(defun py-execute-statement-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-statement-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.2-test")) nil "py-execute-statement-python3.2-dedicated-switch-test failed"))

(defun py-execute-statement-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.3-base arg teststring)))

(defun py-execute-statement-python3.3-base ()
  (assert (progn (py-execute-statement-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.3-test")) nil "py-execute-statement-python3.3-test failed"))

(defun py-execute-statement-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.3-switch-base arg teststring)))

(defun py-execute-statement-python3.3-switch-base ()
  (assert (progn (py-execute-statement-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.3-test")) nil "py-execute-statement-python3.3-switch-test failed"))

(defun py-execute-statement-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.3-no-switch-base arg teststring)))

(defun py-execute-statement-python3.3-no-switch-base ()
  (assert (progn (py-execute-statement-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.3-test")) nil "py-execute-statement-python3.3-no-switch-test failed"))

(defun py-execute-statement-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.3-dedicated-base arg teststring)))

(defun py-execute-statement-python3.3-dedicated-base ()
  (assert (progn (py-execute-statement-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.3-test")) nil "py-execute-statement-python3.3-dedicated-test failed"))

(defun py-execute-statement-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-statement-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-statement-python3.3-test")) nil "py-execute-statement-python3.3-dedicated-switch-test failed"))

(defun py-execute-statement-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-statement-bpython-base arg teststring)))

(defun py-execute-statement-bpython-base ()
  (assert (progn (py-execute-statement-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-statement-bpython-test")) nil "py-execute-statement-bpython-test failed"))

(defun py-execute-statement-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-bpython-switch-base arg teststring)))

(defun py-execute-statement-bpython-switch-base ()
  (assert (progn (py-execute-statement-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-statement-bpython-test")) nil "py-execute-statement-bpython-switch-test failed"))

(defun py-execute-statement-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-bpython-no-switch-base arg teststring)))

(defun py-execute-statement-bpython-no-switch-base ()
  (assert (progn (py-execute-statement-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-statement-bpython-test")) nil "py-execute-statement-bpython-no-switch-test failed"))

(defun py-execute-statement-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-statement-bpython-dedicated-base arg teststring)))

(defun py-execute-statement-bpython-dedicated-base ()
  (assert (progn (py-execute-statement-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-statement-bpython-test")) nil "py-execute-statement-bpython-dedicated-test failed"))

(defun py-execute-statement-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-statement-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-statement-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-statement-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-statement-bpython-test")) nil "py-execute-statement-bpython-dedicated-switch-test failed"))

(defun py-execute-block-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-test\")"))
  (py-bug-tests-intern 'py-execute-block-python-base arg teststring)))

(defun py-execute-block-python-base ()
  (assert (progn (py-execute-block-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-python-test")) nil "py-execute-block-python-test failed"))

(defun py-execute-block-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python-switch-base arg teststring)))

(defun py-execute-block-python-switch-base ()
  (assert (progn (py-execute-block-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-python-test")) nil "py-execute-block-python-switch-test failed"))

(defun py-execute-block-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python-no-switch-base arg teststring)))

(defun py-execute-block-python-no-switch-base ()
  (assert (progn (py-execute-block-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-python-test")) nil "py-execute-block-python-no-switch-test failed"))

(defun py-execute-block-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-python-dedicated-base arg teststring)))

(defun py-execute-block-python-dedicated-base ()
  (assert (progn (py-execute-block-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-python-test")) nil "py-execute-block-python-dedicated-test failed"))

(defun py-execute-block-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python-dedicated-switch-base arg teststring)))

(defun py-execute-block-python-dedicated-switch-base ()
  (assert (progn (py-execute-block-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-python-test")) nil "py-execute-block-python-dedicated-switch-test failed"))

(defun py-execute-block-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-block-ipython-base arg teststring)))

(defun py-execute-block-ipython-base ()
  (assert (progn (py-execute-block-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-ipython-test")) nil "py-execute-block-ipython-test failed"))

(defun py-execute-block-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-ipython-switch-base arg teststring)))

(defun py-execute-block-ipython-switch-base ()
  (assert (progn (py-execute-block-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-ipython-test")) nil "py-execute-block-ipython-switch-test failed"))

(defun py-execute-block-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-ipython-no-switch-base arg teststring)))

(defun py-execute-block-ipython-no-switch-base ()
  (assert (progn (py-execute-block-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-ipython-test")) nil "py-execute-block-ipython-no-switch-test failed"))

(defun py-execute-block-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-ipython-dedicated-base arg teststring)))

(defun py-execute-block-ipython-dedicated-base ()
  (assert (progn (py-execute-block-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-ipython-test")) nil "py-execute-block-ipython-dedicated-test failed"))

(defun py-execute-block-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-block-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-block-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-ipython-test")) nil "py-execute-block-ipython-dedicated-switch-test failed"))

(defun py-execute-block-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3-base arg teststring)))

(defun py-execute-block-python3-base ()
  (assert (progn (py-execute-block-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-python3-test")) nil "py-execute-block-python3-test failed"))

(defun py-execute-block-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3-switch-base arg teststring)))

(defun py-execute-block-python3-switch-base ()
  (assert (progn (py-execute-block-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-python3-test")) nil "py-execute-block-python3-switch-test failed"))

(defun py-execute-block-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3-no-switch-base arg teststring)))

(defun py-execute-block-python3-no-switch-base ()
  (assert (progn (py-execute-block-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-python3-test")) nil "py-execute-block-python3-no-switch-test failed"))

(defun py-execute-block-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3-dedicated-base arg teststring)))

(defun py-execute-block-python3-dedicated-base ()
  (assert (progn (py-execute-block-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-python3-test")) nil "py-execute-block-python3-dedicated-test failed"))

(defun py-execute-block-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3-dedicated-switch-base arg teststring)))

(defun py-execute-block-python3-dedicated-switch-base ()
  (assert (progn (py-execute-block-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-python3-test")) nil "py-execute-block-python3-dedicated-switch-test failed"))

(defun py-execute-block-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2-base arg teststring)))

(defun py-execute-block-python2-base ()
  (assert (progn (py-execute-block-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-python2-test")) nil "py-execute-block-python2-test failed"))

(defun py-execute-block-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2-switch-base arg teststring)))

(defun py-execute-block-python2-switch-base ()
  (assert (progn (py-execute-block-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-python2-test")) nil "py-execute-block-python2-switch-test failed"))

(defun py-execute-block-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2-no-switch-base arg teststring)))

(defun py-execute-block-python2-no-switch-base ()
  (assert (progn (py-execute-block-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-python2-test")) nil "py-execute-block-python2-no-switch-test failed"))

(defun py-execute-block-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2-dedicated-base arg teststring)))

(defun py-execute-block-python2-dedicated-base ()
  (assert (progn (py-execute-block-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-python2-test")) nil "py-execute-block-python2-dedicated-test failed"))

(defun py-execute-block-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2-dedicated-switch-base arg teststring)))

(defun py-execute-block-python2-dedicated-switch-base ()
  (assert (progn (py-execute-block-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-python2-test")) nil "py-execute-block-python2-dedicated-switch-test failed"))

(defun py-execute-block-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2.7-base arg teststring)))

(defun py-execute-block-python2.7-base ()
  (assert (progn (py-execute-block-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-python2.7-test")) nil "py-execute-block-python2.7-test failed"))

(defun py-execute-block-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2.7-switch-base arg teststring)))

(defun py-execute-block-python2.7-switch-base ()
  (assert (progn (py-execute-block-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-python2.7-test")) nil "py-execute-block-python2.7-switch-test failed"))

(defun py-execute-block-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2.7-no-switch-base arg teststring)))

(defun py-execute-block-python2.7-no-switch-base ()
  (assert (progn (py-execute-block-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-python2.7-test")) nil "py-execute-block-python2.7-no-switch-test failed"))

(defun py-execute-block-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2.7-dedicated-base arg teststring)))

(defun py-execute-block-python2.7-dedicated-base ()
  (assert (progn (py-execute-block-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-python2.7-test")) nil "py-execute-block-python2.7-dedicated-test failed"))

(defun py-execute-block-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-block-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-block-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-python2.7-test")) nil "py-execute-block-python2.7-dedicated-switch-test failed"))

(defun py-execute-block-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-test\")"))
  (py-bug-tests-intern 'py-execute-block-jython-base arg teststring)))

(defun py-execute-block-jython-base ()
  (assert (progn (py-execute-block-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-jython-test")) nil "py-execute-block-jython-test failed"))

(defun py-execute-block-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-jython-switch-base arg teststring)))

(defun py-execute-block-jython-switch-base ()
  (assert (progn (py-execute-block-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-jython-test")) nil "py-execute-block-jython-switch-test failed"))

(defun py-execute-block-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-jython-no-switch-base arg teststring)))

(defun py-execute-block-jython-no-switch-base ()
  (assert (progn (py-execute-block-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-jython-test")) nil "py-execute-block-jython-no-switch-test failed"))

(defun py-execute-block-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-jython-dedicated-base arg teststring)))

(defun py-execute-block-jython-dedicated-base ()
  (assert (progn (py-execute-block-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-jython-test")) nil "py-execute-block-jython-dedicated-test failed"))

(defun py-execute-block-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-jython-dedicated-switch-base arg teststring)))

(defun py-execute-block-jython-dedicated-switch-base ()
  (assert (progn (py-execute-block-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-jython-test")) nil "py-execute-block-jython-dedicated-switch-test failed"))

(defun py-execute-block-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.2-base arg teststring)))

(defun py-execute-block-python3.2-base ()
  (assert (progn (py-execute-block-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-python3.2-test")) nil "py-execute-block-python3.2-test failed"))

(defun py-execute-block-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.2-switch-base arg teststring)))

(defun py-execute-block-python3.2-switch-base ()
  (assert (progn (py-execute-block-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-python3.2-test")) nil "py-execute-block-python3.2-switch-test failed"))

(defun py-execute-block-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.2-no-switch-base arg teststring)))

(defun py-execute-block-python3.2-no-switch-base ()
  (assert (progn (py-execute-block-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-python3.2-test")) nil "py-execute-block-python3.2-no-switch-test failed"))

(defun py-execute-block-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.2-dedicated-base arg teststring)))

(defun py-execute-block-python3.2-dedicated-base ()
  (assert (progn (py-execute-block-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-python3.2-test")) nil "py-execute-block-python3.2-dedicated-test failed"))

(defun py-execute-block-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-block-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-block-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-python3.2-test")) nil "py-execute-block-python3.2-dedicated-switch-test failed"))

(defun py-execute-block-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.3-base arg teststring)))

(defun py-execute-block-python3.3-base ()
  (assert (progn (py-execute-block-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-python3.3-test")) nil "py-execute-block-python3.3-test failed"))

(defun py-execute-block-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.3-switch-base arg teststring)))

(defun py-execute-block-python3.3-switch-base ()
  (assert (progn (py-execute-block-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-python3.3-test")) nil "py-execute-block-python3.3-switch-test failed"))

(defun py-execute-block-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.3-no-switch-base arg teststring)))

(defun py-execute-block-python3.3-no-switch-base ()
  (assert (progn (py-execute-block-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-python3.3-test")) nil "py-execute-block-python3.3-no-switch-test failed"))

(defun py-execute-block-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.3-dedicated-base arg teststring)))

(defun py-execute-block-python3.3-dedicated-base ()
  (assert (progn (py-execute-block-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-python3.3-test")) nil "py-execute-block-python3.3-dedicated-test failed"))

(defun py-execute-block-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-block-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-block-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-python3.3-test")) nil "py-execute-block-python3.3-dedicated-switch-test failed"))

(defun py-execute-block-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-block-bpython-base arg teststring)))

(defun py-execute-block-bpython-base ()
  (assert (progn (py-execute-block-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-bpython-test")) nil "py-execute-block-bpython-test failed"))

(defun py-execute-block-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-bpython-switch-base arg teststring)))

(defun py-execute-block-bpython-switch-base ()
  (assert (progn (py-execute-block-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-bpython-test")) nil "py-execute-block-bpython-switch-test failed"))

(defun py-execute-block-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-bpython-no-switch-base arg teststring)))

(defun py-execute-block-bpython-no-switch-base ()
  (assert (progn (py-execute-block-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-bpython-test")) nil "py-execute-block-bpython-no-switch-test failed"))

(defun py-execute-block-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-bpython-dedicated-base arg teststring)))

(defun py-execute-block-bpython-dedicated-base ()
  (assert (progn (py-execute-block-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-bpython-test")) nil "py-execute-block-bpython-dedicated-test failed"))

(defun py-execute-block-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-block-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-block-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-bpython-test")) nil "py-execute-block-bpython-dedicated-switch-test failed"))

(defun py-execute-clause-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python-base arg teststring)))

(defun py-execute-clause-python-base ()
  (assert (progn (py-execute-clause-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-clause-python-test")) nil "py-execute-clause-python-test failed"))

(defun py-execute-clause-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python-switch-base arg teststring)))

(defun py-execute-clause-python-switch-base ()
  (assert (progn (py-execute-clause-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-clause-python-test")) nil "py-execute-clause-python-switch-test failed"))

(defun py-execute-clause-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python-no-switch-base arg teststring)))

(defun py-execute-clause-python-no-switch-base ()
  (assert (progn (py-execute-clause-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-clause-python-test")) nil "py-execute-clause-python-no-switch-test failed"))

(defun py-execute-clause-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python-dedicated-base arg teststring)))

(defun py-execute-clause-python-dedicated-base ()
  (assert (progn (py-execute-clause-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-clause-python-test")) nil "py-execute-clause-python-dedicated-test failed"))

(defun py-execute-clause-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python-dedicated-switch-base arg teststring)))

(defun py-execute-clause-python-dedicated-switch-base ()
  (assert (progn (py-execute-clause-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-clause-python-test")) nil "py-execute-clause-python-dedicated-switch-test failed"))

(defun py-execute-clause-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-clause-ipython-base arg teststring)))

(defun py-execute-clause-ipython-base ()
  (assert (progn (py-execute-clause-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-clause-ipython-test")) nil "py-execute-clause-ipython-test failed"))

(defun py-execute-clause-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-ipython-switch-base arg teststring)))

(defun py-execute-clause-ipython-switch-base ()
  (assert (progn (py-execute-clause-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-clause-ipython-test")) nil "py-execute-clause-ipython-switch-test failed"))

(defun py-execute-clause-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-ipython-no-switch-base arg teststring)))

(defun py-execute-clause-ipython-no-switch-base ()
  (assert (progn (py-execute-clause-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-clause-ipython-test")) nil "py-execute-clause-ipython-no-switch-test failed"))

(defun py-execute-clause-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-ipython-dedicated-base arg teststring)))

(defun py-execute-clause-ipython-dedicated-base ()
  (assert (progn (py-execute-clause-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-clause-ipython-test")) nil "py-execute-clause-ipython-dedicated-test failed"))

(defun py-execute-clause-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-clause-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-clause-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-clause-ipython-test")) nil "py-execute-clause-ipython-dedicated-switch-test failed"))

(defun py-execute-clause-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3-base arg teststring)))

(defun py-execute-clause-python3-base ()
  (assert (progn (py-execute-clause-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3-test")) nil "py-execute-clause-python3-test failed"))

(defun py-execute-clause-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3-switch-base arg teststring)))

(defun py-execute-clause-python3-switch-base ()
  (assert (progn (py-execute-clause-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3-test")) nil "py-execute-clause-python3-switch-test failed"))

(defun py-execute-clause-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3-no-switch-base arg teststring)))

(defun py-execute-clause-python3-no-switch-base ()
  (assert (progn (py-execute-clause-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3-test")) nil "py-execute-clause-python3-no-switch-test failed"))

(defun py-execute-clause-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3-dedicated-base arg teststring)))

(defun py-execute-clause-python3-dedicated-base ()
  (assert (progn (py-execute-clause-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3-test")) nil "py-execute-clause-python3-dedicated-test failed"))

(defun py-execute-clause-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3-dedicated-switch-base arg teststring)))

(defun py-execute-clause-python3-dedicated-switch-base ()
  (assert (progn (py-execute-clause-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3-test")) nil "py-execute-clause-python3-dedicated-switch-test failed"))

(defun py-execute-clause-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2-base arg teststring)))

(defun py-execute-clause-python2-base ()
  (assert (progn (py-execute-clause-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-clause-python2-test")) nil "py-execute-clause-python2-test failed"))

(defun py-execute-clause-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2-switch-base arg teststring)))

(defun py-execute-clause-python2-switch-base ()
  (assert (progn (py-execute-clause-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-clause-python2-test")) nil "py-execute-clause-python2-switch-test failed"))

(defun py-execute-clause-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2-no-switch-base arg teststring)))

(defun py-execute-clause-python2-no-switch-base ()
  (assert (progn (py-execute-clause-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-clause-python2-test")) nil "py-execute-clause-python2-no-switch-test failed"))

(defun py-execute-clause-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2-dedicated-base arg teststring)))

(defun py-execute-clause-python2-dedicated-base ()
  (assert (progn (py-execute-clause-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-clause-python2-test")) nil "py-execute-clause-python2-dedicated-test failed"))

(defun py-execute-clause-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2-dedicated-switch-base arg teststring)))

(defun py-execute-clause-python2-dedicated-switch-base ()
  (assert (progn (py-execute-clause-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-clause-python2-test")) nil "py-execute-clause-python2-dedicated-switch-test failed"))

(defun py-execute-clause-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2.7-base arg teststring)))

(defun py-execute-clause-python2.7-base ()
  (assert (progn (py-execute-clause-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-clause-python2.7-test")) nil "py-execute-clause-python2.7-test failed"))

(defun py-execute-clause-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2.7-switch-base arg teststring)))

(defun py-execute-clause-python2.7-switch-base ()
  (assert (progn (py-execute-clause-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-clause-python2.7-test")) nil "py-execute-clause-python2.7-switch-test failed"))

(defun py-execute-clause-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2.7-no-switch-base arg teststring)))

(defun py-execute-clause-python2.7-no-switch-base ()
  (assert (progn (py-execute-clause-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-clause-python2.7-test")) nil "py-execute-clause-python2.7-no-switch-test failed"))

(defun py-execute-clause-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2.7-dedicated-base arg teststring)))

(defun py-execute-clause-python2.7-dedicated-base ()
  (assert (progn (py-execute-clause-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-clause-python2.7-test")) nil "py-execute-clause-python2.7-dedicated-test failed"))

(defun py-execute-clause-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-clause-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-clause-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-clause-python2.7-test")) nil "py-execute-clause-python2.7-dedicated-switch-test failed"))

(defun py-execute-clause-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-jython-test\")"))
  (py-bug-tests-intern 'py-execute-clause-jython-base arg teststring)))

(defun py-execute-clause-jython-base ()
  (assert (progn (py-execute-clause-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-clause-jython-test")) nil "py-execute-clause-jython-test failed"))

(defun py-execute-clause-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-jython-switch-base arg teststring)))

(defun py-execute-clause-jython-switch-base ()
  (assert (progn (py-execute-clause-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-clause-jython-test")) nil "py-execute-clause-jython-switch-test failed"))

(defun py-execute-clause-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-jython-no-switch-base arg teststring)))

(defun py-execute-clause-jython-no-switch-base ()
  (assert (progn (py-execute-clause-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-clause-jython-test")) nil "py-execute-clause-jython-no-switch-test failed"))

(defun py-execute-clause-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-jython-dedicated-base arg teststring)))

(defun py-execute-clause-jython-dedicated-base ()
  (assert (progn (py-execute-clause-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-clause-jython-test")) nil "py-execute-clause-jython-dedicated-test failed"))

(defun py-execute-clause-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-jython-dedicated-switch-base arg teststring)))

(defun py-execute-clause-jython-dedicated-switch-base ()
  (assert (progn (py-execute-clause-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-clause-jython-test")) nil "py-execute-clause-jython-dedicated-switch-test failed"))

(defun py-execute-clause-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.2-base arg teststring)))

(defun py-execute-clause-python3.2-base ()
  (assert (progn (py-execute-clause-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.2-test")) nil "py-execute-clause-python3.2-test failed"))

(defun py-execute-clause-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.2-switch-base arg teststring)))

(defun py-execute-clause-python3.2-switch-base ()
  (assert (progn (py-execute-clause-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.2-test")) nil "py-execute-clause-python3.2-switch-test failed"))

(defun py-execute-clause-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.2-no-switch-base arg teststring)))

(defun py-execute-clause-python3.2-no-switch-base ()
  (assert (progn (py-execute-clause-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.2-test")) nil "py-execute-clause-python3.2-no-switch-test failed"))

(defun py-execute-clause-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.2-dedicated-base arg teststring)))

(defun py-execute-clause-python3.2-dedicated-base ()
  (assert (progn (py-execute-clause-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.2-test")) nil "py-execute-clause-python3.2-dedicated-test failed"))

(defun py-execute-clause-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-clause-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-clause-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.2-test")) nil "py-execute-clause-python3.2-dedicated-switch-test failed"))

(defun py-execute-clause-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.3-base arg teststring)))

(defun py-execute-clause-python3.3-base ()
  (assert (progn (py-execute-clause-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.3-test")) nil "py-execute-clause-python3.3-test failed"))

(defun py-execute-clause-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.3-switch-base arg teststring)))

(defun py-execute-clause-python3.3-switch-base ()
  (assert (progn (py-execute-clause-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.3-test")) nil "py-execute-clause-python3.3-switch-test failed"))

(defun py-execute-clause-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.3-no-switch-base arg teststring)))

(defun py-execute-clause-python3.3-no-switch-base ()
  (assert (progn (py-execute-clause-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.3-test")) nil "py-execute-clause-python3.3-no-switch-test failed"))

(defun py-execute-clause-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.3-dedicated-base arg teststring)))

(defun py-execute-clause-python3.3-dedicated-base ()
  (assert (progn (py-execute-clause-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.3-test")) nil "py-execute-clause-python3.3-dedicated-test failed"))

(defun py-execute-clause-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-clause-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-clause-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-clause-python3.3-test")) nil "py-execute-clause-python3.3-dedicated-switch-test failed"))

(defun py-execute-clause-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-clause-bpython-base arg teststring)))

(defun py-execute-clause-bpython-base ()
  (assert (progn (py-execute-clause-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-clause-bpython-test")) nil "py-execute-clause-bpython-test failed"))

(defun py-execute-clause-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-bpython-switch-base arg teststring)))

(defun py-execute-clause-bpython-switch-base ()
  (assert (progn (py-execute-clause-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-clause-bpython-test")) nil "py-execute-clause-bpython-switch-test failed"))

(defun py-execute-clause-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-bpython-no-switch-base arg teststring)))

(defun py-execute-clause-bpython-no-switch-base ()
  (assert (progn (py-execute-clause-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-clause-bpython-test")) nil "py-execute-clause-bpython-no-switch-test failed"))

(defun py-execute-clause-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-clause-bpython-dedicated-base arg teststring)))

(defun py-execute-clause-bpython-dedicated-base ()
  (assert (progn (py-execute-clause-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-clause-bpython-test")) nil "py-execute-clause-bpython-dedicated-test failed"))

(defun py-execute-clause-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-clause-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-clause-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-clause-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-clause-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-clause-bpython-test")) nil "py-execute-clause-bpython-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python-base arg teststring)))

(defun py-execute-block-or-clause-python-base ()
  (assert (progn (py-execute-block-or-clause-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python-test")) nil "py-execute-block-or-clause-python-test failed"))

(defun py-execute-block-or-clause-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python-switch-base arg teststring)))

(defun py-execute-block-or-clause-python-switch-base ()
  (assert (progn (py-execute-block-or-clause-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python-test")) nil "py-execute-block-or-clause-python-switch-test failed"))

(defun py-execute-block-or-clause-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-python-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python-test")) nil "py-execute-block-or-clause-python-no-switch-test failed"))

(defun py-execute-block-or-clause-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python-test")) nil "py-execute-block-or-clause-python-dedicated-test failed"))

(defun py-execute-block-or-clause-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python-test")) nil "py-execute-block-or-clause-python-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-ipython-base arg teststring)))

(defun py-execute-block-or-clause-ipython-base ()
  (assert (progn (py-execute-block-or-clause-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-ipython-test")) nil "py-execute-block-or-clause-ipython-test failed"))

(defun py-execute-block-or-clause-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-ipython-switch-base arg teststring)))

(defun py-execute-block-or-clause-ipython-switch-base ()
  (assert (progn (py-execute-block-or-clause-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-ipython-test")) nil "py-execute-block-or-clause-ipython-switch-test failed"))

(defun py-execute-block-or-clause-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-ipython-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-ipython-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-ipython-test")) nil "py-execute-block-or-clause-ipython-no-switch-test failed"))

(defun py-execute-block-or-clause-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-ipython-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-ipython-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-ipython-test")) nil "py-execute-block-or-clause-ipython-dedicated-test failed"))

(defun py-execute-block-or-clause-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-ipython-test")) nil "py-execute-block-or-clause-ipython-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3-base arg teststring)))

(defun py-execute-block-or-clause-python3-base ()
  (assert (progn (py-execute-block-or-clause-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3-test")) nil "py-execute-block-or-clause-python3-test failed"))

(defun py-execute-block-or-clause-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3-test")) nil "py-execute-block-or-clause-python3-switch-test failed"))

(defun py-execute-block-or-clause-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3-test")) nil "py-execute-block-or-clause-python3-no-switch-test failed"))

(defun py-execute-block-or-clause-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python3-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3-test")) nil "py-execute-block-or-clause-python3-dedicated-test failed"))

(defun py-execute-block-or-clause-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3-test")) nil "py-execute-block-or-clause-python3-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2-base arg teststring)))

(defun py-execute-block-or-clause-python2-base ()
  (assert (progn (py-execute-block-or-clause-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2-test")) nil "py-execute-block-or-clause-python2-test failed"))

(defun py-execute-block-or-clause-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2-switch-base ()
  (assert (progn (py-execute-block-or-clause-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2-test")) nil "py-execute-block-or-clause-python2-switch-test failed"))

(defun py-execute-block-or-clause-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2-test")) nil "py-execute-block-or-clause-python2-no-switch-test failed"))

(defun py-execute-block-or-clause-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python2-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2-test")) nil "py-execute-block-or-clause-python2-dedicated-test failed"))

(defun py-execute-block-or-clause-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2-test")) nil "py-execute-block-or-clause-python2-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-base ()
  (assert (progn (py-execute-block-or-clause-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2.7-test")) nil "py-execute-block-or-clause-python2.7-test failed"))

(defun py-execute-block-or-clause-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-switch-base ()
  (assert (progn (py-execute-block-or-clause-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2.7-test")) nil "py-execute-block-or-clause-python2.7-switch-test failed"))

(defun py-execute-block-or-clause-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2.7-test")) nil "py-execute-block-or-clause-python2.7-no-switch-test failed"))

(defun py-execute-block-or-clause-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2.7-test")) nil "py-execute-block-or-clause-python2.7-dedicated-test failed"))

(defun py-execute-block-or-clause-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python2.7-test")) nil "py-execute-block-or-clause-python2.7-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-jython-base arg teststring)))

(defun py-execute-block-or-clause-jython-base ()
  (assert (progn (py-execute-block-or-clause-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-jython-test")) nil "py-execute-block-or-clause-jython-test failed"))

(defun py-execute-block-or-clause-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-jython-switch-base arg teststring)))

(defun py-execute-block-or-clause-jython-switch-base ()
  (assert (progn (py-execute-block-or-clause-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-jython-test")) nil "py-execute-block-or-clause-jython-switch-test failed"))

(defun py-execute-block-or-clause-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-jython-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-jython-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-jython-test")) nil "py-execute-block-or-clause-jython-no-switch-test failed"))

(defun py-execute-block-or-clause-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-jython-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-jython-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-jython-test")) nil "py-execute-block-or-clause-jython-dedicated-test failed"))

(defun py-execute-block-or-clause-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-jython-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-jython-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-jython-test")) nil "py-execute-block-or-clause-jython-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-base ()
  (assert (progn (py-execute-block-or-clause-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.2-test")) nil "py-execute-block-or-clause-python3.2-test failed"))

(defun py-execute-block-or-clause-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.2-test")) nil "py-execute-block-or-clause-python3.2-switch-test failed"))

(defun py-execute-block-or-clause-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.2-test")) nil "py-execute-block-or-clause-python3.2-no-switch-test failed"))

(defun py-execute-block-or-clause-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.2-test")) nil "py-execute-block-or-clause-python3.2-dedicated-test failed"))

(defun py-execute-block-or-clause-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.2-test")) nil "py-execute-block-or-clause-python3.2-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.3-base arg teststring)))

(defun py-execute-block-or-clause-python3.3-base ()
  (assert (progn (py-execute-block-or-clause-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.3-test")) nil "py-execute-block-or-clause-python3.3-test failed"))

(defun py-execute-block-or-clause-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.3-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.3-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.3-test")) nil "py-execute-block-or-clause-python3.3-switch-test failed"))

(defun py-execute-block-or-clause-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.3-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.3-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.3-test")) nil "py-execute-block-or-clause-python3.3-no-switch-test failed"))

(defun py-execute-block-or-clause-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.3-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python3.3-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.3-test")) nil "py-execute-block-or-clause-python3.3-dedicated-test failed"))

(defun py-execute-block-or-clause-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-python3.3-test")) nil "py-execute-block-or-clause-python3.3-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-bpython-base arg teststring)))

(defun py-execute-block-or-clause-bpython-base ()
  (assert (progn (py-execute-block-or-clause-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-bpython-test")) nil "py-execute-block-or-clause-bpython-test failed"))

(defun py-execute-block-or-clause-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-bpython-switch-base arg teststring)))

(defun py-execute-block-or-clause-bpython-switch-base ()
  (assert (progn (py-execute-block-or-clause-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-bpython-test")) nil "py-execute-block-or-clause-bpython-switch-test failed"))

(defun py-execute-block-or-clause-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-bpython-no-switch-base arg teststring)))

(defun py-execute-block-or-clause-bpython-no-switch-base ()
  (assert (progn (py-execute-block-or-clause-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-bpython-test")) nil "py-execute-block-or-clause-bpython-no-switch-test failed"))

(defun py-execute-block-or-clause-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-bpython-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-bpython-dedicated-base ()
  (assert (progn (py-execute-block-or-clause-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-bpython-test")) nil "py-execute-block-or-clause-bpython-dedicated-test failed"))

(defun py-execute-block-or-clause-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-block-or-clause-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-block-or-clause-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-block-or-clause-bpython-test")) nil "py-execute-block-or-clause-bpython-dedicated-switch-test failed"))

(defun py-execute-def-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-test\")"))
  (py-bug-tests-intern 'py-execute-def-python-base arg teststring)))

(defun py-execute-def-python-base ()
  (assert (progn (py-execute-def-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-def-python-test")) nil "py-execute-def-python-test failed"))

(defun py-execute-def-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python-switch-base arg teststring)))

(defun py-execute-def-python-switch-base ()
  (assert (progn (py-execute-def-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-def-python-test")) nil "py-execute-def-python-switch-test failed"))

(defun py-execute-def-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python-no-switch-base arg teststring)))

(defun py-execute-def-python-no-switch-base ()
  (assert (progn (py-execute-def-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-def-python-test")) nil "py-execute-def-python-no-switch-test failed"))

(defun py-execute-def-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-python-dedicated-base arg teststring)))

(defun py-execute-def-python-dedicated-base ()
  (assert (progn (py-execute-def-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-def-python-test")) nil "py-execute-def-python-dedicated-test failed"))

(defun py-execute-def-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python-dedicated-switch-base arg teststring)))

(defun py-execute-def-python-dedicated-switch-base ()
  (assert (progn (py-execute-def-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-def-python-test")) nil "py-execute-def-python-dedicated-switch-test failed"))

(defun py-execute-def-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-def-ipython-base arg teststring)))

(defun py-execute-def-ipython-base ()
  (assert (progn (py-execute-def-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-def-ipython-test")) nil "py-execute-def-ipython-test failed"))

(defun py-execute-def-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-ipython-switch-base arg teststring)))

(defun py-execute-def-ipython-switch-base ()
  (assert (progn (py-execute-def-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-def-ipython-test")) nil "py-execute-def-ipython-switch-test failed"))

(defun py-execute-def-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-ipython-no-switch-base arg teststring)))

(defun py-execute-def-ipython-no-switch-base ()
  (assert (progn (py-execute-def-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-def-ipython-test")) nil "py-execute-def-ipython-no-switch-test failed"))

(defun py-execute-def-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-ipython-dedicated-base arg teststring)))

(defun py-execute-def-ipython-dedicated-base ()
  (assert (progn (py-execute-def-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-def-ipython-test")) nil "py-execute-def-ipython-dedicated-test failed"))

(defun py-execute-def-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-def-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-def-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-def-ipython-test")) nil "py-execute-def-ipython-dedicated-switch-test failed"))

(defun py-execute-def-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3-base arg teststring)))

(defun py-execute-def-python3-base ()
  (assert (progn (py-execute-def-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-def-python3-test")) nil "py-execute-def-python3-test failed"))

(defun py-execute-def-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3-switch-base arg teststring)))

(defun py-execute-def-python3-switch-base ()
  (assert (progn (py-execute-def-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-def-python3-test")) nil "py-execute-def-python3-switch-test failed"))

(defun py-execute-def-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3-no-switch-base arg teststring)))

(defun py-execute-def-python3-no-switch-base ()
  (assert (progn (py-execute-def-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-def-python3-test")) nil "py-execute-def-python3-no-switch-test failed"))

(defun py-execute-def-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3-dedicated-base arg teststring)))

(defun py-execute-def-python3-dedicated-base ()
  (assert (progn (py-execute-def-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-def-python3-test")) nil "py-execute-def-python3-dedicated-test failed"))

(defun py-execute-def-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3-dedicated-switch-base arg teststring)))

(defun py-execute-def-python3-dedicated-switch-base ()
  (assert (progn (py-execute-def-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-def-python3-test")) nil "py-execute-def-python3-dedicated-switch-test failed"))

(defun py-execute-def-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2-base arg teststring)))

(defun py-execute-def-python2-base ()
  (assert (progn (py-execute-def-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-def-python2-test")) nil "py-execute-def-python2-test failed"))

(defun py-execute-def-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2-switch-base arg teststring)))

(defun py-execute-def-python2-switch-base ()
  (assert (progn (py-execute-def-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-def-python2-test")) nil "py-execute-def-python2-switch-test failed"))

(defun py-execute-def-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2-no-switch-base arg teststring)))

(defun py-execute-def-python2-no-switch-base ()
  (assert (progn (py-execute-def-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-def-python2-test")) nil "py-execute-def-python2-no-switch-test failed"))

(defun py-execute-def-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2-dedicated-base arg teststring)))

(defun py-execute-def-python2-dedicated-base ()
  (assert (progn (py-execute-def-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-def-python2-test")) nil "py-execute-def-python2-dedicated-test failed"))

(defun py-execute-def-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2-dedicated-switch-base arg teststring)))

(defun py-execute-def-python2-dedicated-switch-base ()
  (assert (progn (py-execute-def-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-def-python2-test")) nil "py-execute-def-python2-dedicated-switch-test failed"))

(defun py-execute-def-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2.7-base arg teststring)))

(defun py-execute-def-python2.7-base ()
  (assert (progn (py-execute-def-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-def-python2.7-test")) nil "py-execute-def-python2.7-test failed"))

(defun py-execute-def-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2.7-switch-base arg teststring)))

(defun py-execute-def-python2.7-switch-base ()
  (assert (progn (py-execute-def-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-def-python2.7-test")) nil "py-execute-def-python2.7-switch-test failed"))

(defun py-execute-def-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2.7-no-switch-base arg teststring)))

(defun py-execute-def-python2.7-no-switch-base ()
  (assert (progn (py-execute-def-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-def-python2.7-test")) nil "py-execute-def-python2.7-no-switch-test failed"))

(defun py-execute-def-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2.7-dedicated-base arg teststring)))

(defun py-execute-def-python2.7-dedicated-base ()
  (assert (progn (py-execute-def-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-def-python2.7-test")) nil "py-execute-def-python2.7-dedicated-test failed"))

(defun py-execute-def-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-def-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-def-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-def-python2.7-test")) nil "py-execute-def-python2.7-dedicated-switch-test failed"))

(defun py-execute-def-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-test\")"))
  (py-bug-tests-intern 'py-execute-def-jython-base arg teststring)))

(defun py-execute-def-jython-base ()
  (assert (progn (py-execute-def-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-def-jython-test")) nil "py-execute-def-jython-test failed"))

(defun py-execute-def-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-jython-switch-base arg teststring)))

(defun py-execute-def-jython-switch-base ()
  (assert (progn (py-execute-def-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-def-jython-test")) nil "py-execute-def-jython-switch-test failed"))

(defun py-execute-def-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-jython-no-switch-base arg teststring)))

(defun py-execute-def-jython-no-switch-base ()
  (assert (progn (py-execute-def-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-def-jython-test")) nil "py-execute-def-jython-no-switch-test failed"))

(defun py-execute-def-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-jython-dedicated-base arg teststring)))

(defun py-execute-def-jython-dedicated-base ()
  (assert (progn (py-execute-def-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-def-jython-test")) nil "py-execute-def-jython-dedicated-test failed"))

(defun py-execute-def-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-jython-dedicated-switch-base arg teststring)))

(defun py-execute-def-jython-dedicated-switch-base ()
  (assert (progn (py-execute-def-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-def-jython-test")) nil "py-execute-def-jython-dedicated-switch-test failed"))

(defun py-execute-def-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.2-base arg teststring)))

(defun py-execute-def-python3.2-base ()
  (assert (progn (py-execute-def-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-def-python3.2-test")) nil "py-execute-def-python3.2-test failed"))

(defun py-execute-def-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.2-switch-base arg teststring)))

(defun py-execute-def-python3.2-switch-base ()
  (assert (progn (py-execute-def-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-def-python3.2-test")) nil "py-execute-def-python3.2-switch-test failed"))

(defun py-execute-def-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.2-no-switch-base arg teststring)))

(defun py-execute-def-python3.2-no-switch-base ()
  (assert (progn (py-execute-def-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-def-python3.2-test")) nil "py-execute-def-python3.2-no-switch-test failed"))

(defun py-execute-def-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.2-dedicated-base arg teststring)))

(defun py-execute-def-python3.2-dedicated-base ()
  (assert (progn (py-execute-def-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-def-python3.2-test")) nil "py-execute-def-python3.2-dedicated-test failed"))

(defun py-execute-def-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-def-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-def-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-def-python3.2-test")) nil "py-execute-def-python3.2-dedicated-switch-test failed"))

(defun py-execute-def-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.3-base arg teststring)))

(defun py-execute-def-python3.3-base ()
  (assert (progn (py-execute-def-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-def-python3.3-test")) nil "py-execute-def-python3.3-test failed"))

(defun py-execute-def-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.3-switch-base arg teststring)))

(defun py-execute-def-python3.3-switch-base ()
  (assert (progn (py-execute-def-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-def-python3.3-test")) nil "py-execute-def-python3.3-switch-test failed"))

(defun py-execute-def-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.3-no-switch-base arg teststring)))

(defun py-execute-def-python3.3-no-switch-base ()
  (assert (progn (py-execute-def-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-def-python3.3-test")) nil "py-execute-def-python3.3-no-switch-test failed"))

(defun py-execute-def-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.3-dedicated-base arg teststring)))

(defun py-execute-def-python3.3-dedicated-base ()
  (assert (progn (py-execute-def-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-def-python3.3-test")) nil "py-execute-def-python3.3-dedicated-test failed"))

(defun py-execute-def-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-def-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-def-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-def-python3.3-test")) nil "py-execute-def-python3.3-dedicated-switch-test failed"))

(defun py-execute-def-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-def-bpython-base arg teststring)))

(defun py-execute-def-bpython-base ()
  (assert (progn (py-execute-def-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-def-bpython-test")) nil "py-execute-def-bpython-test failed"))

(defun py-execute-def-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-bpython-switch-base arg teststring)))

(defun py-execute-def-bpython-switch-base ()
  (assert (progn (py-execute-def-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-def-bpython-test")) nil "py-execute-def-bpython-switch-test failed"))

(defun py-execute-def-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-bpython-no-switch-base arg teststring)))

(defun py-execute-def-bpython-no-switch-base ()
  (assert (progn (py-execute-def-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-def-bpython-test")) nil "py-execute-def-bpython-no-switch-test failed"))

(defun py-execute-def-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-def-bpython-dedicated-base arg teststring)))

(defun py-execute-def-bpython-dedicated-base ()
  (assert (progn (py-execute-def-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-def-bpython-test")) nil "py-execute-def-bpython-dedicated-test failed"))

(defun py-execute-def-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-def-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-def-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-def-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-def-bpython-test")) nil "py-execute-def-bpython-dedicated-switch-test failed"))

(defun py-execute-class-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-test\")"))
  (py-bug-tests-intern 'py-execute-class-python-base arg teststring)))

(defun py-execute-class-python-base ()
  (assert (progn (py-execute-class-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-class-python-test")) nil "py-execute-class-python-test failed"))

(defun py-execute-class-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python-switch-base arg teststring)))

(defun py-execute-class-python-switch-base ()
  (assert (progn (py-execute-class-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-class-python-test")) nil "py-execute-class-python-switch-test failed"))

(defun py-execute-class-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python-no-switch-base arg teststring)))

(defun py-execute-class-python-no-switch-base ()
  (assert (progn (py-execute-class-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-class-python-test")) nil "py-execute-class-python-no-switch-test failed"))

(defun py-execute-class-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-python-dedicated-base arg teststring)))

(defun py-execute-class-python-dedicated-base ()
  (assert (progn (py-execute-class-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-class-python-test")) nil "py-execute-class-python-dedicated-test failed"))

(defun py-execute-class-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python-dedicated-switch-base arg teststring)))

(defun py-execute-class-python-dedicated-switch-base ()
  (assert (progn (py-execute-class-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-class-python-test")) nil "py-execute-class-python-dedicated-switch-test failed"))

(defun py-execute-class-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-class-ipython-base arg teststring)))

(defun py-execute-class-ipython-base ()
  (assert (progn (py-execute-class-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-class-ipython-test")) nil "py-execute-class-ipython-test failed"))

(defun py-execute-class-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-ipython-switch-base arg teststring)))

(defun py-execute-class-ipython-switch-base ()
  (assert (progn (py-execute-class-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-class-ipython-test")) nil "py-execute-class-ipython-switch-test failed"))

(defun py-execute-class-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-ipython-no-switch-base arg teststring)))

(defun py-execute-class-ipython-no-switch-base ()
  (assert (progn (py-execute-class-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-class-ipython-test")) nil "py-execute-class-ipython-no-switch-test failed"))

(defun py-execute-class-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-ipython-dedicated-base arg teststring)))

(defun py-execute-class-ipython-dedicated-base ()
  (assert (progn (py-execute-class-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-class-ipython-test")) nil "py-execute-class-ipython-dedicated-test failed"))

(defun py-execute-class-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-class-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-class-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-class-ipython-test")) nil "py-execute-class-ipython-dedicated-switch-test failed"))

(defun py-execute-class-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3-base arg teststring)))

(defun py-execute-class-python3-base ()
  (assert (progn (py-execute-class-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-class-python3-test")) nil "py-execute-class-python3-test failed"))

(defun py-execute-class-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3-switch-base arg teststring)))

(defun py-execute-class-python3-switch-base ()
  (assert (progn (py-execute-class-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-class-python3-test")) nil "py-execute-class-python3-switch-test failed"))

(defun py-execute-class-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3-no-switch-base arg teststring)))

(defun py-execute-class-python3-no-switch-base ()
  (assert (progn (py-execute-class-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-class-python3-test")) nil "py-execute-class-python3-no-switch-test failed"))

(defun py-execute-class-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3-dedicated-base arg teststring)))

(defun py-execute-class-python3-dedicated-base ()
  (assert (progn (py-execute-class-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-class-python3-test")) nil "py-execute-class-python3-dedicated-test failed"))

(defun py-execute-class-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3-dedicated-switch-base arg teststring)))

(defun py-execute-class-python3-dedicated-switch-base ()
  (assert (progn (py-execute-class-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-class-python3-test")) nil "py-execute-class-python3-dedicated-switch-test failed"))

(defun py-execute-class-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2-base arg teststring)))

(defun py-execute-class-python2-base ()
  (assert (progn (py-execute-class-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-class-python2-test")) nil "py-execute-class-python2-test failed"))

(defun py-execute-class-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2-switch-base arg teststring)))

(defun py-execute-class-python2-switch-base ()
  (assert (progn (py-execute-class-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-class-python2-test")) nil "py-execute-class-python2-switch-test failed"))

(defun py-execute-class-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2-no-switch-base arg teststring)))

(defun py-execute-class-python2-no-switch-base ()
  (assert (progn (py-execute-class-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-class-python2-test")) nil "py-execute-class-python2-no-switch-test failed"))

(defun py-execute-class-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2-dedicated-base arg teststring)))

(defun py-execute-class-python2-dedicated-base ()
  (assert (progn (py-execute-class-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-class-python2-test")) nil "py-execute-class-python2-dedicated-test failed"))

(defun py-execute-class-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2-dedicated-switch-base arg teststring)))

(defun py-execute-class-python2-dedicated-switch-base ()
  (assert (progn (py-execute-class-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-class-python2-test")) nil "py-execute-class-python2-dedicated-switch-test failed"))

(defun py-execute-class-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2.7-base arg teststring)))

(defun py-execute-class-python2.7-base ()
  (assert (progn (py-execute-class-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-class-python2.7-test")) nil "py-execute-class-python2.7-test failed"))

(defun py-execute-class-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2.7-switch-base arg teststring)))

(defun py-execute-class-python2.7-switch-base ()
  (assert (progn (py-execute-class-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-class-python2.7-test")) nil "py-execute-class-python2.7-switch-test failed"))

(defun py-execute-class-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2.7-no-switch-base arg teststring)))

(defun py-execute-class-python2.7-no-switch-base ()
  (assert (progn (py-execute-class-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-class-python2.7-test")) nil "py-execute-class-python2.7-no-switch-test failed"))

(defun py-execute-class-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2.7-dedicated-base arg teststring)))

(defun py-execute-class-python2.7-dedicated-base ()
  (assert (progn (py-execute-class-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-class-python2.7-test")) nil "py-execute-class-python2.7-dedicated-test failed"))

(defun py-execute-class-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-class-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-class-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-class-python2.7-test")) nil "py-execute-class-python2.7-dedicated-switch-test failed"))

(defun py-execute-class-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-test\")"))
  (py-bug-tests-intern 'py-execute-class-jython-base arg teststring)))

(defun py-execute-class-jython-base ()
  (assert (progn (py-execute-class-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-class-jython-test")) nil "py-execute-class-jython-test failed"))

(defun py-execute-class-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-jython-switch-base arg teststring)))

(defun py-execute-class-jython-switch-base ()
  (assert (progn (py-execute-class-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-class-jython-test")) nil "py-execute-class-jython-switch-test failed"))

(defun py-execute-class-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-jython-no-switch-base arg teststring)))

(defun py-execute-class-jython-no-switch-base ()
  (assert (progn (py-execute-class-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-class-jython-test")) nil "py-execute-class-jython-no-switch-test failed"))

(defun py-execute-class-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-jython-dedicated-base arg teststring)))

(defun py-execute-class-jython-dedicated-base ()
  (assert (progn (py-execute-class-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-class-jython-test")) nil "py-execute-class-jython-dedicated-test failed"))

(defun py-execute-class-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-jython-dedicated-switch-base arg teststring)))

(defun py-execute-class-jython-dedicated-switch-base ()
  (assert (progn (py-execute-class-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-class-jython-test")) nil "py-execute-class-jython-dedicated-switch-test failed"))

(defun py-execute-class-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.2-base arg teststring)))

(defun py-execute-class-python3.2-base ()
  (assert (progn (py-execute-class-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-class-python3.2-test")) nil "py-execute-class-python3.2-test failed"))

(defun py-execute-class-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.2-switch-base arg teststring)))

(defun py-execute-class-python3.2-switch-base ()
  (assert (progn (py-execute-class-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-class-python3.2-test")) nil "py-execute-class-python3.2-switch-test failed"))

(defun py-execute-class-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.2-no-switch-base arg teststring)))

(defun py-execute-class-python3.2-no-switch-base ()
  (assert (progn (py-execute-class-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-class-python3.2-test")) nil "py-execute-class-python3.2-no-switch-test failed"))

(defun py-execute-class-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.2-dedicated-base arg teststring)))

(defun py-execute-class-python3.2-dedicated-base ()
  (assert (progn (py-execute-class-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-class-python3.2-test")) nil "py-execute-class-python3.2-dedicated-test failed"))

(defun py-execute-class-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-class-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-class-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-class-python3.2-test")) nil "py-execute-class-python3.2-dedicated-switch-test failed"))

(defun py-execute-class-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.3-base arg teststring)))

(defun py-execute-class-python3.3-base ()
  (assert (progn (py-execute-class-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-class-python3.3-test")) nil "py-execute-class-python3.3-test failed"))

(defun py-execute-class-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.3-switch-base arg teststring)))

(defun py-execute-class-python3.3-switch-base ()
  (assert (progn (py-execute-class-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-class-python3.3-test")) nil "py-execute-class-python3.3-switch-test failed"))

(defun py-execute-class-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.3-no-switch-base arg teststring)))

(defun py-execute-class-python3.3-no-switch-base ()
  (assert (progn (py-execute-class-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-class-python3.3-test")) nil "py-execute-class-python3.3-no-switch-test failed"))

(defun py-execute-class-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.3-dedicated-base arg teststring)))

(defun py-execute-class-python3.3-dedicated-base ()
  (assert (progn (py-execute-class-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-class-python3.3-test")) nil "py-execute-class-python3.3-dedicated-test failed"))

(defun py-execute-class-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-class-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-class-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-class-python3.3-test")) nil "py-execute-class-python3.3-dedicated-switch-test failed"))

(defun py-execute-class-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-class-bpython-base arg teststring)))

(defun py-execute-class-bpython-base ()
  (assert (progn (py-execute-class-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-class-bpython-test")) nil "py-execute-class-bpython-test failed"))

(defun py-execute-class-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-bpython-switch-base arg teststring)))

(defun py-execute-class-bpython-switch-base ()
  (assert (progn (py-execute-class-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-class-bpython-test")) nil "py-execute-class-bpython-switch-test failed"))

(defun py-execute-class-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-bpython-no-switch-base arg teststring)))

(defun py-execute-class-bpython-no-switch-base ()
  (assert (progn (py-execute-class-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-class-bpython-test")) nil "py-execute-class-bpython-no-switch-test failed"))

(defun py-execute-class-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-class-bpython-dedicated-base arg teststring)))

(defun py-execute-class-bpython-dedicated-base ()
  (assert (progn (py-execute-class-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-class-bpython-test")) nil "py-execute-class-bpython-dedicated-test failed"))

(defun py-execute-class-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-class-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-class-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-class-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-class-bpython-test")) nil "py-execute-class-bpython-dedicated-switch-test failed"))

(defun py-execute-region-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-test\")"))
  (py-bug-tests-intern 'py-execute-region-python-base arg teststring)))

(defun py-execute-region-python-base ()
  (assert (progn (py-execute-region-python (line-beginning-position) (line-end-position))(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-region-python-test")) nil "py-execute-region-python-test failed"))

(defun py-execute-region-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python-switch-base arg teststring)))

(defun py-execute-region-python-switch-base ()
  (assert (progn (py-execute-region-python-switch (line-beginning-position) (line-end-position))(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-region-python-test")) nil "py-execute-region-python-switch-test failed"))

(defun py-execute-region-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python-no-switch-base arg teststring)))

(defun py-execute-region-python-no-switch-base ()
  (assert (progn (py-execute-region-python-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-region-python-test")) nil "py-execute-region-python-no-switch-test failed"))

(defun py-execute-region-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-python-dedicated-base arg teststring)))

(defun py-execute-region-python-dedicated-base ()
  (assert (progn (py-execute-region-python-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-region-python-test")) nil "py-execute-region-python-dedicated-test failed"))

(defun py-execute-region-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python-dedicated-switch-base arg teststring)))

(defun py-execute-region-python-dedicated-switch-base ()
  (assert (progn (py-execute-region-python-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-region-python-test")) nil "py-execute-region-python-dedicated-switch-test failed"))

(defun py-execute-region-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-region-ipython-base arg teststring)))

(defun py-execute-region-ipython-base ()
  (assert (progn (py-execute-region-ipython (line-beginning-position) (line-end-position))(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-region-ipython-test")) nil "py-execute-region-ipython-test failed"))

(defun py-execute-region-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-ipython-switch-base arg teststring)))

(defun py-execute-region-ipython-switch-base ()
  (assert (progn (py-execute-region-ipython-switch (line-beginning-position) (line-end-position))(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-region-ipython-test")) nil "py-execute-region-ipython-switch-test failed"))

(defun py-execute-region-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-ipython-no-switch-base arg teststring)))

(defun py-execute-region-ipython-no-switch-base ()
  (assert (progn (py-execute-region-ipython-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-region-ipython-test")) nil "py-execute-region-ipython-no-switch-test failed"))

(defun py-execute-region-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-ipython-dedicated-base arg teststring)))

(defun py-execute-region-ipython-dedicated-base ()
  (assert (progn (py-execute-region-ipython-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-region-ipython-test")) nil "py-execute-region-ipython-dedicated-test failed"))

(defun py-execute-region-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-region-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-region-ipython-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-region-ipython-test")) nil "py-execute-region-ipython-dedicated-switch-test failed"))

(defun py-execute-region-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3-base arg teststring)))

(defun py-execute-region-python3-base ()
  (assert (progn (py-execute-region-python3 (line-beginning-position) (line-end-position))(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-region-python3-test")) nil "py-execute-region-python3-test failed"))

(defun py-execute-region-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3-switch-base arg teststring)))

(defun py-execute-region-python3-switch-base ()
  (assert (progn (py-execute-region-python3-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-region-python3-test")) nil "py-execute-region-python3-switch-test failed"))

(defun py-execute-region-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3-no-switch-base arg teststring)))

(defun py-execute-region-python3-no-switch-base ()
  (assert (progn (py-execute-region-python3-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-region-python3-test")) nil "py-execute-region-python3-no-switch-test failed"))

(defun py-execute-region-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3-dedicated-base arg teststring)))

(defun py-execute-region-python3-dedicated-base ()
  (assert (progn (py-execute-region-python3-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-region-python3-test")) nil "py-execute-region-python3-dedicated-test failed"))

(defun py-execute-region-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3-dedicated-switch-base arg teststring)))

(defun py-execute-region-python3-dedicated-switch-base ()
  (assert (progn (py-execute-region-python3-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-region-python3-test")) nil "py-execute-region-python3-dedicated-switch-test failed"))

(defun py-execute-region-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2-base arg teststring)))

(defun py-execute-region-python2-base ()
  (assert (progn (py-execute-region-python2 (line-beginning-position) (line-end-position))(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-region-python2-test")) nil "py-execute-region-python2-test failed"))

(defun py-execute-region-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2-switch-base arg teststring)))

(defun py-execute-region-python2-switch-base ()
  (assert (progn (py-execute-region-python2-switch (line-beginning-position) (line-end-position))(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-region-python2-test")) nil "py-execute-region-python2-switch-test failed"))

(defun py-execute-region-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2-no-switch-base arg teststring)))

(defun py-execute-region-python2-no-switch-base ()
  (assert (progn (py-execute-region-python2-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-region-python2-test")) nil "py-execute-region-python2-no-switch-test failed"))

(defun py-execute-region-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2-dedicated-base arg teststring)))

(defun py-execute-region-python2-dedicated-base ()
  (assert (progn (py-execute-region-python2-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-region-python2-test")) nil "py-execute-region-python2-dedicated-test failed"))

(defun py-execute-region-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2-dedicated-switch-base arg teststring)))

(defun py-execute-region-python2-dedicated-switch-base ()
  (assert (progn (py-execute-region-python2-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-region-python2-test")) nil "py-execute-region-python2-dedicated-switch-test failed"))

(defun py-execute-region-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2.7-base arg teststring)))

(defun py-execute-region-python2.7-base ()
  (assert (progn (py-execute-region-python2.7 (line-beginning-position) (line-end-position))(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-region-python2.7-test")) nil "py-execute-region-python2.7-test failed"))

(defun py-execute-region-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2.7-switch-base arg teststring)))

(defun py-execute-region-python2.7-switch-base ()
  (py-execute-region-python2.7-switch (line-beginning-position) (line-end-position))
  (assert (search-backward "py-execute-region-python2.7-switch-test")
	  nil "py-execute-region-python2.7-switch-test failed"))

(defun py-execute-region-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2.7-no-switch-base arg teststring)))

(defun py-execute-region-python2.7-no-switch-base ()
  (assert (progn (py-execute-region-python2.7-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-region-python2.7-test")) nil "py-execute-region-python2.7-no-switch-test failed"))

(defun py-execute-region-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2.7-dedicated-base arg teststring)))

(defun py-execute-region-python2.7-dedicated-base ()
  (assert (progn (py-execute-region-python2.7-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-region-python2.7-test")) nil "py-execute-region-python2.7-dedicated-test failed"))

(defun py-execute-region-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-region-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-region-python2.7-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-region-python2.7-test")) nil "py-execute-region-python2.7-dedicated-switch-test failed"))

(defun py-execute-region-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-test\")"))
  (py-bug-tests-intern 'py-execute-region-jython-base arg teststring)))

(defun py-execute-region-jython-base ()
  (assert (progn (py-execute-region-jython (line-beginning-position) (line-end-position))(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-region-jython-test")) nil "py-execute-region-jython-test failed"))

(defun py-execute-region-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-jython-switch-base arg teststring)))

(defun py-execute-region-jython-switch-base ()
  (assert (progn (py-execute-region-jython-switch (line-beginning-position) (line-end-position))(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-region-jython-test")) nil "py-execute-region-jython-switch-test failed"))

(defun py-execute-region-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-jython-no-switch-base arg teststring)))

(defun py-execute-region-jython-no-switch-base ()
  (assert (progn (py-execute-region-jython-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-region-jython-test")) nil "py-execute-region-jython-no-switch-test failed"))

(defun py-execute-region-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-jython-dedicated-base arg teststring)))

(defun py-execute-region-jython-dedicated-base ()
  (assert (progn (py-execute-region-jython-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-region-jython-test")) nil "py-execute-region-jython-dedicated-test failed"))

(defun py-execute-region-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-jython-dedicated-switch-base arg teststring)))

(defun py-execute-region-jython-dedicated-switch-base ()
  (assert (progn (py-execute-region-jython-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-region-jython-test")) nil "py-execute-region-jython-dedicated-switch-test failed"))

(defun py-execute-region-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.2-base arg teststring)))

(defun py-execute-region-python3.2-base ()
  (assert (progn (py-execute-region-python3.2 (line-beginning-position) (line-end-position))(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-region-python3.2-test")) nil "py-execute-region-python3.2-test failed"))

(defun py-execute-region-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.2-switch-base arg teststring)))

(defun py-execute-region-python3.2-switch-base ()
  (assert (progn (py-execute-region-python3.2-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-region-python3.2-test")) nil "py-execute-region-python3.2-switch-test failed"))

(defun py-execute-region-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.2-no-switch-base arg teststring)))

(defun py-execute-region-python3.2-no-switch-base ()
  (assert (progn (py-execute-region-python3.2-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-region-python3.2-test")) nil "py-execute-region-python3.2-no-switch-test failed"))

(defun py-execute-region-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.2-dedicated-base arg teststring)))

(defun py-execute-region-python3.2-dedicated-base ()
  (assert (progn (py-execute-region-python3.2-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-region-python3.2-test")) nil "py-execute-region-python3.2-dedicated-test failed"))

(defun py-execute-region-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-region-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-region-python3.2-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-region-python3.2-test")) nil "py-execute-region-python3.2-dedicated-switch-test failed"))

(defun py-execute-region-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.3-base arg teststring)))

(defun py-execute-region-python3.3-base ()
  (assert (progn (py-execute-region-python3.3 (line-beginning-position) (line-end-position))(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-region-python3.3-test")) nil "py-execute-region-python3.3-test failed"))

(defun py-execute-region-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.3-switch-base arg teststring)))

(defun py-execute-region-python3.3-switch-base ()
  (assert (progn (py-execute-region-python3.3-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-region-python3.3-test")) nil "py-execute-region-python3.3-switch-test failed"))

(defun py-execute-region-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.3-no-switch-base arg teststring)))

(defun py-execute-region-python3.3-no-switch-base ()
  (assert (progn (py-execute-region-python3.3-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-region-python3.3-test")) nil "py-execute-region-python3.3-no-switch-test failed"))

(defun py-execute-region-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.3-dedicated-base arg teststring)))

(defun py-execute-region-python3.3-dedicated-base ()
  (assert (progn (py-execute-region-python3.3-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-region-python3.3-test")) nil "py-execute-region-python3.3-dedicated-test failed"))

(defun py-execute-region-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-region-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-region-python3.3-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-region-python3.3-test")) nil "py-execute-region-python3.3-dedicated-switch-test failed"))

(defun py-execute-region-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-region-bpython-base arg teststring)))

(defun py-execute-region-bpython-base ()
  (assert (progn (py-execute-region-bpython (line-beginning-position) (line-end-position))(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-region-bpython-test")) nil "py-execute-region-bpython-test failed"))

(defun py-execute-region-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-bpython-switch-base arg teststring)))

(defun py-execute-region-bpython-switch-base ()
  (assert (progn (py-execute-region-bpython-switch (line-beginning-position) (line-end-position))(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-region-bpython-test")) nil "py-execute-region-bpython-switch-test failed"))

(defun py-execute-region-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-bpython-no-switch-base arg teststring)))

(defun py-execute-region-bpython-no-switch-base ()
  (assert (progn (py-execute-region-bpython-no-switch (line-beginning-position) (line-end-position))(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-region-bpython-test")) nil "py-execute-region-bpython-no-switch-test failed"))

(defun py-execute-region-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-region-bpython-dedicated-base arg teststring)))

(defun py-execute-region-bpython-dedicated-base ()
  (assert (progn (py-execute-region-bpython-dedicated (line-beginning-position) (line-end-position))(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-region-bpython-test")) nil "py-execute-region-bpython-dedicated-test failed"))

(defun py-execute-region-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-region-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-region-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-region-bpython-dedicated-switch (line-beginning-position) (line-end-position))(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-region-bpython-test")) nil "py-execute-region-bpython-dedicated-switch-test failed"))

(defun py-execute-buffer-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python-base arg teststring)))

(defun py-execute-buffer-python-base ()
  (assert (progn (py-execute-buffer-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-buffer-python-test")) nil "py-execute-buffer-python-test failed"))


(defun py-execute-buffer-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python-switch-base arg teststring)))

(defun py-execute-buffer-python-switch-base ()
  (py-execute-buffer-python-switch)
;;  (switch-to-buffer (current-buffer))
  (assert (search-backward "py-execute-buffer-python-switch-test")
	  nil "py-execute-buffer-python-switch-test failed"))

(defun py-execute-buffer-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python-no-switch-base arg teststring)))

(defun py-execute-buffer-python-no-switch-base ()
  (assert (progn (py-execute-buffer-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-buffer-python-test")) nil "py-execute-buffer-python-no-switch-test failed"))

(defun py-execute-buffer-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python-dedicated-base arg teststring)))

(defun py-execute-buffer-python-dedicated-base ()
  (assert (progn (py-execute-buffer-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-buffer-python-test")) nil "py-execute-buffer-python-dedicated-test failed"))

(defun py-execute-buffer-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-buffer-python-test")) nil "py-execute-buffer-python-dedicated-switch-test failed"))

(defun py-execute-buffer-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-ipython-base arg teststring)))

(defun py-execute-buffer-ipython-base ()
  (assert (progn (py-execute-buffer-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-buffer-ipython-test")) nil "py-execute-buffer-ipython-test failed"))

(defun py-execute-buffer-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-ipython-switch-base arg teststring)))

(defun py-execute-buffer-ipython-switch-base ()
  (assert (progn (py-execute-buffer-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-buffer-ipython-test")) nil "py-execute-buffer-ipython-switch-test failed"))

(defun py-execute-buffer-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-ipython-no-switch-base arg teststring)))

(defun py-execute-buffer-ipython-no-switch-base ()
  (assert (progn (py-execute-buffer-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-buffer-ipython-test")) nil "py-execute-buffer-ipython-no-switch-test failed"))

(defun py-execute-buffer-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-ipython-dedicated-base arg teststring)))

(defun py-execute-buffer-ipython-dedicated-base ()
  (assert (progn (py-execute-buffer-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-buffer-ipython-test")) nil "py-execute-buffer-ipython-dedicated-test failed"))

(defun py-execute-buffer-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-buffer-ipython-test")) nil "py-execute-buffer-ipython-dedicated-switch-test failed"))

(defun py-execute-buffer-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3-base arg teststring)))

(defun py-execute-buffer-python3-base ()
  (assert (progn (py-execute-buffer-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3-test")) nil "py-execute-buffer-python3-test failed"))

(defun py-execute-buffer-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3-switch-base arg teststring)))

(defun py-execute-buffer-python3-switch-base ()
  (assert (progn (py-execute-buffer-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3-test")) nil "py-execute-buffer-python3-switch-test failed"))

(defun py-execute-buffer-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3-no-switch-base arg teststring)))

(defun py-execute-buffer-python3-no-switch-base ()
  (assert (progn (py-execute-buffer-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3-test")) nil "py-execute-buffer-python3-no-switch-test failed"))

(defun py-execute-buffer-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3-dedicated-base arg teststring)))

(defun py-execute-buffer-python3-dedicated-base ()
  (assert (progn (py-execute-buffer-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3-test")) nil "py-execute-buffer-python3-dedicated-test failed"))

(defun py-execute-buffer-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python3-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3-test")) nil "py-execute-buffer-python3-dedicated-switch-test failed"))

(defun py-execute-buffer-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2-base arg teststring)))

(defun py-execute-buffer-python2-base ()
  (assert (progn (py-execute-buffer-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2-test")) nil "py-execute-buffer-python2-test failed"))

(defun py-execute-buffer-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2-switch-base arg teststring)))

(defun py-execute-buffer-python2-switch-base ()
  (assert (progn (py-execute-buffer-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2-test")) nil "py-execute-buffer-python2-switch-test failed"))

(defun py-execute-buffer-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2-no-switch-base arg teststring)))

(defun py-execute-buffer-python2-no-switch-base ()
  (assert (progn (py-execute-buffer-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2-test")) nil "py-execute-buffer-python2-no-switch-test failed"))

(defun py-execute-buffer-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2-dedicated-base arg teststring)))

(defun py-execute-buffer-python2-dedicated-base ()
  (assert (progn (py-execute-buffer-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2-test")) nil "py-execute-buffer-python2-dedicated-test failed"))

(defun py-execute-buffer-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python2-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2-test")) nil "py-execute-buffer-python2-dedicated-switch-test failed"))

(defun py-execute-buffer-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2.7-base arg teststring)))

(defun py-execute-buffer-python2.7-base ()
  (assert (progn (py-execute-buffer-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2.7-test")) nil "py-execute-buffer-python2.7-test failed"))

(defun py-execute-buffer-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2.7-switch-base arg teststring)))

(defun py-execute-buffer-python2.7-switch-base ()
  (assert (progn (py-execute-buffer-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2.7-test")) nil "py-execute-buffer-python2.7-switch-test failed"))

(defun py-execute-buffer-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2.7-no-switch-base arg teststring)))

(defun py-execute-buffer-python2.7-no-switch-base ()
  (assert (progn (py-execute-buffer-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2.7-test")) nil "py-execute-buffer-python2.7-no-switch-test failed"))

(defun py-execute-buffer-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2.7-dedicated-base arg teststring)))

(defun py-execute-buffer-python2.7-dedicated-base ()
  (assert (progn (py-execute-buffer-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2.7-test")) nil "py-execute-buffer-python2.7-dedicated-test failed"))

(defun py-execute-buffer-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-buffer-python2.7-test")) nil "py-execute-buffer-python2.7-dedicated-switch-test failed"))

(defun py-execute-buffer-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-jython-base arg teststring)))

(defun py-execute-buffer-jython-base ()
  (assert (progn (py-execute-buffer-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-buffer-jython-test")) nil "py-execute-buffer-jython-test failed"))

(defun py-execute-buffer-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-jython-switch-base arg teststring)))

(defun py-execute-buffer-jython-switch-base ()
  (assert (progn (py-execute-buffer-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-buffer-jython-test")) nil "py-execute-buffer-jython-switch-test failed"))

(defun py-execute-buffer-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-jython-no-switch-base arg teststring)))

(defun py-execute-buffer-jython-no-switch-base ()
  (assert (progn (py-execute-buffer-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-buffer-jython-test")) nil "py-execute-buffer-jython-no-switch-test failed"))

(defun py-execute-buffer-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-jython-dedicated-base arg teststring)))

(defun py-execute-buffer-jython-dedicated-base ()
  (assert (progn (py-execute-buffer-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-buffer-jython-test")) nil "py-execute-buffer-jython-dedicated-test failed"))

(defun py-execute-buffer-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-jython-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-jython-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-buffer-jython-test")) nil "py-execute-buffer-jython-dedicated-switch-test failed"))

(defun py-execute-buffer-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.2-base arg teststring)))

(defun py-execute-buffer-python3.2-base ()
  (assert (progn (py-execute-buffer-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.2-test")) nil "py-execute-buffer-python3.2-test failed"))

(defun py-execute-buffer-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.2-switch-base arg teststring)))

(defun py-execute-buffer-python3.2-switch-base ()
  (assert (progn (py-execute-buffer-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.2-test")) nil "py-execute-buffer-python3.2-switch-test failed"))

(defun py-execute-buffer-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.2-no-switch-base arg teststring)))

(defun py-execute-buffer-python3.2-no-switch-base ()
  (assert (progn (py-execute-buffer-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.2-test")) nil "py-execute-buffer-python3.2-no-switch-test failed"))

(defun py-execute-buffer-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.2-dedicated-base arg teststring)))

(defun py-execute-buffer-python3.2-dedicated-base ()
  (assert (progn (py-execute-buffer-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.2-test")) nil "py-execute-buffer-python3.2-dedicated-test failed"))

(defun py-execute-buffer-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.2-test")) nil "py-execute-buffer-python3.2-dedicated-switch-test failed"))

(defun py-execute-buffer-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.3-base arg teststring)))

(defun py-execute-buffer-python3.3-base ()
  (assert (progn (py-execute-buffer-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.3-test")) nil "py-execute-buffer-python3.3-test failed"))

(defun py-execute-buffer-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.3-switch-base arg teststring)))

(defun py-execute-buffer-python3.3-switch-base ()
  (assert (progn (py-execute-buffer-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.3-test")) nil "py-execute-buffer-python3.3-switch-test failed"))

(defun py-execute-buffer-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.3-no-switch-base arg teststring)))

(defun py-execute-buffer-python3.3-no-switch-base ()
  (assert (progn (py-execute-buffer-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.3-test")) nil "py-execute-buffer-python3.3-no-switch-test failed"))

(defun py-execute-buffer-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.3-dedicated-base arg teststring)))

(defun py-execute-buffer-python3.3-dedicated-base ()
  (assert (progn (py-execute-buffer-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.3-test")) nil "py-execute-buffer-python3.3-dedicated-test failed"))

(defun py-execute-buffer-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-buffer-python3.3-test")) nil "py-execute-buffer-python3.3-dedicated-switch-test failed"))

(defun py-execute-buffer-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-bpython-base arg teststring)))

(defun py-execute-buffer-bpython-base ()
  (assert (progn (py-execute-buffer-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-buffer-bpython-test")) nil "py-execute-buffer-bpython-test failed"))

(defun py-execute-buffer-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-bpython-switch-base arg teststring)))

(defun py-execute-buffer-bpython-switch-base ()
  (assert (progn (py-execute-buffer-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-buffer-bpython-test")) nil "py-execute-buffer-bpython-switch-test failed"))

(defun py-execute-buffer-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-bpython-no-switch-base arg teststring)))

(defun py-execute-buffer-bpython-no-switch-base ()
  (assert (progn (py-execute-buffer-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-buffer-bpython-test")) nil "py-execute-buffer-bpython-no-switch-test failed"))

(defun py-execute-buffer-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-bpython-dedicated-base arg teststring)))

(defun py-execute-buffer-bpython-dedicated-base ()
  (assert (progn (py-execute-buffer-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-buffer-bpython-test")) nil "py-execute-buffer-bpython-dedicated-test failed"))

(defun py-execute-buffer-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-buffer-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-buffer-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-buffer-bpython-test")) nil "py-execute-buffer-bpython-dedicated-switch-test failed"))

(defun py-execute-expression-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python-base arg teststring)))

(defun py-execute-expression-python-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-expression-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-expression-python-test")) nil "py-execute-expression-python-test failed")))

(defun py-execute-expression-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python-switch-base arg teststring)))

(defun py-execute-expression-python-switch-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-expression-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-expression-python-test")) nil "py-execute-expression-python-switch-test failed")))

(defun py-execute-expression-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((py-shell-name "python")
	(teststring "print(\"I'm the py-execute-expression-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python-no-switch-base arg teststring)))

(defun py-execute-expression-python-no-switch-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-expression-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-expression-python-test")) nil "py-execute-expression-python-no-switch-test failed")))

(defun py-execute-expression-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python-dedicated-base arg teststring)))

(defun py-execute-expression-python-dedicated-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-expression-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-expression-python-test")) nil "py-execute-expression-python-dedicated-test failed")))

(defun py-execute-expression-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python-dedicated-switch-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-expression-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-expression-python-test")) nil "py-execute-expression-python-dedicated-switch-test failed")))

(defun py-execute-expression-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-expression-ipython-base arg teststring)))

(defun py-execute-expression-ipython-base ()
  (assert (progn (py-execute-expression-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-expression-ipython-test")) nil "py-execute-expression-ipython-test failed"))

(defun py-execute-expression-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-ipython-switch-base arg teststring)))

(defun py-execute-expression-ipython-switch-base ()
  (assert (progn (py-execute-expression-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-expression-ipython-test")) nil "py-execute-expression-ipython-switch-test failed"))

(defun py-execute-expression-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-ipython-no-switch-base arg teststring)))

(defun py-execute-expression-ipython-no-switch-base ()
  (assert (progn (py-execute-expression-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-expression-ipython-test")) nil "py-execute-expression-ipython-no-switch-test failed"))

(defun py-execute-expression-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-ipython-dedicated-base arg teststring)))

(defun py-execute-expression-ipython-dedicated-base ()
  (assert (progn (py-execute-expression-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-expression-ipython-test")) nil "py-execute-expression-ipython-dedicated-test failed"))

(defun py-execute-expression-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-expression-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-expression-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-expression-ipython-test")) nil "py-execute-expression-ipython-dedicated-switch-test failed"))

(defun py-execute-expression-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3-base arg teststring)))

(defun py-execute-expression-python3-base ()
  (assert (progn (py-execute-expression-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3-test")) nil "py-execute-expression-python3-test failed"))

(defun py-execute-expression-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3-switch-base arg teststring)))

(defun py-execute-expression-python3-switch-base ()
  (assert (progn (py-execute-expression-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3-test")) nil "py-execute-expression-python3-switch-test failed"))

(defun py-execute-expression-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3-no-switch-base arg teststring)))

(defun py-execute-expression-python3-no-switch-base ()
  (assert (progn (py-execute-expression-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3-test")) nil "py-execute-expression-python3-no-switch-test failed"))

(defun py-execute-expression-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3-dedicated-base arg teststring)))

(defun py-execute-expression-python3-dedicated-base ()
  (assert (progn (py-execute-expression-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3-test")) nil "py-execute-expression-python3-dedicated-test failed"))

(defun py-execute-expression-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python3-dedicated-switch-base ()
  (assert (progn (py-execute-expression-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3-test")) nil "py-execute-expression-python3-dedicated-switch-test failed"))

(defun py-execute-expression-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2-base arg teststring)))

(defun py-execute-expression-python2-base ()
  (assert (progn (py-execute-expression-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-expression-python2-test")) nil "py-execute-expression-python2-test failed"))

(defun py-execute-expression-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2-switch-base arg teststring)))

(defun py-execute-expression-python2-switch-base ()
  (assert (progn (py-execute-expression-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-expression-python2-test")) nil "py-execute-expression-python2-switch-test failed"))

(defun py-execute-expression-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2-no-switch-base arg teststring)))

(defun py-execute-expression-python2-no-switch-base ()
  (assert (progn (py-execute-expression-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-expression-python2-test")) nil "py-execute-expression-python2-no-switch-test failed"))

(defun py-execute-expression-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2-dedicated-base arg teststring)))

(defun py-execute-expression-python2-dedicated-base ()
  (assert (progn (py-execute-expression-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-expression-python2-test")) nil "py-execute-expression-python2-dedicated-test failed"))

(defun py-execute-expression-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python2-dedicated-switch-base ()
  (assert (progn (py-execute-expression-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-expression-python2-test")) nil "py-execute-expression-python2-dedicated-switch-test failed"))

(defun py-execute-expression-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2.7-base arg teststring)))

(defun py-execute-expression-python2.7-base ()
  (assert (progn (py-execute-expression-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-expression-python2.7-test")) nil "py-execute-expression-python2.7-test failed"))

(defun py-execute-expression-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2.7-switch-base arg teststring)))

(defun py-execute-expression-python2.7-switch-base ()
  (assert (progn (py-execute-expression-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-expression-python2.7-test")) nil "py-execute-expression-python2.7-switch-test failed"))

(defun py-execute-expression-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2.7-no-switch-base arg teststring)))

(defun py-execute-expression-python2.7-no-switch-base ()
  (assert (progn (py-execute-expression-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-expression-python2.7-test")) nil "py-execute-expression-python2.7-no-switch-test failed"))

(defun py-execute-expression-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2.7-dedicated-base arg teststring)))

(defun py-execute-expression-python2.7-dedicated-base ()
  (assert (progn (py-execute-expression-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-expression-python2.7-test")) nil "py-execute-expression-python2.7-dedicated-test failed"))

(defun py-execute-expression-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-expression-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-expression-python2.7-test")) nil "py-execute-expression-python2.7-dedicated-switch-test failed"))

(defun py-execute-expression-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-test\")"))
  (py-bug-tests-intern 'py-execute-expression-jython-base arg teststring)))

(defun py-execute-expression-jython-base ()
  (assert (progn (py-execute-expression-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-expression-jython-test")) nil "py-execute-expression-jython-test failed"))

(defun py-execute-expression-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-jython-switch-base arg teststring)))

(defun py-execute-expression-jython-switch-base ()
  (assert (progn (py-execute-expression-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-expression-jython-test")) nil "py-execute-expression-jython-switch-test failed"))

(defun py-execute-expression-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-jython-no-switch-base arg teststring)))

(defun py-execute-expression-jython-no-switch-base ()
  (assert (progn (py-execute-expression-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-expression-jython-test")) nil "py-execute-expression-jython-no-switch-test failed"))

(defun py-execute-expression-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-jython-dedicated-base arg teststring)))

(defun py-execute-expression-jython-dedicated-base ()
  (assert (progn (py-execute-expression-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-expression-jython-test")) nil "py-execute-expression-jython-dedicated-test failed"))

(defun py-execute-expression-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-jython-dedicated-switch-base arg teststring)))

(defun py-execute-expression-jython-dedicated-switch-base ()
  (assert (progn (py-execute-expression-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-expression-jython-test")) nil "py-execute-expression-jython-dedicated-switch-test failed"))

(defun py-execute-expression-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.2-base arg teststring)))

(defun py-execute-expression-python3.2-base ()
  (assert (progn (py-execute-expression-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.2-test")) nil "py-execute-expression-python3.2-test failed"))

(defun py-execute-expression-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.2-switch-base arg teststring)))

(defun py-execute-expression-python3.2-switch-base ()
  (assert (progn (py-execute-expression-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.2-test")) nil "py-execute-expression-python3.2-switch-test failed"))

(defun py-execute-expression-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.2-no-switch-base arg teststring)))

(defun py-execute-expression-python3.2-no-switch-base ()
  (assert (progn (py-execute-expression-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.2-test")) nil "py-execute-expression-python3.2-no-switch-test failed"))

(defun py-execute-expression-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.2-dedicated-base arg teststring)))

(defun py-execute-expression-python3.2-dedicated-base ()
  (assert (progn (py-execute-expression-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.2-test")) nil "py-execute-expression-python3.2-dedicated-test failed"))

(defun py-execute-expression-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-expression-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.2-test")) nil "py-execute-expression-python3.2-dedicated-switch-test failed"))

(defun py-execute-expression-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.3-base arg teststring)))

(defun py-execute-expression-python3.3-base ()
  (assert (progn (py-execute-expression-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.3-test")) nil "py-execute-expression-python3.3-test failed"))

(defun py-execute-expression-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.3-switch-base arg teststring)))

(defun py-execute-expression-python3.3-switch-base ()
  (assert (progn (py-execute-expression-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.3-test")) nil "py-execute-expression-python3.3-switch-test failed"))

(defun py-execute-expression-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.3-no-switch-base arg teststring)))

(defun py-execute-expression-python3.3-no-switch-base ()
  (assert (progn (py-execute-expression-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.3-test")) nil "py-execute-expression-python3.3-no-switch-test failed"))

(defun py-execute-expression-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.3-dedicated-base arg teststring)))

(defun py-execute-expression-python3.3-dedicated-base ()
  (assert (progn (py-execute-expression-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.3-test")) nil "py-execute-expression-python3.3-dedicated-test failed"))

(defun py-execute-expression-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-expression-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-expression-python3.3-test")) nil "py-execute-expression-python3.3-dedicated-switch-test failed"))

(defun py-execute-expression-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-expression-bpython-base arg teststring)))

(defun py-execute-expression-bpython-base ()
  (assert (progn (py-execute-expression-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-expression-bpython-test")) nil "py-execute-expression-bpython-test failed"))

(defun py-execute-expression-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-bpython-switch-base arg teststring)))

(defun py-execute-expression-bpython-switch-base ()
  (assert (progn (py-execute-expression-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-expression-bpython-test")) nil "py-execute-expression-bpython-switch-test failed"))

(defun py-execute-expression-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-bpython-no-switch-base arg teststring)))

(defun py-execute-expression-bpython-no-switch-base ()
  (assert (progn (py-execute-expression-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-expression-bpython-test")) nil "py-execute-expression-bpython-no-switch-test failed"))

(defun py-execute-expression-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-expression-bpython-dedicated-base arg teststring)))

(defun py-execute-expression-bpython-dedicated-base ()
  (assert (progn (py-execute-expression-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-expression-bpython-test")) nil "py-execute-expression-bpython-dedicated-test failed"))

(defun py-execute-expression-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-expression-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-expression-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-expression-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-expression-bpython-test")) nil "py-execute-expression-bpython-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python-base arg teststring)))

(defun py-execute-partial-expression-python-base ()
  (assert (progn (py-execute-partial-expression-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python-test")) nil "py-execute-partial-expression-python-test failed"))

(defun py-execute-partial-expression-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python-switch-base arg teststring)))

(defun py-execute-partial-expression-python-switch-base ()
  (assert (progn (py-execute-partial-expression-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python-test")) nil "py-execute-partial-expression-python-switch-test failed"))

(defun py-execute-partial-expression-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python-no-switch-base arg teststring)))

(defun py-execute-partial-expression-python-no-switch-base ()
  (assert (progn (py-execute-partial-expression-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python-test")) nil "py-execute-partial-expression-python-no-switch-test failed"))

(defun py-execute-partial-expression-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python-dedicated-base ()
  (assert (progn (py-execute-partial-expression-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python-test")) nil "py-execute-partial-expression-python-dedicated-test failed"))

(defun py-execute-partial-expression-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python-test")) nil "py-execute-partial-expression-python-dedicated-switch-test failed"))

(defun py-execute-partial-expression-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-ipython-base arg teststring)))

(defun py-execute-partial-expression-ipython-base ()
  (assert (progn (py-execute-partial-expression-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-ipython-test")) nil "py-execute-partial-expression-ipython-test failed"))

(defun py-execute-partial-expression-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-ipython-switch-base arg teststring)))

(defun py-execute-partial-expression-ipython-switch-base ()
  (assert (progn (py-execute-partial-expression-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-ipython-test")) nil "py-execute-partial-expression-ipython-switch-test failed"))

(defun py-execute-partial-expression-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-ipython-no-switch-base arg teststring)))

(defun py-execute-partial-expression-ipython-no-switch-base ()
  (assert (progn (py-execute-partial-expression-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-ipython-test")) nil "py-execute-partial-expression-ipython-no-switch-test failed"))

(defun py-execute-partial-expression-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-ipython-dedicated-base arg teststring)))

(defun py-execute-partial-expression-ipython-dedicated-base ()
  (assert (progn (py-execute-partial-expression-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-ipython-test")) nil "py-execute-partial-expression-ipython-dedicated-test failed"))

(defun py-execute-partial-expression-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-ipython-test")) nil "py-execute-partial-expression-ipython-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3-base arg teststring)))

(defun py-execute-partial-expression-python3-base ()
  (assert (progn (py-execute-partial-expression-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3-test")) nil "py-execute-partial-expression-python3-test failed"))

(defun py-execute-partial-expression-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3-switch-base arg teststring)))

(defun py-execute-partial-expression-python3-switch-base ()
  (assert (progn (py-execute-partial-expression-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3-test")) nil "py-execute-partial-expression-python3-switch-test failed"))

(defun py-execute-partial-expression-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3-no-switch-base arg teststring)))

(defun py-execute-partial-expression-python3-no-switch-base ()
  (assert (progn (py-execute-partial-expression-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3-test")) nil "py-execute-partial-expression-python3-no-switch-test failed"))

(defun py-execute-partial-expression-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python3-dedicated-base ()
  (assert (progn (py-execute-partial-expression-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3-test")) nil "py-execute-partial-expression-python3-dedicated-test failed"))

(defun py-execute-partial-expression-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python3-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3-test")) nil "py-execute-partial-expression-python3-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2-base arg teststring)))

(defun py-execute-partial-expression-python2-base ()
  (assert (progn (py-execute-partial-expression-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2-test")) nil "py-execute-partial-expression-python2-test failed"))

(defun py-execute-partial-expression-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2-switch-base arg teststring)))

(defun py-execute-partial-expression-python2-switch-base ()
  (assert (progn (py-execute-partial-expression-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2-test")) nil "py-execute-partial-expression-python2-switch-test failed"))

(defun py-execute-partial-expression-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2-no-switch-base arg teststring)))

(defun py-execute-partial-expression-python2-no-switch-base ()
  (assert (progn (py-execute-partial-expression-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2-test")) nil "py-execute-partial-expression-python2-no-switch-test failed"))

(defun py-execute-partial-expression-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python2-dedicated-base ()
  (assert (progn (py-execute-partial-expression-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2-test")) nil "py-execute-partial-expression-python2-dedicated-test failed"))

(defun py-execute-partial-expression-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python2-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2-test")) nil "py-execute-partial-expression-python2-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2.7-base arg teststring)))

(defun py-execute-partial-expression-python2.7-base ()
  (assert (progn (py-execute-partial-expression-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2.7-test")) nil "py-execute-partial-expression-python2.7-test failed"))

(defun py-execute-partial-expression-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2.7-switch-base arg teststring)))

(defun py-execute-partial-expression-python2.7-switch-base ()
  (assert (progn (py-execute-partial-expression-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2.7-test")) nil "py-execute-partial-expression-python2.7-switch-test failed"))

(defun py-execute-partial-expression-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2.7-no-switch-base arg teststring)))

(defun py-execute-partial-expression-python2.7-no-switch-base ()
  (assert (progn (py-execute-partial-expression-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2.7-test")) nil "py-execute-partial-expression-python2.7-no-switch-test failed"))

(defun py-execute-partial-expression-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2.7-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python2.7-dedicated-base ()
  (assert (progn (py-execute-partial-expression-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2.7-test")) nil "py-execute-partial-expression-python2.7-dedicated-test failed"))

(defun py-execute-partial-expression-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python2.7-test")) nil "py-execute-partial-expression-python2.7-dedicated-switch-test failed"))

(defun py-execute-partial-expression-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-jython-base arg teststring)))

(defun py-execute-partial-expression-jython-base ()
  (assert (progn (py-execute-partial-expression-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-jython-test")) nil "py-execute-partial-expression-jython-test failed"))

(defun py-execute-partial-expression-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-jython-switch-base arg teststring)))

(defun py-execute-partial-expression-jython-switch-base ()
  (assert (progn (py-execute-partial-expression-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-jython-test")) nil "py-execute-partial-expression-jython-switch-test failed"))

(defun py-execute-partial-expression-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-jython-no-switch-base arg teststring)))

(defun py-execute-partial-expression-jython-no-switch-base ()
  (assert (progn (py-execute-partial-expression-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-jython-test")) nil "py-execute-partial-expression-jython-no-switch-test failed"))

(defun py-execute-partial-expression-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-jython-dedicated-base arg teststring)))

(defun py-execute-partial-expression-jython-dedicated-base ()
  (assert (progn (py-execute-partial-expression-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-jython-test")) nil "py-execute-partial-expression-jython-dedicated-test failed"))

(defun py-execute-partial-expression-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-jython-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-jython-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-jython-test")) nil "py-execute-partial-expression-jython-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.2-base arg teststring)))

(defun py-execute-partial-expression-python3.2-base ()
  (assert (progn (py-execute-partial-expression-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.2-test")) nil "py-execute-partial-expression-python3.2-test failed"))

(defun py-execute-partial-expression-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.2-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.2-switch-base ()
  (assert (progn (py-execute-partial-expression-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.2-test")) nil "py-execute-partial-expression-python3.2-switch-test failed"))

(defun py-execute-partial-expression-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.2-no-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.2-no-switch-base ()
  (assert (progn (py-execute-partial-expression-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.2-test")) nil "py-execute-partial-expression-python3.2-no-switch-test failed"))

(defun py-execute-partial-expression-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.2-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python3.2-dedicated-base ()
  (assert (progn (py-execute-partial-expression-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.2-test")) nil "py-execute-partial-expression-python3.2-dedicated-test failed"))

(defun py-execute-partial-expression-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.2-test")) nil "py-execute-partial-expression-python3.2-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.3-base arg teststring)))

(defun py-execute-partial-expression-python3.3-base ()
  (assert (progn (py-execute-partial-expression-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.3-test")) nil "py-execute-partial-expression-python3.3-test failed"))

(defun py-execute-partial-expression-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.3-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.3-switch-base ()
  (assert (progn (py-execute-partial-expression-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.3-test")) nil "py-execute-partial-expression-python3.3-switch-test failed"))

(defun py-execute-partial-expression-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.3-no-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.3-no-switch-base ()
  (assert (progn (py-execute-partial-expression-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.3-test")) nil "py-execute-partial-expression-python3.3-no-switch-test failed"))

(defun py-execute-partial-expression-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.3-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python3.3-dedicated-base ()
  (assert (progn (py-execute-partial-expression-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.3-test")) nil "py-execute-partial-expression-python3.3-dedicated-test failed"))

(defun py-execute-partial-expression-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-python3.3-test")) nil "py-execute-partial-expression-python3.3-dedicated-switch-test failed"))

(defun py-execute-partial-expression-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-bpython-base arg teststring)))

(defun py-execute-partial-expression-bpython-base ()
  (assert (progn (py-execute-partial-expression-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-bpython-test")) nil "py-execute-partial-expression-bpython-test failed"))

(defun py-execute-partial-expression-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-bpython-switch-base arg teststring)))

(defun py-execute-partial-expression-bpython-switch-base ()
  (assert (progn (py-execute-partial-expression-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-bpython-test")) nil "py-execute-partial-expression-bpython-switch-test failed"))

(defun py-execute-partial-expression-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-bpython-no-switch-base arg teststring)))

(defun py-execute-partial-expression-bpython-no-switch-base ()
  (assert (progn (py-execute-partial-expression-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-bpython-test")) nil "py-execute-partial-expression-bpython-no-switch-test failed"))

(defun py-execute-partial-expression-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-bpython-dedicated-base arg teststring)))

(defun py-execute-partial-expression-bpython-dedicated-base ()
  (assert (progn (py-execute-partial-expression-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-bpython-test")) nil "py-execute-partial-expression-bpython-dedicated-test failed"))

(defun py-execute-partial-expression-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-partial-expression-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-partial-expression-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-partial-expression-bpython-test")) nil "py-execute-partial-expression-bpython-dedicated-switch-test failed"))

(defun py-execute-line-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-test\")"))
  (py-bug-tests-intern 'py-execute-line-python-base arg teststring)))

(defun py-execute-line-python-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-line-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-line-python-test")) nil "py-execute-line-python-test failed")))

(defun py-execute-line-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python-switch-base arg teststring)))

(defun py-execute-line-python-switch-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-line-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-line-python-test")) nil "py-execute-line-python-switch-test failed")))

(defun py-execute-line-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python-no-switch-base arg teststring)))

(defun py-execute-line-python-no-switch-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-line-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-line-python-test")) nil "py-execute-line-python-no-switch-test failed")))

(defun py-execute-line-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-python-dedicated-base arg teststring)))

(defun py-execute-line-python-dedicated-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-line-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-line-python-test")) nil "py-execute-line-python-dedicated-test failed")))

(defun py-execute-line-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python-dedicated-switch-base arg teststring)))

(defun py-execute-line-python-dedicated-switch-base ()
  (let ((py-shell-name "python"))
    (assert (progn (py-execute-line-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-line-python-test")) nil "py-execute-line-python-dedicated-switch-test failed")))

(defun py-execute-line-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-line-ipython-base arg teststring)))

(defun py-execute-line-ipython-base ()
  (assert (progn (py-execute-line-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-line-ipython-test")) nil "py-execute-line-ipython-test failed"))

(defun py-execute-line-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-ipython-switch-base arg teststring)))

(defun py-execute-line-ipython-switch-base ()
  (assert (progn (py-execute-line-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-line-ipython-test")) nil "py-execute-line-ipython-switch-test failed"))

(defun py-execute-line-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-ipython-no-switch-base arg teststring)))

(defun py-execute-line-ipython-no-switch-base ()
  (assert (progn (py-execute-line-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-line-ipython-test")) nil "py-execute-line-ipython-no-switch-test failed"))

(defun py-execute-line-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-ipython-dedicated-base arg teststring)))

(defun py-execute-line-ipython-dedicated-base ()
  (assert (progn (py-execute-line-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-line-ipython-test")) nil "py-execute-line-ipython-dedicated-test failed"))

(defun py-execute-line-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-line-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-line-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-line-ipython-test")) nil "py-execute-line-ipython-dedicated-switch-test failed"))

(defun py-execute-line-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3-base arg teststring)))

(defun py-execute-line-python3-base ()
  (assert (progn (py-execute-line-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-line-python3-test")) nil "py-execute-line-python3-test failed"))

(defun py-execute-line-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3-switch-base arg teststring)))

(defun py-execute-line-python3-switch-base ()
  (assert (progn (py-execute-line-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-line-python3-test")) nil "py-execute-line-python3-switch-test failed"))

(defun py-execute-line-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3-no-switch-base arg teststring)))

(defun py-execute-line-python3-no-switch-base ()
  (assert (progn (py-execute-line-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-line-python3-test")) nil "py-execute-line-python3-no-switch-test failed"))

(defun py-execute-line-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3-dedicated-base arg teststring)))

(defun py-execute-line-python3-dedicated-base ()
  (assert (progn (py-execute-line-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-line-python3-test")) nil "py-execute-line-python3-dedicated-test failed"))

(defun py-execute-line-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3-dedicated-switch-base arg teststring)))

(defun py-execute-line-python3-dedicated-switch-base ()
  (assert (progn (py-execute-line-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-line-python3-test")) nil "py-execute-line-python3-dedicated-switch-test failed"))

(defun py-execute-line-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2-base arg teststring)))

(defun py-execute-line-python2-base ()
  (assert (progn (py-execute-line-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-line-python2-test")) nil "py-execute-line-python2-test failed"))

(defun py-execute-line-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2-switch-base arg teststring)))

(defun py-execute-line-python2-switch-base ()
  (assert (progn (py-execute-line-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-line-python2-test")) nil "py-execute-line-python2-switch-test failed"))

(defun py-execute-line-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2-no-switch-base arg teststring)))

(defun py-execute-line-python2-no-switch-base ()
  (assert (progn (py-execute-line-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-line-python2-test")) nil "py-execute-line-python2-no-switch-test failed"))

(defun py-execute-line-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2-dedicated-base arg teststring)))

(defun py-execute-line-python2-dedicated-base ()
  (assert (progn (py-execute-line-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-line-python2-test")) nil "py-execute-line-python2-dedicated-test failed"))

(defun py-execute-line-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2-dedicated-switch-base arg teststring)))

(defun py-execute-line-python2-dedicated-switch-base ()
  (assert (progn (py-execute-line-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-line-python2-test")) nil "py-execute-line-python2-dedicated-switch-test failed"))

(defun py-execute-line-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2.7-base arg teststring)))

(defun py-execute-line-python2.7-base ()
  (assert (progn (py-execute-line-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-line-python2.7-test")) nil "py-execute-line-python2.7-test failed"))

(defun py-execute-line-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2.7-switch-base arg teststring)))

(defun py-execute-line-python2.7-switch-base ()
  (assert (progn (py-execute-line-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-line-python2.7-test")) nil "py-execute-line-python2.7-switch-test failed"))

(defun py-execute-line-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2.7-no-switch-base arg teststring)))

(defun py-execute-line-python2.7-no-switch-base ()
  (assert (progn (py-execute-line-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-line-python2.7-test")) nil "py-execute-line-python2.7-no-switch-test failed"))

(defun py-execute-line-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2.7-dedicated-base arg teststring)))

(defun py-execute-line-python2.7-dedicated-base ()
  (assert (progn (py-execute-line-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-line-python2.7-test")) nil "py-execute-line-python2.7-dedicated-test failed"))

(defun py-execute-line-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-line-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-line-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-line-python2.7-test")) nil "py-execute-line-python2.7-dedicated-switch-test failed"))

(defun py-execute-line-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-test\")"))
  (py-bug-tests-intern 'py-execute-line-jython-base arg teststring)))

(defun py-execute-line-jython-base ()
  (assert (progn (py-execute-line-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-line-jython-test")) nil "py-execute-line-jython-test failed"))

(defun py-execute-line-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-jython-switch-base arg teststring)))

(defun py-execute-line-jython-switch-base ()
  (assert (progn (py-execute-line-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-line-jython-test")) nil "py-execute-line-jython-switch-test failed"))

(defun py-execute-line-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-jython-no-switch-base arg teststring)))

(defun py-execute-line-jython-no-switch-base ()
  (assert (progn (py-execute-line-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-line-jython-test")) nil "py-execute-line-jython-no-switch-test failed"))

(defun py-execute-line-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-jython-dedicated-base arg teststring)))

(defun py-execute-line-jython-dedicated-base ()
  (assert (progn (py-execute-line-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-line-jython-test")) nil "py-execute-line-jython-dedicated-test failed"))

(defun py-execute-line-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-jython-dedicated-switch-base arg teststring)))

(defun py-execute-line-jython-dedicated-switch-base ()
  (assert (progn (py-execute-line-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-line-jython-test")) nil "py-execute-line-jython-dedicated-switch-test failed"))

(defun py-execute-line-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.2-base arg teststring)))

(defun py-execute-line-python3.2-base ()
  (assert (progn (py-execute-line-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-line-python3.2-test")) nil "py-execute-line-python3.2-test failed"))

(defun py-execute-line-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.2-switch-base arg teststring)))

(defun py-execute-line-python3.2-switch-base ()
  (assert (progn (py-execute-line-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-line-python3.2-test")) nil "py-execute-line-python3.2-switch-test failed"))

(defun py-execute-line-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.2-no-switch-base arg teststring)))

(defun py-execute-line-python3.2-no-switch-base ()
  (assert (progn (py-execute-line-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-line-python3.2-test")) nil "py-execute-line-python3.2-no-switch-test failed"))

(defun py-execute-line-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.2-dedicated-base arg teststring)))

(defun py-execute-line-python3.2-dedicated-base ()
  (assert (progn (py-execute-line-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-line-python3.2-test")) nil "py-execute-line-python3.2-dedicated-test failed"))

(defun py-execute-line-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-line-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-line-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-line-python3.2-test")) nil "py-execute-line-python3.2-dedicated-switch-test failed"))

(defun py-execute-line-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.3-base arg teststring)))

(defun py-execute-line-python3.3-base ()
  (assert (progn (py-execute-line-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-line-python3.3-test")) nil "py-execute-line-python3.3-test failed"))

(defun py-execute-line-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.3-switch-base arg teststring)))

(defun py-execute-line-python3.3-switch-base ()
  (assert (progn (py-execute-line-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-line-python3.3-test")) nil "py-execute-line-python3.3-switch-test failed"))

(defun py-execute-line-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.3-no-switch-base arg teststring)))

(defun py-execute-line-python3.3-no-switch-base ()
  (assert (progn (py-execute-line-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-line-python3.3-test")) nil "py-execute-line-python3.3-no-switch-test failed"))

(defun py-execute-line-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.3-dedicated-base arg teststring)))

(defun py-execute-line-python3.3-dedicated-base ()
  (assert (progn (py-execute-line-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-line-python3.3-test")) nil "py-execute-line-python3.3-dedicated-test failed"))

(defun py-execute-line-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-line-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-line-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-line-python3.3-test")) nil "py-execute-line-python3.3-dedicated-switch-test failed"))

(defun py-execute-line-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-line-bpython-base arg teststring)))

(defun py-execute-line-bpython-base ()
  (assert (progn (py-execute-line-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-line-bpython-test")) nil "py-execute-line-bpython-test failed"))

(defun py-execute-line-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-bpython-switch-base arg teststring)))

(defun py-execute-line-bpython-switch-base ()
  (assert (progn (py-execute-line-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-line-bpython-test")) nil "py-execute-line-bpython-switch-test failed"))

(defun py-execute-line-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-bpython-no-switch-base arg teststring)))

(defun py-execute-line-bpython-no-switch-base ()
  (assert (progn (py-execute-line-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-line-bpython-test")) nil "py-execute-line-bpython-no-switch-test failed"))

(defun py-execute-line-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-line-bpython-dedicated-base arg teststring)))

(defun py-execute-line-bpython-dedicated-base ()
  (assert (progn (py-execute-line-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-line-bpython-test")) nil "py-execute-line-bpython-dedicated-test failed"))

(defun py-execute-line-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-line-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-line-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-line-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-line-bpython-test")) nil "py-execute-line-bpython-dedicated-switch-test failed"))

(defun py-execute-top-level-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python-base arg teststring)))

(defun py-execute-top-level-python-base ()
  (assert (progn (py-execute-top-level-python)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-top-level-python-test")) nil "py-execute-top-level-python-test failed"))

(defun py-execute-top-level-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python-switch-base arg teststring)))

(defun py-execute-top-level-python-switch-base ()
  (assert (progn (py-execute-top-level-python-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-top-level-python-test")) nil "py-execute-top-level-python-switch-test failed"))

(defun py-execute-top-level-python-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python-no-switch-base arg teststring)))

(defun py-execute-top-level-python-no-switch-base ()
  (assert (progn (py-execute-top-level-python-no-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-top-level-python-test")) nil "py-execute-top-level-python-no-switch-test failed"))

(defun py-execute-top-level-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python-dedicated-base arg teststring)))

(defun py-execute-top-level-python-dedicated-base ()
  (assert (progn (py-execute-top-level-python-dedicated)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-top-level-python-test")) nil "py-execute-top-level-python-dedicated-test failed"))

(defun py-execute-top-level-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-python-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-python-dedicated-switch)(set-buffer "*Python*")(goto-char (point-min))(search-forward "the py-execute-top-level-python-test")) nil "py-execute-top-level-python-dedicated-switch-test failed"))

(defun py-execute-top-level-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-ipython-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-ipython-base arg teststring)))

(defun py-execute-top-level-ipython-base ()
  (assert (progn (py-execute-top-level-ipython)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-top-level-ipython-test")) nil "py-execute-top-level-ipython-test failed"))

(defun py-execute-top-level-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-ipython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-ipython-switch-base arg teststring)))

(defun py-execute-top-level-ipython-switch-base ()
  (assert (progn (py-execute-top-level-ipython-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-top-level-ipython-test")) nil "py-execute-top-level-ipython-switch-test failed"))

(defun py-execute-top-level-ipython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-ipython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-ipython-no-switch-base arg teststring)))

(defun py-execute-top-level-ipython-no-switch-base ()
  (assert (progn (py-execute-top-level-ipython-no-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-top-level-ipython-test")) nil "py-execute-top-level-ipython-no-switch-test failed"))

(defun py-execute-top-level-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-ipython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-ipython-dedicated-base arg teststring)))

(defun py-execute-top-level-ipython-dedicated-base ()
  (assert (progn (py-execute-top-level-ipython-dedicated)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-top-level-ipython-test")) nil "py-execute-top-level-ipython-dedicated-test failed"))

(defun py-execute-top-level-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-ipython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-ipython-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-ipython-dedicated-switch)(set-buffer "*Ipython*")(goto-char (point-min))(search-forward "the py-execute-top-level-ipython-test")) nil "py-execute-top-level-ipython-dedicated-switch-test failed"))

(defun py-execute-top-level-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3-base arg teststring)))

(defun py-execute-top-level-python3-base ()
  (assert (progn (py-execute-top-level-python3)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3-test")) nil "py-execute-top-level-python3-test failed"))

(defun py-execute-top-level-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3-switch-base arg teststring)))

(defun py-execute-top-level-python3-switch-base ()
  (assert (progn (py-execute-top-level-python3-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3-test")) nil "py-execute-top-level-python3-switch-test failed"))

(defun py-execute-top-level-python3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3-no-switch-base arg teststring)))

(defun py-execute-top-level-python3-no-switch-base ()
  (assert (progn (py-execute-top-level-python3-no-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3-test")) nil "py-execute-top-level-python3-no-switch-test failed"))

(defun py-execute-top-level-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3-dedicated-base arg teststring)))

(defun py-execute-top-level-python3-dedicated-base ()
  (assert (progn (py-execute-top-level-python3-dedicated)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3-test")) nil "py-execute-top-level-python3-dedicated-test failed"))

(defun py-execute-top-level-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-python3-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-python3-dedicated-switch)(set-buffer "*Python3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3-test")) nil "py-execute-top-level-python3-dedicated-switch-test failed"))

(defun py-execute-top-level-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2-base arg teststring)))

(defun py-execute-top-level-python2-base ()
  (assert (progn (py-execute-top-level-python2)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2-test")) nil "py-execute-top-level-python2-test failed"))

(defun py-execute-top-level-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2-switch-base arg teststring)))

(defun py-execute-top-level-python2-switch-base ()
  (assert (progn (py-execute-top-level-python2-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2-test")) nil "py-execute-top-level-python2-switch-test failed"))

(defun py-execute-top-level-python2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2-no-switch-base arg teststring)))

(defun py-execute-top-level-python2-no-switch-base ()
  (assert (progn (py-execute-top-level-python2-no-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2-test")) nil "py-execute-top-level-python2-no-switch-test failed"))

(defun py-execute-top-level-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2-dedicated-base arg teststring)))

(defun py-execute-top-level-python2-dedicated-base ()
  (assert (progn (py-execute-top-level-python2-dedicated)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2-test")) nil "py-execute-top-level-python2-dedicated-test failed"))

(defun py-execute-top-level-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-python2-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-python2-dedicated-switch)(set-buffer "*Python2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2-test")) nil "py-execute-top-level-python2-dedicated-switch-test failed"))

(defun py-execute-top-level-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2.7-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2.7-base arg teststring)))

(defun py-execute-top-level-python2.7-base ()
  (assert (progn (py-execute-top-level-python2.7)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2.7-test")) nil "py-execute-top-level-python2.7-test failed"))

(defun py-execute-top-level-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2.7-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2.7-switch-base arg teststring)))

(defun py-execute-top-level-python2.7-switch-base ()
  (assert (progn (py-execute-top-level-python2.7-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2.7-test")) nil "py-execute-top-level-python2.7-switch-test failed"))

(defun py-execute-top-level-python2.7-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2.7-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2.7-no-switch-base arg teststring)))

(defun py-execute-top-level-python2.7-no-switch-base ()
  (assert (progn (py-execute-top-level-python2.7-no-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2.7-test")) nil "py-execute-top-level-python2.7-no-switch-test failed"))

(defun py-execute-top-level-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2.7-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2.7-dedicated-base arg teststring)))

(defun py-execute-top-level-python2.7-dedicated-base ()
  (assert (progn (py-execute-top-level-python2.7-dedicated)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2.7-test")) nil "py-execute-top-level-python2.7-dedicated-test failed"))

(defun py-execute-top-level-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python2.7-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-python2.7-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-python2.7-dedicated-switch)(set-buffer "*Python2.7*")(goto-char (point-min))(search-forward "the py-execute-top-level-python2.7-test")) nil "py-execute-top-level-python2.7-dedicated-switch-test failed"))

(defun py-execute-top-level-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-jython-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-jython-base arg teststring)))

(defun py-execute-top-level-jython-base ()
  (assert (progn (py-execute-top-level-jython)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-top-level-jython-test")) nil "py-execute-top-level-jython-test failed"))

(defun py-execute-top-level-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-jython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-jython-switch-base arg teststring)))

(defun py-execute-top-level-jython-switch-base ()
  (assert (progn (py-execute-top-level-jython-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-top-level-jython-test")) nil "py-execute-top-level-jython-switch-test failed"))

(defun py-execute-top-level-jython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-jython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-jython-no-switch-base arg teststring)))

(defun py-execute-top-level-jython-no-switch-base ()
  (assert (progn (py-execute-top-level-jython-no-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-top-level-jython-test")) nil "py-execute-top-level-jython-no-switch-test failed"))

(defun py-execute-top-level-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-jython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-jython-dedicated-base arg teststring)))

(defun py-execute-top-level-jython-dedicated-base ()
  (assert (progn (py-execute-top-level-jython-dedicated)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-top-level-jython-test")) nil "py-execute-top-level-jython-dedicated-test failed"))

(defun py-execute-top-level-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-jython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-jython-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-jython-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-jython-dedicated-switch)(set-buffer "*Jython*")(goto-char (point-min))(search-forward "the py-execute-top-level-jython-test")) nil "py-execute-top-level-jython-dedicated-switch-test failed"))

(defun py-execute-top-level-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.2-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.2-base arg teststring)))

(defun py-execute-top-level-python3.2-base ()
  (assert (progn (py-execute-top-level-python3.2)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.2-test")) nil "py-execute-top-level-python3.2-test failed"))

(defun py-execute-top-level-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.2-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.2-switch-base arg teststring)))

(defun py-execute-top-level-python3.2-switch-base ()
  (assert (progn (py-execute-top-level-python3.2-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.2-test")) nil "py-execute-top-level-python3.2-switch-test failed"))

(defun py-execute-top-level-python3.2-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.2-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.2-no-switch-base arg teststring)))

(defun py-execute-top-level-python3.2-no-switch-base ()
  (assert (progn (py-execute-top-level-python3.2-no-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.2-test")) nil "py-execute-top-level-python3.2-no-switch-test failed"))

(defun py-execute-top-level-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.2-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.2-dedicated-base arg teststring)))

(defun py-execute-top-level-python3.2-dedicated-base ()
  (assert (progn (py-execute-top-level-python3.2-dedicated)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.2-test")) nil "py-execute-top-level-python3.2-dedicated-test failed"))

(defun py-execute-top-level-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.2-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-python3.2-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-python3.2-dedicated-switch)(set-buffer "*Python3.2*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.2-test")) nil "py-execute-top-level-python3.2-dedicated-switch-test failed"))

(defun py-execute-top-level-python3.3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.3-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.3-base arg teststring)))

(defun py-execute-top-level-python3.3-base ()
  (assert (progn (py-execute-top-level-python3.3)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.3-test")) nil "py-execute-top-level-python3.3-test failed"))

(defun py-execute-top-level-python3.3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.3-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.3-switch-base arg teststring)))

(defun py-execute-top-level-python3.3-switch-base ()
  (assert (progn (py-execute-top-level-python3.3-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.3-test")) nil "py-execute-top-level-python3.3-switch-test failed"))

(defun py-execute-top-level-python3.3-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.3-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.3-no-switch-base arg teststring)))

(defun py-execute-top-level-python3.3-no-switch-base ()
  (assert (progn (py-execute-top-level-python3.3-no-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.3-test")) nil "py-execute-top-level-python3.3-no-switch-test failed"))

(defun py-execute-top-level-python3.3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.3-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.3-dedicated-base arg teststring)))

(defun py-execute-top-level-python3.3-dedicated-base ()
  (assert (progn (py-execute-top-level-python3.3-dedicated)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.3-test")) nil "py-execute-top-level-python3.3-dedicated-test failed"))

(defun py-execute-top-level-python3.3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-python3.3-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-python3.3-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-python3.3-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-python3.3-dedicated-switch)(set-buffer "*Python3.3*")(goto-char (point-min))(search-forward "the py-execute-top-level-python3.3-test")) nil "py-execute-top-level-python3.3-dedicated-switch-test failed"))

(defun py-execute-top-level-bpython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-bpython-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-bpython-base arg teststring)))

(defun py-execute-top-level-bpython-base ()
  (assert (progn (py-execute-top-level-bpython)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-top-level-bpython-test")) nil "py-execute-top-level-bpython-test failed"))

(defun py-execute-top-level-bpython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-bpython-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-bpython-switch-base arg teststring)))

(defun py-execute-top-level-bpython-switch-base ()
  (assert (progn (py-execute-top-level-bpython-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-top-level-bpython-test")) nil "py-execute-top-level-bpython-switch-test failed"))

(defun py-execute-top-level-bpython-no-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-bpython-no-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-bpython-no-switch-base arg teststring)))

(defun py-execute-top-level-bpython-no-switch-base ()
  (assert (progn (py-execute-top-level-bpython-no-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-top-level-bpython-test")) nil "py-execute-top-level-bpython-no-switch-test failed"))

(defun py-execute-top-level-bpython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-bpython-dedicated-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-bpython-dedicated-base arg teststring)))

(defun py-execute-top-level-bpython-dedicated-base ()
  (assert (progn (py-execute-top-level-bpython-dedicated)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-top-level-bpython-test")) nil "py-execute-top-level-bpython-dedicated-test failed"))

(defun py-execute-top-level-bpython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-top-level-bpython-dedicated-switch-test\")"))
  (py-bug-tests-intern 'py-execute-top-level-bpython-dedicated-switch-base arg teststring)))

(defun py-execute-top-level-bpython-dedicated-switch-base ()
  (assert (progn (py-execute-top-level-bpython-dedicated-switch)(set-buffer "*Bpython*")(goto-char (point-min))(search-forward "the py-execute-top-level-bpython-test")) nil "py-execute-top-level-bpython-dedicated-switch-test failed"))

(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here
