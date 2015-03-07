;;; lab-notebook.el ---    -*- lexical-binding: t -*-
;;; Header:

;;; Commentary:

;; ;;; Reminders about lentic:
;; Lentic key binding reminders
;; C-c , h  Move here in other view
;; C-c , s  Swap windows
;; [[elisp:lentic-mode-split-window-right]]  C-c o to open this link and make the view

;;; Code:
;; ** Setup
;; #+BEGIN_SRC emacs-lisp
(defvar nb-notebook-file
  "~/Dropbox/org-mode/lab-notebook.org"
  "Notebook file where entries are stored.")
;; #+END_SRC
;; ** A capture template
;; #+BEGIN_SRC emacs-lisp
;; capture to a notebook
(add-to-list
 'org-capture-templates
 '("n" "Notebook" entry (file+datetree nb-notebook-file "Notebook")
  "* %?\nEntered on %U\n  %i\n"))

;; #+END_SRC

;; ** Integration with git
;; In the interest of streamlining interactions with version control, here we anticipate our needs and make a function to "do what I need" next in vc. On an empty directory, make it under version control, on a new file, make it under version control. otherwise, commit the changes.

;; #+BEGIN_SRC emacs-lisp
(require 'vc)
(defun dwin-vc ()
  "Do what I need in vc next.
Add current file if not in vc, then prompt for commit message"
  (interactive)
  (save-buffer)

  (let* ((deduction (vc-deduce-fileset nil t 'state-model-only-files))
	 (backend (elt deduction 0))
	 (fileset (elt deduction 1))
	 (state (elt deduction 3)))

    (cond
     (backend  ;; we are in a vc directory
      (when (file-exists-p (buffer-file-name))
	;; register the file if it is not
	(when (eq state 'unregistered)
	  (vc-register))

	;; Now commit it if needed. We know about edited, added,
	;; unregistered files. these should all be committed.
	(cond
	 ((or
	   (eq state 'edited)
	   (eq state 'added)
	   (eq state 'unregistered))
	  (vc-checkin (list (buffer-file-name))
		      backend
		      (read-string "Commit message: ")
		      t
		      ))
	 ((eq state 'up-to-date)
	  (message "up-to-date. No commit needed"))
	 ;; catch other states we do not have a plan for yet.
	 (t
	  (message "state of %s = %s. No action coded." (buffer-file-name) state)))))
     ;; catch case not in vc
     (t
      (message "Not in a VC repo. Perhaps run vc-create-repo?"))
     )))
;; #+END_SRC


;; ** lab-notebook
;; We want a nice function to open our lab notebook, with helm buffer to select headlines.
;; #+BEGIN_SRC emacs-lisp
(defun lab-notebook ()
  (interactive)
  (find-file nb-notebook-file)
  (helm-org-in-buffer-headings)
  (lab-notebook-mode))
;; #+END_SRC




;; ** A minor mode
;; #+BEGIN_SRC emacs-lisp
;;;###autoload
(define-minor-mode lab-notebook-mode
  "Toggle lab notebook mode.
This will prompt you to commit a file when you kill a buffer
"
  ;; initial value
  nil
  ;; indicator
  :lighter " LN"
  ;; keybindings
  :keymap (let ((map (make-sparse-keymap)))
            ;(define-key map (kbd "<f5>") 'export-bibtex)
            map)
  ;; body
  (add-hook 'kill-buffer-hook 'dwin-vc nil 'make-it-local))


;; #+END_SRC




;; * The end
;; #+BEGIN_SRC emacs-lisp
(provide 'lab-notebook)
;; #+END_SRC
;; ;;; lab-notebook.el ends here

;; # Local Variables:
;; # lentic-init: lentic-orgel-org-init
;; # End:
