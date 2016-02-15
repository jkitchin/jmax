;;; vipyr.el --- a vi-like navigation and editing for python

;;; Commentary:
;; vipyr is an attempt to get modal navigation and editing in python-mode
;; like lispy does for emacs-lisp-mode. We define some "special positions" where
;; hot keys work specially for some navigation and editing.

;; We remap [ and ] for navigation. These go backward and forward by statement.
;; To insert these in python-mode, we remap { to insert a pair of curly
;; brackets, and } to insert a pair of square brackets.

;; Here are the current keys and their actions
;; | hotkey | action                                         |
;; |--------+------------------------------------------------|
;; | [      | go to beginning of current/previous statement  |
;; | ]      | go to end of current/next statement            |
;; | {      | insert a pair of {}                            |
;; | }      | insert a pair of []                            |
;; |--------+------------------------------------------------|
;; | n      | next def, block or statement                   |
;; | p      | previous def, block or statement               |
;; | e      | goto end of def, block or statement            |
;; |--------+------------------------------------------------|
;; | l      | jump to a line with avy-goto-line              |
;; | d      | jump to a def or block/statement with avy      |
;; | h      | jump to any def with helm                      |
;; |--------+------------------------------------------------|
;; | m      | mark the def, block or statement               |
;; | w      | copy the def, block or statement               |
;; | c      | clone the def, block or statement              |
;; | k      | kill the def, block or statement               |
;; |--------+------------------------------------------------|
;; | i      | auto-indent the def, block or statement        |
;; |--------+------------------------------------------------|
;; | >      | indent the def, block or statement in a level  |
;; | <      | indent the def, block or statement out a level |
;; |--------+------------------------------------------------|
;; | " "    | (space) type a hotkey in the special place     |
;; | ?      | list hints                                     |
;; | v      | select an action with ivy                      |


;;; Code:
(require 'python)

(defun vsp ()
  "Message about status of point"
  (interactive)
  (message-box "begin defun: %3S
begin block: %3S
begin  stmt: %3S"
	       (looking-at python-nav-beginning-of-defun-regexp)
	       (python-info-beginning-of-block-p)
	       (and (not (python-info-current-line-empty-p))
		    (python-info-beginning-of-statement-p))))


(defvar vipyr-original-cursor-color "#0FB300"
  "Original cursor color. For resetting.")


(defun vipyr-cursor-color ()
  "change cursor color on special places."
  (if (and (eq major-mode 'python-mode)
	   (or (and (not (python-info-current-line-empty-p))
		    (python-info-beginning-of-statement-p))
	       (python-info-beginning-of-block-p)
	       (looking-at python-nav-beginning-of-defun-regexp)))
      (set-cursor-color "DarkOrange")
    (set-cursor-color vipyr-original-cursor-color)))


;; Set an idle timer that will set the cursor color
(defvar vipyr-idle-cursor-timer
  nil
  "Timer to change cursor color.")

(defun vipyr-start-timer ()
  (interactive)
  (setq vipyr-idle-cursor-timer
	(run-with-idle-timer 0.2 t 'vipyr-cursor-color))
  (message "vipyr cursor timer is on."))


(defun vipyr-cancel-timer ()
  (interactive)
  (cancel-timer vipyr-idle-cursor-timer)
  (setq vipyr-idle-cursor-timer nil))


;; this is too slow. but it works.
;; local hook
;; (add-hook 'python-mode-hook
;;	  (lambda ()
;;	    (add-hook 'post-command-hook 'vipyr-cursor-color nil t)))
;;


(add-hook 'python-mode-hook
	  (lambda ()
	    (vipyr-start-timer)))


(add-hook 'kill-buffer-hook
          (lambda ()
            (when (timerp vipyr-idle-cursor-timer)
              (cancel-timer vipyr-idle-cursor-timer))))

;;* Navigation
(define-key python-mode-map (kbd "[")
  (lambda ()
    (interactive)
    (let ((p (point)))
      (when (= p (python-nav-beginning-of-statement))
	(python-nav-backward-statement)))))


(define-key python-mode-map (kbd "]")
  (lambda ()
    "go to end of statement, or next end of statement."
    (interactive)
    (let ((p (point)))
      (when (= p (python-nav-end-of-statement))
	(python-nav-forward-statement)
	(python-nav-end-of-statement)))))


;;** Now make it possible to put {} and [] back in
(define-key python-mode-map (kbd "{") (lambda ()

					(interactive)
					(insert "{}")
					(backward-char)))

(define-key python-mode-map (kbd "}") (lambda ()
					(interactive)
					(insert "[]")
					(backward-char)))



;; * Now the context aware keys
(defvar vipyr-hotkeys '()
  "List of registered keys")

(defmacro vipyr-conditional-key (key docstring conditional)
  "Define a conditional KEY.
DOCSTRING describes the action. CONDITIONAL is a statement that
returns the interactive function for the KEY."
  (add-to-list
   'vipyr-hotkeys
   (cons key docstring)
   t)

  `(define-key python-mode-map ,key
     '(menu-item "python-nav" nil
		 :filter (lambda (&optional _)
			   ,conditional))))

(vipyr-conditional-key
 "?"
 "Display help."
 (cond
  ((or (looking-at python-nav-beginning-of-defun-regexp)
       (python-info-beginning-of-block-p)
       (python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (with-help-window
	 (help-buffer)
       (cl-loop for (key . docstring) in vipyr-hotkeys
		do
		(princ (format "%s %s\n" key docstring))))))))


;; make it possible to overwrite special place. When we are there, press spc,
;; then the letter to insert
(vipyr-conditional-key
 " "
 "Override special position and insert next typed character."
 (cond
  ((or (looking-at
	python-nav-beginning-of-defun-regexp)
       (python-info-beginning-of-block-p)
       (python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (insert (read-char))
     (skip-chars-backward " ")))))


(vipyr-conditional-key
 "v"
 "Use ivy to select action"
 (cond
  ((or (looking-at python-nav-beginning-of-defun-regexp)
       (python-info-beginning-of-block-p)
       (python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (setq
      unread-command-events
      (listify-key-sequence
       (substring
	(ivy-read "Action: "
		  (cl-loop for (key . docstring) in vipyr-hotkeys
			   collect
			   (format "%s %s" key docstring)))
	0 1)))))))


(vipyr-conditional-key
 "H"
 "Use helm to select action"
 (cond
  ((or (looking-at python-nav-beginning-of-defun-regexp)
       (python-info-beginning-of-block-p)
       (python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (setq
      unread-command-events
      (listify-key-sequence
       (helm
	:sources
	(helm-build-sync-source "vipyr"
	  :candidates (cl-loop for (key . docstring) in vipyr-hotkeys
			       collect
			       (cons
				(format "%s %s" key docstring)
				key))
	  :action (lambda (key)
		    (setq unread-command-events
			  (listify-key-sequence key)))))))))))

;;** move to end of defun or block

(vipyr-conditional-key
 "e"
 "Move to end of defun/block/statement"
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   #'python-nav-end-of-defun)
  ;; go to end of block
  ((python-info-beginning-of-block-p)
   #'python-nav-end-of-block)
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   #'python-nav-end-of-statement)))


;;** next def, block, statement
(vipyr-conditional-key
 "n"
 "Move to next def/block/statement"
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (python-nav-forward-defun 2)
     (python-nav-beginning-of-defun)))
  ((python-info-beginning-of-block-p)
   #'python-nav-forward-block)
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   #'python-nav-forward-statement)))


;;** previous def, block, statement
(vipyr-conditional-key
 "p"
 "Goto previous def/block/statement"
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   #'python-nav-backward-defun)
  ((python-info-beginning-of-block-p)
   #'python-nav-backward-block)
  ((and (not (python-info-beginning-of-statement-p))
	(python-info-beginning-of-statement-p))
   #'python-nav-backward-statement)))

;;* avy jumps
;;** jump to a line.
(vipyr-conditional-key
 "l"
 "Jump to line with avy-goto-line"
 (when
     (or (looking-at python-nav-beginning-of-defun-regexp)
	 (python-info-beginning-of-block-p)
	 (and (not (python-info-beginning-of-statement-p))
	      (python-info-beginning-of-statement-p)))
   #'avy-goto-line))

;; ** avy jump to block or def that is visible
(defun avy-python-goto-block ()
  "Jump to a visible block with avy."
  (interactive)
  (let (candidates
	(start (point))
	(begin (window-start))
	(end (window-end)))
    (save-excursion
      (while (python-nav-backward-block)
	(push (point) candidates))
      (goto-char start)

      (while (python-nav-forward-block)
	(push (point) candidates)))
    (avy--process candidates (avy--style-fn 'post))))


(defun avy-python-goto-def ()
  "Jump to a visible def with avy."
  (interactive)
  (let (candidates
	(start (point))
	(begin (window-start))
	(end (window-end)))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward "^def " end t)
	(push (match-beginning 0) candidates)))
    (avy--process candidates (avy--style-fn 'post))))

(vipyr-conditional-key
 "d"
 "Jump to def/block with avy."
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   #'avy-python-goto-def)
  ((or
    (python-info-beginning-of-block-p)
    (and (not (python-info-current-line-empty-p))
	 (python-info-beginning-of-statement-p)))
   #'avy-python-goto-block)))


;;** jump to def with helm
(defvar vipyr-helm-def-candidates '()
  "Variable to hold candidates.")


(defun helm-vipyr-def-candidates ()
  "Get list of defs and positions for helm."
  (save-restriction
    (save-excursion
      (let (candidates)
	(goto-char (point-min))
	(while (re-search-forward
		python-nav-beginning-of-defun-regexp
		(point-max) t)
	  (push (cons (buffer-substring (line-beginning-position)
					(line-end-position))
		      (match-beginning 0))
		candidates))
	(setq vipyr-helm-def-candidates candidates)))))


(defvar vipyr-helm-def
  (helm-build-sync-source "vipyr def"
    :init 'helm-vipyr-def-candidates
    :candidates (lambda () vipyr-helm-def-candidates)
    :action (lambda (candidate)
	      (goto-char candidate))))


(vipyr-conditional-key
 "h"
 "Goto def with helm"
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (helm :sources '(vipyr-helm-def))))))


;;* mark, clone, copy and kill def or block
(vipyr-conditional-key
 "m"
 "Mark the def/block/statement"
 (cond
  ;; mark the def
  ((looking-at python-nav-beginning-of-defun-regexp)
   #'python-mark-defun)
  ;; mark the block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-block)))
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-statement)))))


;; * Comment
(vipyr-conditional-key
 "3"
 "Comment the def/block/statement"
 (cond
  ;; mark the def
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (save-excursion
       (python-mark-defun)
       (comment-region (region-beginning) (region-end)))))
  ;; mark the block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (set-mark (point))
     (save-excursion
       (python-nav-end-of-block)
       (comment-region (region-beginning) (region-end)))))
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (set-mark (point))
     (save-excursion
       (python-nav-end-of-statement)
       (comment-region (region-beginning) (region-end)))))))


(vipyr-conditional-key
 "#"
 "Uncomment the def/block/statement"
 (cond
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (set-mark (point))
     (while (looking-at "#")
       (forward-line))
     (save-excursion
       (python-nav-end-of-statement)
       (uncomment-region (region-beginning) (region-end)))))))


;;** copy
(vipyr-conditional-key
 "w"
 "Copy the def/block/statement"
 (cond
  ;; mark the def
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-defun)
     (kill-new (buffer-substring (region-beginning)
				 (region-end)))))
  ;; mark the block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-block)
     (kill-new (buffer-substring (region-beginning)
				 (region-end)))))
  ;; mark a statement
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-statement)
     (kill-new (buffer-substring (region-beginning)
				 (region-end)))))))


;;** clone - makes a copy and inserts def or block or statement.
(vipyr-conditional-key
 "c"
 "Clone the def/block/statement"
 (cond
  ;; mark the def
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-defun)
     (kill-new (buffer-substring (region-beginning)
				 (region-end)))
     (newline)
     (yank)))
  ;; mark the block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-block)
     (kill-new (buffer-substring (region-beginning)
				 (region-end)))
     (newline)
     (yank)))
  ;; mark a statement
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-statement)
     (kill-new (buffer-substring (region-beginning)
				 (region-end)))
     (newline)
     (yank)))))



;; kill a def or block or statement
(vipyr-conditional-key
 "k"
 "Kill the def/block/statement"
 (cond
  ;; kill the def
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-defun)
     (kill-region (region-beginning)
		  (region-end))))
  ;; mark the block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-block)
     (kill-region (region-beginning)
		  (region-end))))
  ;; mark a statement
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (set-mark (point))
     (python-nav-end-of-statement)
     (kill-region (region-beginning)
		  (region-end))))))


;;* Indentation
(vipyr-conditional-key
 "i"
 "Indent the def/block/statement"
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (save-excursion
       (python-mark-defun)
       ;; this marks the line before the def
       (forward-line 2)
       (let ((pmax (region-end)))
	 (while (< (point) pmax)
	   (python-indent-line)
	   (forward-line))))))
  ;; indent a block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (save-excursion
       (let ((p1 (point))
	     p2)
	 (set-mark (point))
	 (python-nav-end-of-block)
	 (setq p2 (point))
	 (goto-char p1)
	 (forward-line)
	 (while (< (point) p2)
	   (python-indent-line)
	   (forward-line))))))
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (save-excursion
       (let ((p1 (point))
	     p2)
	 (set-mark (point))
	 (python-nav-end-of-statement)
	 (setq p2 (point))
	 (goto-char p1)
	 (forward-line)
	 (while (< (point) p2)
	   (python-indent-line)
	   (forward-line))))))))


;;** change indentation, in and out
(vipyr-conditional-key
 ">"
 "Indent def/block/statement to right"
 (cond
  ((or (looking-at python-nav-beginning-of-defun-regexp)
       (python-info-beginning-of-block-p))
   (lambda ()
     (interactive)
     (save-excursion
       (beginning-of-line)
       (set-mark (point))
       (python-nav-end-of-block)
       (python-indent-shift-right (region-beginning)
				  (region-end)))))
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (save-excursion
       (beginning-of-line)
       (set-mark (point))
       (python-nav-end-of-statement)
       (python-indent-shift-right (region-beginning)
				  (region-end)))))))


(vipyr-conditional-key
 "<"
 "Dedent def/block/statement"
 (cond
  ((or (looking-at python-nav-beginning-of-defun-regexp)
       (python-info-beginning-of-block-p))
   (lambda ()
     (interactive)
     (save-excursion
       (beginning-of-line)
       (set-mark (point))
       (python-nav-end-of-block)
       (python-indent-shift-left (region-beginning)
				 (region-end)))))
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (save-excursion
       (beginning-of-line)
       (set-mark (point))
       (python-nav-end-of-statement)
       (python-indent-shift-left (region-beginning)
				 (region-end)))))))

;;* Send code to REPL
(vipyr-conditional-key
 "s"
 "Send def/block/statement to repl"
 (cond
  ;; on a def
  ((looking-at python-nav-beginning-of-defun-regexp)
   (lambda ()
     (interactive)
     (save-excursion
       (python-mark-defun)
       (elpy-shell-send-region-or-buffer)
       (deactivate-mark))))
  ;; mark the block
  ((python-info-beginning-of-block-p)
   (lambda ()
     (interactive)
     (save-excursion
       (set-mark (point))
       (python-nav-end-of-block)
       (elpy-shell-send-region-or-buffer)
       (deactivate-mark))))
  ((and (not (python-info-current-line-empty-p))
	(python-info-beginning-of-statement-p))
   (lambda ()
     (interactive)
     (save-excursion
       (set-mark (point))
       (python-nav-end-of-statement)
       (elpy-shell-send-region-or-buffer)
       (deactivate-mark))))))


(provide 'vipyr)
;;; vipyr.el ends here
