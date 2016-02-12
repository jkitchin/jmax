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
;; | a      | goto beginning of def, block or statement      |
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


;;; Code:

(defun vsp ()
  "Message about status of point"
  (interactive)
  (message-box "begin defun: %3S
begin block: %3S   end block: %3S
begin  stmt: %3S   end  stmt: %3S"
	   (python-info-looking-at-beginning-of-defun)
	   (python-info-beginning-of-block-p)
	   (python-info-end-of-block-p)
	   (python-info-beginning-of-statement-p)
	   (python-info-end-of-statement-p)))


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


;; make it possible to overwrite special place. When we are there, press spc,
;; then the letter to insert
(define-key python-mode-map " "
  '(menu-item "python-nav" nil
	      :filter (lambda (&optional _)
			(message "space override")
			(cond
			 ((or (python-info-looking-at-beginning-of-defun)
			      (python-info-beginning-of-block-p)
			      (python-info-end-of-block-p)
			      (python-info-beginning-of-statement-p)
			      (python-info-end-of-statement-p))
			  (delete-char -1)
			  (insert-char (read-char))
			  )))))

;;** move to end of defun or block

(define-key python-mode-map "e"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ((python-info-looking-at-beginning-of-defun)
			  #'python-nav-end-of-defun)
			 ;; go to end of block
			 ((python-info-beginning-of-block-p)
			  #'python-nav-end-of-block)
			 ((and (not (python-info-current-line-empty-p))
			       (python-info-beginning-of-statement-p))
			  #'python-nav-end-of-statement)))))


(define-key python-mode-map "a"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			nil
			(cond
			 ((or (python-info-end-of-block-p)
			      (python-info-end-of-statement-p))
			  nil)
			 ;; in a defun at block or statement
			 ((and
			   (or (python-info-beginning-of-block-p)
			       (python-info-beginning-of-statement-p)
			       (python-info-looking-at-beginning-of-defun)
			       (python-info-current-defun)))
			  (lambda ()
			    (interactive)
			    (python-nav-beginning-of-defun)))
			 ((python-info-end-of-block-p)
			  #'python-nav-beginning-of-block)
			 ((python-info-end-of-statement-p)
			  #'python-nav-beginning-of-statement)))))


;;** next def, block, statement
(define-key python-mode-map "n"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ((python-info-looking-at-beginning-of-defun)
			  (lambda ()
			    (interactive)
			    (python-nav-forward-defun 2)
			    (python-nav-beginning-of-defun)))
			 ((python-info-beginning-of-block-p)
			  #'python-nav-forward-block)
			 ((and (not (python-info-beginning-of-statement-p))
			       (python-info-beginning-of-statement-p))
			  #'python-nav-forward-statement)))))


;;** previous def, block, statement
(define-key python-mode-map "p"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ((python-info-looking-at-beginning-of-defun)
			  #'python-nav-backward-defun)
			 ((python-info-beginning-of-block-p)
			  #'python-nav-backward-block)
			 ((and (not (python-info-beginning-of-statement-p))
			       (python-info-beginning-of-statement-p))
			  #'python-nav-backward-statement)))))


;;* avy jumps
;;** jump to a line.
(define-key python-mode-map "l"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(when
			    (or (python-info-looking-at-beginning-of-defun)
				(python-info-beginning-of-block-p)
				(and (not (python-info-beginning-of-statement-p))
				     (python-info-beginning-of-statement-p)))
			  #'avy-goto-line))))


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


(define-key python-mode-map "d"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ((python-info-looking-at-beginning-of-defun)
			  #'avy-python-goto-def)
			 ((or
			   (python-info-beginning-of-block-p)
			   (and (not (python-info-current-line-empty-p))
				(python-info-beginning-of-statement-p)))
			  #'avy-python-goto-block)))))

;;** jump to def with helm
(defvar vipyr-helm-def-candidates '()
  "Variable to hold candidates.")


(defun helm-vipyr-def-candidates ()
  "Get list of defs and positions for helm."
  (message-box (buffer-name))
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


(define-key python-mode-map "h"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ((python-info-looking-at-beginning-of-defun)
			  (lambda ()
			    (interactive)
			    (helm :sources '(vipyr-helm-def))))))))


;;* mark, clone, copy and kill def or block
(define-key python-mode-map "m"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ;; mark the def
			 ((python-info-looking-at-beginning-of-defun)
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
			    (python-nav-end-of-statement)))))))


;;** copy
(define-key python-mode-map "w"
  '(menu-item "python-nav" nil
	      :filter (lambda (&optional _)
			(cond
			 ;; mark the def
			 ((python-info-looking-at-beginning-of-defun)
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
							(region-end)))))))))


;;** clone - makes a copy and inserts def or block or statement.
(define-key python-mode-map "c"
  '(menu-item "python-nav" nil
	      :filter (lambda (&optional _)
			(cond
			 ;; mark the def
			 ((python-info-looking-at-beginning-of-defun)
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
			    (yank)))))))


;; kill a def or block or statement
(define-key python-mode-map "k"
  '(menu-item "python-nav" nil
	      :filter (lambda (&optional _)
			(cond
			 ;; kill the def
			 ((python-info-looking-at-beginning-of-defun)
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
					 (region-end))))))))



;;* Indentation
(define-key python-mode-map "i"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ((python-info-looking-at-beginning-of-defun)
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
				  (forward-line))))))))))


;;** change indentation, in and out

(define-key python-mode-map ">"
  '(menu-item "python-nav" nil
	      :filter (lambda (&optional _)
			(cond
			 ((or (python-info-looking-at-beginning-of-defun)
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
							 (region-end)))))))))

(define-key python-mode-map "<"
  '(menu-item "python-nav" nil
	      :filter (lambda (&optional _)
			(cond
			 ((or (python-info-looking-at-beginning-of-defun)
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
							(region-end)))))))))




(provide 'vipyr)
;;* Send code to REPL
(define-key python-mode-map "s"
  '(menu-item "python-nav" nil
              :filter (lambda (&optional _)
			(cond
			 ;; on a def
			 ((python-info-looking-at-beginning-of-defun)
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
			      (deactivate-mark))))))))

;;; vipyr.el ends here
