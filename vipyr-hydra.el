;;; vipyr-hydra.el --- vipyr-hydra for Python navigation/editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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

;;

;;; Code:

(require 'hydra)

(defvar vipyr-original-cursor-color "#0FB300"
  "Original cursor color. For resetting.")

(defhydra vipyr (
		 :hint nil
		       :pre (set-cursor-color "DarkOrange")
		       :post (set-cursor-color vipyr-original-cursor-color))
  "
_[_: back       _]_: forward        _N_: next            _P_: prev         _a_: beg _e_: end
_f_: forwd char _b_: bckwd char     _F_: forwd word      _B_: bckwd word
_._: definition _,_: back to symbol _d_: documentation   _i_: reindent
_#_: comment    _3_: uncomment      _>_: indent          _<_: dedent
_m_: mark       _n_: copy           _t_: cut             _c_: clone
_g_: goto d/b/l _1_: 1 char         _2_: 2-char          _6_: char in line _W_: word _S_: subword
_l_: line       _u_: line above      _v_: line below
_r_: to repl
"
  ("q" nil "quit")

  ("a" (lambda ()
	 (interactive)
	 (beginning-of-line)))

  ("e" (lambda ()
	 (interactive)
	 (end-of-line)))

  ("[" (lambda ()
	 (interactive)
	 (python-nav-backward-statement)))

  ("{" (lambda ()
	 (interactive)
	 (python-nav-backward-block)))

  ("]" (lambda ()
	 (interactive)
	 (let ((p (point)))
	   (python-nav-end-of-statement)
	   (when (= p (point))
	     (forward-char)
	     (python-nav-end-of-statement)))))

  ("}" (lambda ()
	 (interactive)
	 (let ((p (point)))
	   (python-nav-end-of-block)
	   (when (= p (point))
	     (forward-char)
	     (python-nav-end-of-block)))))

  ("." (lambda ()
	 (interactive)
	 (elpy-goto-definition)))

  ("," (lambda ()
	 (interactive)
	 (xref-pop-marker-stack)))

  ("d" (lambda ()
	 (interactive)
	 (elpy-doc)))

  ("f" (forward-char))
  ("F" (forward-word))
  ("b" (backward-char))
  ("B" (backward-word))


  ;; next
  ("N" (lambda ()
	 (interactive)
	 (cond
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (python-nav-forward-defun 2)
	   (python-nav-beginning-of-defun))
	  ((python-info-beginning-of-block-p)
	   (python-nav-forward-block))
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (python-nav-forward-statement)))))

  ;; previous
  ("P" (lambda ()
	 (interactive)
	 (cond
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (python-nav-backward-defun))
	  ((python-info-beginning-of-block-p)
	   (python-nav-backward-block))
	  ((and (not (python-info-beginning-of-statement-p))
		(python-info-beginning-of-statement-p))
	   (python-nav-backward-statement)))))

  ;; goto
  ("g" (lambda ()
	 (interactive)
	 (cond
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (avy-python-goto-def))
	  ((or
	    (python-info-beginning-of-block-p)
	    (and (not (python-info-current-line-empty-p))
		 (python-info-beginning-of-statement-p)))
	   (avy-python-goto-block))
	  (t
	   (avy-goto-line)))))

  ("1" (lambda () (interactive) (call-interactively 'avy-goto-char)))
  ("2" (lambda () (interactive) (call-interactively 'avy-goto-char-2)))
  ("6" (lambda () (interactive) (call-interactively 'avy-goto-char-in-line)))
  ("l" (lambda () (interactive) (call-interactively 'avy-goto-line)))
  ("u" (lambda () (interactive) (call-interactively 'avy-goto-line-above)))
  ("v" (lambda () (interactive) (call-interactively 'avy-goto-line-below)))

  ;; mark
  ("m" (lambda ()
	 (interactive)
	 (cond
	  ;; mark the def
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (python-mark-defun))
	  ;; mark the block
	  ((python-info-beginning-of-block-p)
	   (set-mark (point))
	   (python-nav-end-of-block))
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (set-mark (point))
	   (python-nav-end-of-statement)))))

  ;; comment
  ("#"
   (lambda ()
     (interactive)
     (cond
      ;; mark the def
      ((looking-at python-nav-beginning-of-defun-regexp)
       (python-mark-defun)
       (comment-region (region-beginning) (region-end)))
      ;; mark the block
      ((python-info-beginning-of-block-p)
       (set-mark (point))
       (save-excursion
	 (python-nav-end-of-block)
	 (comment-region (region-beginning) (region-end))))
      ((and (not (python-info-current-line-empty-p))
	    (python-info-beginning-of-statement-p))
       (set-mark (point))
       (save-excursion
	 (python-nav-end-of-statement)
	 (comment-region (region-beginning) (region-end)))))))

  ;; uncomment
  ("3" (lambda ()
	 (interactive)
	 (cond
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (set-mark (point))
	   (while (looking-at "#")
	     (forward-line))
	   (save-excursion
	     (python-nav-end-of-statement)
	     (uncomment-region (region-beginning) (region-end)))))))

  ;; goto word
  ("W" (lambda ()
	 (interactive)
	 (call-interactively 'avy-goto-word-1)))

  ;; goto subword
  ("S" (lambda ()
	 (interactive)
	 (call-interactively 'avy-goto-subword-1)))

  ;; copy
  ("n" (lambda ()
	 (interactive)
	 (cond
	  ;; mark the def
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (set-mark (point))
	   (python-nav-end-of-defun)
	   (kill-new (buffer-substring (region-beginning)
				       (region-end))))
	  ;; mark the block
	  ((python-info-beginning-of-block-p)
	   (set-mark (point))
	   (python-nav-end-of-block)
	   (kill-new (buffer-substring (region-beginning)
				       (region-end))))
	  ;; mark a statement
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (set-mark (point))
	   (python-nav-end-of-statement)
	   (kill-new (buffer-substring (region-beginning)
				       (region-end)))))))

  ;; clone
  ("c" (lambda ()
	 (interactive)
	 (cond
	  ;; mark the def
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (set-mark (point))
	   (python-nav-end-of-defun)
	   (kill-new (buffer-substring (region-beginning)
				       (region-end)))
	   (newline)
	   (yank))
	  ;; mark the block
	  ((python-info-beginning-of-block-p)
	   (set-mark (point))
	   (python-nav-end-of-block)
	   (kill-new (buffer-substring (region-beginning)
				       (region-end)))
	   (newline)
	   (yank))
	  ;; mark a statement
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (set-mark (point))
	   (python-nav-end-of-statement)
	   (kill-new (buffer-substring (region-beginning)
				       (region-end)))
	   (newline)
	   (yank)))))

  ;; cut
  ("t" (lambda ()
	 (interactive)
	 (cond
	  ;; kill the def
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (set-mark (point))
	   (python-nav-end-of-defun)
	   (kill-region (region-beginning)
			(region-end)))
	  ;; mark the block
	  ((python-info-beginning-of-block-p)
	   (set-mark (point))
	   (python-nav-end-of-block)
	   (kill-region (region-beginning)
			(region-end)))
	  ;; mark a statement
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (set-mark (point))
	   (python-nav-end-of-statement)
	   (kill-region (region-beginning)
			(region-end))))))

  ;; reindent
  ("i" (lambda ()
	 (interactive)
	 (cond
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (save-excursion
	     (python-mark-defun)
	     ;; this marks the line before the def
	     (forward-line 2)
	     (let ((pmax (region-end)))
	       (while (< (point) pmax)
		 (python-indent-line)
		 (forward-line)))))
	  ;; indent a block
	  ((python-info-beginning-of-block-p)
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
		 (forward-line)))))
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
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

  ;; indent
  (">" (lambda ()
	 (interactive)
	 (cond
	  ((or (looking-at python-nav-beginning-of-defun-regexp)
	       (python-info-beginning-of-block-p))
	   (save-excursion
	     (beginning-of-line)
	     (set-mark (point))
	     (python-nav-end-of-block)
	     (python-indent-shift-right (region-beginning)
					(region-end))))
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (save-excursion
	     (beginning-of-line)
	     (set-mark (point))
	     (python-nav-end-of-statement)
	     (python-indent-shift-right (region-beginning)
					(region-end)))))))

  ;; dedent
  ("<" (lambda ()
	 (interactive)
	 (cond
	  ((or (looking-at python-nav-beginning-of-defun-regexp)
	       (python-info-beginning-of-block-p))
	   (save-excursion
	     (beginning-of-line)
	     (set-mark (point))
	     (python-nav-end-of-block)
	     (python-indent-shift-left (region-beginning)
				       (region-end))))
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (save-excursion
	     (beginning-of-line)
	     (set-mark (point))
	     (python-nav-end-of-statement)
	     (python-indent-shift-left (region-beginning)
				       (region-end)))))))

  ;; send to repl
  ("r" (lambda ()
	 (interactive)
	 (cond
	  ;; on a def
	  ((looking-at python-nav-beginning-of-defun-regexp)
	   (save-excursion
	     (python-mark-defun)
	     (elpy-shell-send-region-or-buffer)
	     (deactivate-mark)))
	  ;; mark the block
	  ((python-info-beginning-of-block-p)
	   (save-excursion
	     (set-mark (point))
	     (python-nav-end-of-block)
	     (elpy-shell-send-region-or-buffer)
	     (deactivate-mark)))
	  ((and (not (python-info-current-line-empty-p))
		(python-info-beginning-of-statement-p))
	   (save-excursion
	     (set-mark (point))
	     (python-nav-end-of-statement)
	     (elpy-shell-send-region-or-buffer)
	     (deactivate-mark)))))))

(global-set-key (kbd "s-v") 'vipyr/body)

(provide 'vipyr-hydra)
;;; vipyr-hydra.el ends here
