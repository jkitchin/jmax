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




;; * Now the context aware keys
(defvar vipyr-hotkeys '()
  "List of registered keys")


(defvar vipyr-mode-map
  (let ((map (make-sparse-keymap)))

    map)
  "Keymap for vipyr-mode.")


(defmacro vipyr-conditional-key (key docstring conditional)
  "Define a conditional KEY.
DOCSTRING describes the action. CONDITIONAL is a statement that
returns the interactive function for the KEY.

Also registers the key in `vipyr-hotkeys' for help generation."
  (add-to-list
   'vipyr-hotkeys
   (cons key docstring)
   t)

  `(define-key vipyr-mode-map ,key
     '(menu-item "python-nav" nil
		 :filter (lambda (&optional _)
			   ,conditional))))


(vipyr-conditional-key
 "q" "Quit vipyr-mode."
 (lambda ()
   (interactive)
   (vipyr-mode -1)))

(vipyr-conditional-key
 "?"
 "Display help."
 (lambda ()
   (interactive)
   (with-help-window
       (help-buffer)
     (cl-loop for (key . docstring) in vipyr-hotkeys
	      do
	      (princ (format "%s %s\n" key docstring))))))

;;* Navigation
(vipyr-conditional-key
 "["
 "Backward statement."
 (lambda ()
   (interactive)
   (python-nav-backward-statement)))

(vipyr-conditional-key
 "{"
 "Backward block."
 (lambda ()
   (interactive)
   (python-nav-backward-block)))

(vipyr-conditional-key
 "]"
 "Forward statement."
 (lambda ()
   (interactive)
   (python-nav-forward-statement)))

(vipyr-conditional-key
 "}"
 "Forward block."
 (lambda ()
   (interactive)
   (python-nav-forward-block)))

(vipyr-conditional-key
 "."
 "Goto symbol definition at point."
 (lambda ()
   (interactive)
   (elpy-goto-definition)))

(vipyr-conditional-key
 "," "Go back to symbol."
 (lambda ()
   (interactive)
   (xref-pop-marker-stack)))

(vipyr-conditional-key
 "d" "Get documentation for object at point."
 (lambda ()
   (interactive)
   (elpy-doc)))

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
 #'avy-goto-line)

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
 "D"
 "Jump to def/block with avy."
 (cond
  ((looking-at python-nav-beginning-of-defun-regexp)
   #'avy-python-goto-def)
  ((or
    (python-info-beginning-of-block-p)
    (and (not (python-info-current-line-empty-p))
	 (python-info-beginning-of-statement-p)))
   #'avy-python-goto-block)))

;; * Jump to a char or 2 char or char-inline
(vipyr-conditional-key
 "1"
 "Jump to visible char."
 #'avy-goto-char)

(vipyr-conditional-key
 "2"
 "Jump to pair of chars."
 #'avy-goto-char-2)

(vipyr-conditional-key
 "6"
 "Jump to char in line."
 #'avy-goto-char-in-line)


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
  (lambda ()
    (interactive)
    (helm :sources '(vipyr-helm-def)))))


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
 "#"
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
 "3"
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

;;* Minor mode
(defvar vipyr-original-cursor-color "#0FB300"
  "Original cursor color. For resetting.")


(define-minor-mode vipyr-mode
  "Speed of thought Python navigation mode."
  :lighter " Vipyr"
  :keymap vipyr-mode-map
  (if vipyr-mode
      (set-cursor-color "DarkOrange")
    (set-cursor-color vipyr-original-cursor-color)))

;;* Python at the speed of thought
;; adapted from https://github.com/Malabarba/speed-of-thought-lisp/blob/master/sotlisp.el
;; and thoughts from
;; http://nakkaya.com/2009/12/11/type-less-to-type-more/
;; https://www.emacswiki.org/emacs/SkeletonMode

;; Notes: _ is where the cursor will end up. @ are markers to jump to with C-n. To get a literal _, you need to escape it with \\_.

(defun vipyr-escape (s)
  "Escape _ and @ in s."
  (setq s (replace-regexp-in-string "\\\\_" "XXXYYY" s))
  (setq s (replace-regexp-in-string "\\\\@" "AAABBB" s))
  (setq s (replace-regexp-in-string "\\\\>" "CCCDDD" s))
  s)


(defun vipyr-unescape (s)
  "Convert escaped _ and @ back to their original values."
  (setq s (replace-regexp-in-string "XXXYYY" "_" s))
  (setq s (replace-regexp-in-string "AAABBB" "@" s))
  (setq s (replace-regexp-in-string "CCCDDD" ">" s))
  s)


(defun vipyr-parse-template (s)
  "Parse a skeleton string to skeleton lists.
This does not support the full syntax.
_
@
>
"
  (let ((i 0)
	(parsed '()))
    (if (string-match "_\\|@\\|>" s)
	(progn
	  (setq s (vipyr-escape s))
	  (while (string-match "_\\|@\\|>" s i)
	    (push (vipyr-unescape (substring s i (match-beginning 0)))
		  parsed)
	    (push (intern (match-string 0 s)) parsed)
	    (setq i (match-end 0)))
	  (push (vipyr-unescape (substring s (match-end 0)))
		parsed))
      (setq parsed (list s)))
    (reverse parsed)))

(defconst vipyr--default-abbrevs
  '(("d" . "def _(@):\n    @")
    ("c" . "class _(object):\n    @def \\_\\_init\\_\\_(self,@):\n    ")
    ("str" . "def \\_\\_str\\_\\_(self):\n    @")
    ("pr" . "print(_)@")
    ("lf" . "[for _ in @]")
    ("lfi" . "[for _ in @ if @]")
    ("for" . "for _ in @:\n    @")

    ;; numpy
    ("inp" . "import numpy as np\n")
    ("ila" . "import np.linalg as la")
    ("npa" . "np.array(_)")
    ("nls" . "np.linspace(_)")
    ("npf" . "np.polyfit(_, @)")
    ("npv" . "np.polyval(_)")
    ("npd" . "np.polyder(_)")
    ("nslv" . "np.linalg.solve(_, @)")

    ;; matplotlib
    ("imp" . "import matplotlib.pyplot as plt\n")
    ("plt" . "plt.plot(_)")
    ("pxl" . "plt.xlabel('_')")
    ("pyl" . "plt.ylabel('_')")
    ("pl" . "plt.legend(_)")
    ("psf" . "plt.savefig('_')")
    ("plf" . "plt.figure(_)@")

    ;; jasp
    ("ij" . "from jasp import *\n")
    ("wj" . "with jasp('_',) as calc:\n    @")
    ("cc" . "calc.calculate(atoms)")
    ("cga" . "calc.get_atoms()")

    ;; ase
    ("ias" . "from ase import Atom, Atoms\n")
    ("iav" . "from ase.visualize import view\n")
    ("iaw" . "from ase.io import write\n")
    ("atm" . "Atom('_', ['@', [@]]),")
    ("ats" . "Atoms([_], cell=[@])")
    ("ape" . "atoms.get_potential\\_energy()")
    ("afg" . "atoms.get\\_forces()")
    ("av" . "view(atoms)")
    ("aw" . "write('_.png', atoms, show\\_unit\\_cell=2)")

    ;; scipy
    ("isod" . "from scipy.integrate import odeint\n")
    ("isfs" . "from scipy.optimize import fsolve\n")
    ("isq" . "from scipy.integrate import quad\n")
    ("ode" . "odeint(_, @, @)@")
    ("fsol" . "fsolve(_, @)@")
    ("odel" . "X, F, TE, YE, IE = odelay(_, @, @, events=[@])@"))
  "Alist of (ABBREV . EXPANSION) used by `vipyr'.")

;; Here I want to define the skeletons, and load the abbrevs. You have to be a
;; bit careful not to define skeletons that have the same name as emacs
;; commands, as they can change the way emacs behaves badly. We avoid that here
;; with a message alerting you if something would conflict.
(defun vipyr-define-abbrev (name expansion)
  (if (fboundp (intern name))
      (message "%s already a function. Skipping (%s . %s)" name name expansion)
    (eval `(define-skeleton ,(intern name) ,name nil ,@(vipyr-parse-template expansion)))
    (define-abbrev python-mode-abbrev-table name "" (intern name))))

(defun vipyr-load-abbrevs ()
  (mapc (lambda (x)
	  (vipyr-define-abbrev (car x) (cdr x)))
	vipyr--default-abbrevs))

(vipyr-load-abbrevs)

;; this is kind of slow, for everytime you open a python file.
;; (add-hook 'python-mode-hook 'vipyr-load-abbrevs)


;;** skeleton markers
(defvar *skeleton-markers* nil
  "Markers for locations saved in skeleton-positions")

(defvar *skeleton-min-position* nil
  "Smallest position")

(defvar *skeleton-max-position* nil
  "Largest position")

(defun skeleton-make-markers ()
  (while *skeleton-markers*
    (set-marker (pop *skeleton-markers*) nil))
  (setq *skeleton-markers*
	(mapcar 'copy-marker (reverse skeleton-positions)))

  (when *skeleton-markers*
    (setq *skeleton-max-position* `(max ,@*skeleton-markers*)
	  *skeleton-min-position* `(min ,@*skeleton-markers*))))


(add-hook 'skeleton-end-hook 'skeleton-make-markers)


(defun skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
         REVERSE - Jump to previous position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position *skeleton-markers*))
	 (positions (if reverse (reverse positions) positions))
	 (comp (if reverse '> '<))
	 pos)
    (when positions
      (if (catch 'break
	    (while (setq pos (pop positions))
	      (when (funcall comp (point) pos)
		(throw 'break t))))
	  (goto-char pos)
	(goto-char (marker-position
		    (car *skeleton-markers*)))))))

;; Ideally, we would get rid of the markers while visiting them. That doesn't
;; seem easy though, and there is no way to check if we are in the skeleton
;; region.
(defun vipyr-point-in-skeleton ()
  (and (> (point) *skeleton-min-position*)
       (< (point) *skeleton-max-position*)))

(vipyr-conditional-key
 (kbd "s-n")
 "Next skeleton marker."
 (when (vipyr-point-in-skeleton)
   (lambda ()
     (interactive)
     (skeleton-next-position))))


(vipyr-conditional-key
 (kbd "s-p")
 "Previous skeleton marker"
 (when (vipyr-point-in-skeleton)
   (lambda ()
     (interactive)
     (skeleton-next-position '(4)))))


(provide 'vipyr)
;;; vipyr.el ends here
