;;; jmax-org.el --- jmax customization for org-mode

;;; Commentary:
;;
(require 'org)
(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-org)
(require 'ox-ascii)
; (require 'ox-odt)
(require 'org-inlinetask)
(require 'org-mouse)
(require 'org-contacts)
(require 'emacs-keybinding-command-tooltip-mode)

;;; Code:
;;* Basic variables
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb) ; convenient switching between open org-buffers

(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition

(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

;; I like to press enter to follow a link. mouse clicks also work.
(setq org-return-follows-link t)

;; Setup the frame configuration for following links.
(setq org-link-frame-setup (quote ((gnus . org-gnus-no-new-news)
                                   (file . find-file))))

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

;; use this code in emacs-lisp for folding code.
(global-set-key (kbd "C-M-]") (lambda () (interactive) (org-cycle t)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (ignore-errors
                                (end-of-defun)
                                (beginning-of-defun))
                              (org-cycle)))

;; use ido completion wherever possible
(setq org-completion-use-ido t)

;; I do not like this mode
(auto-fill-mode -1)

;; turn off auto-fill in org-mode. It is not enough to turn it off
;; everywhere.
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; allow lists with letters in them.
(setq org-list-allow-alphabetical t)

;; capture key binding
(define-key global-map "\C-cc" 'org-capture)

;; setup archive location in archive directory in current folder
(setq org-archive-location "archive/%s_archive::")

;;* Org-id

(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-link-search-must-match-exact-headline 'query-to-create)
(setq org-id-locations-file
      (expand-file-name "user/.org-id-locations" starter-kit-dir))
(require 'org-id)

;;* Speed commands
;; activate single letter commands at beginning of a headline.
;; User-defined Speed commands
;; ===========================

;; Built-in Speed commands
;; =======================

;; Outline Navigation
;; ------------------
;; n   (org-speed-move-safe (quote outline-next-visible-heading))
;; p   (org-speed-move-safe (quote outline-previous-visible-heading))
;; f   (org-speed-move-safe (quote org-forward-heading-same-level))
;; b   (org-speed-move-safe (quote org-backward-heading-same-level))
;; F   org-next-block
;; B   org-previous-block
;; u   (org-speed-move-safe (quote outline-up-heading))
;; j   org-goto
;; g   (org-refile t)

;; Outline Visibility
;; ------------------
;; c   org-cycle
;; C   org-shifttab
;;     org-display-outline-path
;; s   org-narrow-to-subtree
;; =   org-columns

;; Outline Structure Editing
;; -------------------------
;; U   org-shiftmetaup
;; D   org-shiftmetadown
;; r   org-metaright
;; l   org-metaleft
;; R   org-shiftmetaright
;; L   org-shiftmetaleft
;; i   (progn (forward-char 1) (call-interactively (quote org-insert-heading-respect-content)))
;; ^   org-sort
;; w   org-refile
;; a   org-archive-subtree-default-with-confirmation
;; @   org-mark-subtree
;; #   org-toggle-comment

;; Clock Commands
;; --------------
;; I   org-clock-in
;; O   org-clock-out

;; Meta Data Editing
;; -----------------
;; t   org-todo
;; ,   (org-priority)
;; 0   (org-priority 32)
;; 1   (org-priority 65)
;; 2   (org-priority 66)
;; 3   (org-priority 67)
;; :   org-set-tags-command
;; e   org-set-effort
;; E   org-inc-effort
;; W   (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m))

;; Agenda Views etc
;; ----------------
;; v   org-agenda
;; /   org-sparse-tree

;; Misc
;; ----
;; o   org-open-at-point
;; ?   org-speed-command-help
;; <   (org-agenda-set-restriction-lock (quote subtree))
;; >   (org-agenda-remove-restriction-lock)

;;* New speed bindings

(defun org-teleport (&optional arg)
  "Teleport the current heading to after a headline selected with avy.
With a prefix ARG move the headline to before the selected
headline. With a numeric prefix, set the headline level. If ARG
is positive, move after, and if negative, move before."
  (interactive "P")
  ;; Kill current headline
  (org-mark-subtree)
  (kill-region (region-beginning) (region-end))
  ;; Jump to a visible headline
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style))
  (cond
   ;; Move before  and change headline level
   ((and (numberp arg) (> 0 arg))
    (save-excursion
      (yank))
    ;; arg is what we want, second is what we have
    ;; if n is positive, we need to demote (increase level)
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
	       do
	       (if (> 0 n)
		   (org-promote-subtree)
		 (org-demote-subtree)))))
   ;; Move after and change level
   ((and (numberp arg) (< 0 arg))
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))
    ;; n is what we want and second is what we have
    ;; if n is positive, we need to demote
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
	       do
	       (if (> 0 n) (org-promote-subtree)
		 (org-demote-subtree)))))

   ;; move to before selection
   ((equal arg '(4))
    (save-excursion
      (yank)))
   ;; move to after selection
   (t
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))))
  (outline-hide-leaves))

(add-to-list 'org-speed-commands-user (cons "m" 'org-mark-subtree))
(add-to-list 'org-speed-commands-user (cons "S" 'widen))
(add-to-list 'org-speed-commands-user (cons "k" (lambda ()
						  (org-mark-subtree)
						  (kill-region
						   (region-beginning)
						   (region-end)))))
(add-to-list 'org-speed-commands-user
	     (cons "q" (lambda ()
			 (avy-with avy-goto-line
			   (avy--generic-jump "^\\*+" nil avy-style)))))

(add-to-list 'org-speed-commands-user (cons "T" 'org-teleport))

(setq org-use-speed-commands t)

;;* Misc org settings

;; renumber footnotes when new ones are inserted
(setq org-footnote-auto-adjust t)

;; default with images open
(setq org-startup-with-inline-images "inlineimages")

;; clocking setup http://www.gnu.org/software/emacs/manual/html_node/org/Clocking-work-time.html
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; store clock entries in a drawer
(setq org-clock-into-drawer t)

;;* Expansions for blocks
(require 'ob-ipython)
(add-to-list 'org-structure-template-alist
             '("ip" "#+BEGIN_SRC ipython :session\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <p for python expansion
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC python\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <por for python expansion with raw output
(add-to-list 'org-structure-template-alist
             '("por" "#+BEGIN_SRC python :results output raw\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <pv for python expansion with value
(add-to-list 'org-structure-template-alist
             '("pv" "#+BEGIN_SRC python :results value\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n?\n</src>"))

;; add <sh for shell
(add-to-list 'org-structure-template-alist
             '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC" "<src lang=\"shell\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
             '("lh" "#+latex_header: " ""))

(add-to-list 'org-structure-template-alist
             '("lc" "#+latex_class: " ""))

(add-to-list 'org-structure-template-alist
             '("lco" "#+latex_class_options: " ""))

(add-to-list 'org-structure-template-alist
             '("ao" "#+attr_org: " ""))

(add-to-list 'org-structure-template-alist
             '("al" "#+attr_latex: " ""))

(add-to-list 'org-structure-template-alist
             '("ca" "#+caption: " ""))

(add-to-list 'org-structure-template-alist
             '("tn" "#+tblname: " ""))

(add-to-list 'org-structure-template-alist
             '("n" "#+name: " ""))

;; table expansions
(loop for i from 1 to 6
      do
      (let ((template (make-string i ?t))
	    (expansion (concat "|"
			       (mapconcat
				'identity
				(loop for j to i collect "   ")
				"|"))))
	(setf (substring expansion 2 3) "?")
	(add-to-list 'org-structure-template-alist
		     `(,template ,expansion ""))))


;;* Babel settings
;; do not evaluate code on export by default
(setq org-export-babel-evaluate nil)

;; enable prompt-free code running
(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil)

;; register languages in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sh . t)
   (matlab . t)
   (sqlite . t)
   (ruby . t)
   (perl . t)
   (org . t)
   (dot . t)
   (plantuml . t)
   (R . t)
   (C . t)))

;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)

;; language specific headers. I think this comes before the defaults
;; for emacs-lisp I want results to be value
(setq org-babel-default-header-args:emacs-lisp
      (cons '(:results . "value replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; for everything else set default :results to output
(setq org-babel-default-header-args
      (cons '(:results . "output replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
	    (assq-delete-all :exports org-babel-default-header-args)))

;; Interpret "_" and "^" for export when braces are used.
(setq org-export-with-sub-superscripts '{})


;;* Agenda setup
; I don't want to see things that are done. turn that off here.
; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-timestamp t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-start-on-weekday nil) ;; start on current day

(setq org-upcoming-deadline '(:foreground "blue" :weight bold))

;; give me some warning of upcoming deadlines
(setq org-deadline-warning-days 0)

;; record time I finished a task when I change it to DONE
(setq org-log-done 'time)

(add-to-list
 'org-agenda-custom-commands
 '("w" "Weekly Review"
   ( ;; deadlines
    (tags-todo "+DEADLINE<=\"<today>\""
	       ((org-agenda-overriding-header "Late Deadlines")))
    ;; scheduled  past due
    (tags-todo "+SCHEDULED<=\"<today>\""
	       ((org-agenda-overriding-header "Late Scheduled")))

    ;; now the agenda
    (agenda ""
	    ((org-agenda-overriding-header "weekly agenda")
	     (org-agenda-ndays 7)
	     (org-agenda-tags-todo-honor-ignore-options t)
	     (org-agenda-todo-ignore-scheduled nil)
	     (org-agenda-todo-ignore-deadlines nil)
	     (org-deadline-warning-days 0)))
    ;; and last a global todo list
    (todo "TODO"))))

;;* New org-links
;; support for links to microsoft docx,pptx,xlsx files
;; standard org-mode opens these as zip-files
;;  http://orgmode.org/manual/Adding-hyperlink-types.html
(defun org-msx-open (path)
       "Visit the msx file on PATH.

uses the dos command:
start  empty title path"
       (cond
	((string= "windows-nt" system-type)
	 (shell-command
	  (concat "start \"title\" " (shell-quote-argument path)) t))
	((string= "darwin" system-type)
	 (shell-command
	  (concat "open " (shell-quote-argument path)) t))))

(org-add-link-type
 "msx"
 'org-msx-open)

(org-add-link-type
 "attachfile"
 (lambda (link-string) (org-open-file link-string))
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "")); no output for html
    ((eq format 'latex)
     ;; write out the latex command
     (format "\\attachfile{%s}" keyword)))))



;;* Export settings
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
	("" "lmodern" nil)
	("T1" "fontenc" t)
	("" "fixltx2e" nil)
;	("" "charter" nil) ;; a decent font
;	("expert" "mathdesign" nil)
	("" "graphicx" t)
	("" "longtable" nil)
	("" "float" nil)
	("" "wrapfig" nil)
	("" "rotating" nil)
	("normalem" "ulem" t)
	("" "amsmath" t)
	("" "textcomp" t)
	("" "marvosym" t)
	("" "wasysym" t)
	("" "amssymb" t)
	("" "amsmath" t)
	("version=3" "mhchem" t)
	("numbers,super,sort&compress" "natbib" nil)
	("" "natmove" nil)
	("" "url" nil)
	("" "minted" nil)
	("" "underscore" nil)
	("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	 "hyperref" nil)
	("" "attachfile" nil)))

;; do not put in \hypersetup use your own
;; \hypersetup{pdfkeywords={%s},\n pdfsubject={%s},\n pdfcreator={%s}
(setq org-latex-with-hyperref nil)

;; this is for code syntax highlighting in export
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
;;         "bibtex %b"
;;         "makeindex %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))

;; I have not had good luck with this on windows
;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch"))

;; avoid getting \maketitle right after begin{document}
;; you should put \maketitle if and where you want it.
(setq org-latex-title-command "")

;; customized article. better margins
(add-to-list 'org-latex-classes
	     '("cmu-article"                          ;class-name
	       "\\documentclass{article}
\\usepackage[top=1in, bottom=1.in, left=1in, right=1in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     '("article-no-defaults"                          ;class-name
	       "\\documentclass{article}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(require 'ox-cmu-qualifier)
(require 'ox-cmu-ms-report)
(require 'ox-cmu-dissertation)
(require 'ox-manuscript)
(require 'ox-archive)

;;* org-require
;; org path for loadable org-files
;; (defvar org-load-path
;;   (list (file-name-as-directory
;;	 (expand-file-name "org" starter-kit-dir)))
;;   "List of directories to find org-files that `org-babel-load-file' can load code from.")

;; (defun org-require (feature)
;;   "Load a FEATURE from an org-file.
;; FEATURE is a symbol, and it is loaded from an org-file by the name of FEATURE.org, that is in the `org-load-path'.  The FEATURE is loaded from `org-babel-load-file'."
;;   (let ((org-file (concat (symbol-name feature) ".org"))
;;	(path))

;;     ;; find the org-file
;;     (catch 'result
;;       (loop for dir in org-load-path do
;;	    (when (file-exists-p
;;		   (setq path
;;			 (expand-file-name
;;			  org-file
;;			  dir)))
;;	      (throw 'result path))))
;;     (let ((default-directory (file-name-directory path)))
;;       (org-babel-load-file path))))


(add-to-list 'load-path
	     (expand-file-name "org" starter-kit-dir))

(require 'org-show)


;;* cm-mode
(add-to-list 'load-path
	     (expand-file-name "criticmarkup-emacs" starter-kit-dir))

(require 'cm-mode)
(require 'cm-mods)


;;* https://github.com/jkitchin/org-ref
(add-to-list 'load-path
	     (expand-file-name "org-ref" starter-kit-dir))

(require 'org-ref)
(require 'doi-utils)
(require 'org-ref-isbn)
(require 'org-ref-pubmed)
(require 'org-ref-arxiv)
(require 'org-ref-bibtex)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)

;; variables that control bibtex key format for auto-generation
;; I want firstauthor-year-title-words
;; this usually makes a legitimate filename to store pdfs under.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;;* Images in org-mode - default width
(setq org-image-actual-width '(600))

;; refresh images after running a block
(defun org-refresh-images ()
  "Refreshes images displayed inline."
  (interactive)
  (org-remove-inline-images)
  (org-display-inline-images))

(add-hook 'org-babel-after-execute-hook
	  (lambda () (org-refresh-images)))


;;* font-lock on LaTeX fragments
(add-hook 'org-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     `((,(mapconcat (lambda (x)
			      (nth 1 x))
			    org-latex-regexps
			    "\\|") 0 '(:foreground "blue"))
	       ("@@latex:[^@]*@@" 0 '(:foreground "blue"))))))

;;* Insert org figures and tables
(defun jmax-insert-figure (fname width params)
  "Insert FNAME as a figure in org-mode.
WIDTH specifies how wide it should be, e.g. 300, 3.25in, or 4cm.
PARAMS is a string like \":placement [H]\"."
  (interactive (list
		(ido-read-file-name "File: ")
		(read-input "Org width: " "200")
		(read-input "Parameters: ")))

  ;; parse width
  (let ((dpi 72)
	org-width latex-width)
    ;; calculate width
    (cond
     ((string= "" width)
      (setq width nil
	    org-width nil))
     ((string-match "\\([0-9]\\.?[0-9]*\\)in" width)
      (setq org-width (* dpi (string-to-number (string-match 1 width)))
	    latex-width width))
     ((string-match "\\([0-9]\\.?[0-9]*\\)cm" width)
      (setq org-width (* dpi (string-to-number (string-match 1 width)) 2.54)
	    latex-width width))
     ((string-match "\\([0-9]*\\)" width)
      (setq org-width (string-to-number width)
	    latex-width (/ org-width dpi))))
    (when width
      (insert (format "#+attr_latex: :width %sin %s\n" latex-width params))
      (insert (format "#+attr_org: :width %s\n" org-width)))

    (insert "#+caption: \n")
    (insert (format "[[./%s]]\n" fname))
    (forward-line -2)
    (end-of-line)
    (org-redisplay-inline-images)))


;; make it possible to insert from helm.
(defun helm-insert-org-figure (target)
  (jmax-insert-figure
   (file-relative-name target)
   (read-input "Width: ")
   (read-input "Parameters: ")))


(add-hook 'helm-find-files-before-init-hook
          (lambda ()
	    (helm-add-action-to-source
	     "Insert as org-mode figure"
	     'helm-insert-org-figure
	     helm-source-find-files)))


(defun jmax-insert-table (ncols tblname attributes)
  "Insert a table with NCOLS and named TBLNAME.
If you enter ATTRIBUTES they are inserted as LaTeX attributes."
  (interactive "nColumns: \nsName: \nsAttributes: ")
  (when (not (string= "" tblname))
    (insert (format "#+tblname: %s\n" tblname)))
  (when (not (string= "" attributes))
    (insert (format "#+attr_latex: %s\n" attributes)))
  (insert "#+caption: \n")
  (loop
   initially (insert "|")
   repeat ncols do (insert "  |" )
   finally (insert "\n"))
  (forward-line -2)
  (end-of-line))

;;* Adapting C-c C-c for Latex overlays
;; Define a new toggling function for equations.
(defun org-toggle-latex-overlays (arg)
  "Toggle LaTeX fragments.  The prefix ARG is passed to `org-preview-latex-fragment'."
  (interactive "P")
  (if org-latex-fragment-image-overlays
      (org-remove-latex-fragment-image-overlays)
    (org-preview-latex-fragment arg)))

(org-defkey org-mode-map "\C-c\C-x\C-l"    'org-toggle-latex-overlays)


(defadvice org-ctrl-c-ctrl-c (around latex-overlays nil activate)
  "Ignore latex overlays in C-cC-c."
  (let ((org-latex-fragment-image-overlays nil))
    ad-do-it))


(require 'ore)

;;* Python sessions

;; define a better org-mode tab
(define-key org-mode-map (kbd "<tab>")
  '(menu-item "org-mode-tab" nil
	      :filter (lambda (&optional _)
			(when
			    (and
			     (org-in-src-block-p t)
			     (string= "python"
				      (org-element-property
				       :language
				       (org-element-context))))
			  (org-babel-do-key-sequence-in-edit-buffer
			   (kbd "<tab>"))))))

;; Make shift-tab remove spaces backwards. Jake wanted this.
(define-key org-mode-map (kbd "S-<tab>")
  '(menu-item "org-mode-tab" nil
	      :filter (lambda (&optional _)
			(when
			    (and
			     (org-in-src-block-p t)
			     (string= "python"
				      (org-element-property
				       :language
				       (org-element-context))))
			  (let ((p (point)))
			    (skip-chars-backward " ")
			    (setf (buffer-substring p (point)) ""))))))


(defun org-babel-python-strip-session-chars ()
  "Remove >>> and ... from a Python session output."
  (when (and (org-element-property :parameters (org-element-at-point))
	     (string=
	      "python"
	      (org-element-property :language (org-element-at-point)))
	     (string-match
	      ":session"
	      (org-element-property :parameters (org-element-at-point))))

    (save-excursion
      (when (org-babel-where-is-src-block-result)
	(goto-char (org-babel-where-is-src-block-result))
	(end-of-line 1)
					;(while (looking-at "[\n\r\t\f ]") (forward-char 1))
	(while (re-search-forward
		"\\(>>> \\|\\.\\.\\. \\|: $\\|: >>>$\\)"
		(org-element-property :end (org-element-at-point))
		t)
	  (replace-match "")
	  ;; this enables us to get rid of blank lines and blank : >>>
	  (beginning-of-line)
	  (when (looking-at "^$")
	    (kill-line)))))))

(add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars)

;;** Asynchronous Python
(defun org-babel-async-execute:python ()
  "Execute the python src-block at point asynchronously.
:var headers are supported.
:results output is all that is supported for output.

A new window will pop up showing you the output as it appears,
and the output in that window will be put in the RESULTS section
of the code block."
  (interactive)
  (let* ((current-file (buffer-file-name))
	 (uuid (org-id-uuid))
	 (code (org-element-property :value (org-element-context)))
	 (temporary-file-directory ".")
	 (tempfile (make-temp-file "py-"))
	 (pbuffer (format "*%s*" uuid))
	 (varcmds (org-babel-variable-assignments:python
		   (nth 2 (org-babel-get-src-block-info))))
	 process)

    ;; get rid of old results, and put a place-holder for the new results to
    ;; come.
    (org-babel-remove-result)

    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (insert (format
	       "\n\n#+RESULTS: %s\n: %s"
	       (or (org-element-property :name (org-element-context))
		   "")
	       uuid)))

    ;; open the results buffer to see the results in.
    (switch-to-buffer-other-window pbuffer)

    ;; Create temp file containing the code.
    (with-temp-file tempfile
      ;; if there are :var headers insert them.
      (dolist (cmd varcmds)
	(insert cmd)
	(insert "\n"))
      (insert code))

    ;; run the code
    (setq process (start-process
		   uuid
		   pbuffer
		   "python"
		   tempfile))
    
    ;; when the process is done, run this code to put the results in the
    ;; org-mode buffer.
    (set-process-sentinel
     process
     `(lambda (process event)
	(delete-file ,tempfile)
	(unwind-protect
	    (save-window-excursion
	      (save-excursion
		(save-restriction
		  (with-current-buffer (find-file-noselect ,current-file)
		    (goto-char (point-min))
		    (when (re-search-forward ,uuid nil t)
		      (beginning-of-line)
		      (kill-line)
		      (when (with-current-buffer
				,pbuffer
			      (buffer-string)))
		      (insert
		       (mapconcat
			(lambda (x)
			  (format ": %s" x))
			(butlast (split-string
				  (with-current-buffer
				      ,pbuffer
				    (buffer-string))
				  "\n"))
			"\n")))))))
	  ;; delete the results buffer then delete the tempfile.
	  ;; finally, delete the process.
	  (when (get-buffer ,pbuffer)
	    (kill-buffer ,pbuffer)
	    (delete-window)) 
	  (delete-process process))))))


(defun org-babel-kill-async ()
  "Kill the current async process.
Run this in the code block that is running."
  (interactive)
  (goto-char (org-babel-where-is-src-block-result))
  (forward-line)
  (forward-char)
  (interrupt-process
   (s-trim (buffer-substring (point) (line-end-position)))))


;;** Asynchronous shell commands
;; (defun org-babel-async-execute:sh ()
;;   "Execute the shell src-block at point asynchronously.
;; :var headers are supported.
;; :results output is all that is supported for output.

;; A new window will pop up showing you the output as it appears,
;; and the output in that window will be put in the RESULTS section
;; of the code block."
;;   (interactive)
;;   (let* ((current-file (buffer-file-name))
;;	 (uuid (org-id-uuid))
;;	 (code (org-element-property :value (org-element-context)))
;;	 (temporary-file-directory ".")
;;	 (tempfile (make-temp-file "sh-"))
;;	 (pbuffer (format "*%s*" uuid))
;;	 (varcmds (org-babel-variable-assignments:sh
;;		   (nth 2 (org-babel-get-src-block-info))))
;;	 process)

;;     ;; get rid of old results, and put a place-holder for the new results to
;;     ;; come.
;;     (org-babel-remove-result)

;;     (save-excursion
;;       (re-search-forward "#\\+END_SRC")
;;       (insert (format
;;	       "\n\n#+RESULTS: %s\n: %s"
;;	       (or (org-element-property :name (org-element-context))
;;		   "")
;;	       uuid)))

;;     ;; open the results buffer to see the results in.
;;     (switch-to-buffer-other-window pbuffer)

;;     ;; Create temp file containing the code.
;;     (with-temp-file tempfile
;;       ;; if there are :var headers insert them.
;;       (dolist (cmd varcmds)
;;	(insert cmd)
;;	(insert "\n"))
;;       (insert code))

;;     ;; run the code
;;     (setq process (start-process
;;		   uuid
;;		   pbuffer
;;		   "bash"
;;		   tempfile))

;;     ;; when the process is done, run this code to put the results in the
;;     ;; org-mode buffer.
;;     (set-process-sentinel
;;      process
;;      `(lambda (process event)
;;	(save-window-excursion
;;	  (save-excursion
;;	    (save-restriction
;;	      (with-current-buffer (find-file-noselect ,current-file)
;;		(goto-char (point-min))
;;		(re-search-forward ,uuid)
;;		(beginning-of-line)
;;		(kill-line)
;;		(insert
;;		 (mapconcat
;;		  (lambda (x)
;;		    (format ": %s" x))
;;		  (butlast (split-string
;;			    (with-current-buffer
;;				,pbuffer
;;			      (buffer-string))
;;			    "\n"))
;;		  "\n"))))))
;;	;; delete the results buffer then delete the tempfile.
;;	;; finally, delete the process.
;;	(when (get-buffer ,pbuffer)
;;	  (kill-buffer ,pbuffer)
;;	  (delete-window))
;;	(delete-file ,tempfile)
;;	(delete-process process)))))

;; (defun org-babel-execute:sh (body params)
;;   "Execute a block of Shell commands with Babel.
;; Adds :async to the header vars
;; This function is called by `org-babel-execute-src-block'."
;;   (let* ((session (org-babel-sh-initiate-session
;;		   (cdr (assoc :session params))))
;;	 (async (assoc :async params))
;;	 (stdin (let ((stdin (cdr (assoc :stdin params))))
;;                   (when stdin (org-babel-sh-var-to-string
;;                                (org-babel-ref-resolve stdin)))))
;;          (full-body (org-babel-expand-body:generic
;;		     body params (org-babel-variable-assignments:sh params))))
;;     (if async
;;	;; run asynchronously
;;	(org-babel-async-execute:sh)
;;       ;; else run regularly
;;       (org-babel-reassemble-table
;;        (org-babel-sh-evaluate session full-body params stdin)
;;        (org-babel-pick-name
;;	(cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
;;        (org-babel-pick-name
;;	(cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

;;** Restarting an org-babel session

(defun src-block-in-session-p (&optional name)
  "Return if src-block is in a session of NAME.
NAME may be nil for unnamed sessions."
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))

    (cond
     ;; unnamed session, both name and session are nil
     ((and (null session)
	   (null name))
      t)
     ;; Matching name and session
     ((and
       (stringp name)
       (stringp session)
       (string= name session))
      t)
     ;; no match
     (t nil))))

(defun org-babel-restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (org-babel-kill-session)
  (let* ((current-point (point-marker))
	 (info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
	;; goto start of block
        (goto-char (match-beginning 0))
	(let* ((this-info (org-babel-get-src-block-info))
	       (this-lang (nth 0 this-info))
	       (this-params (nth 2 this-info))
	       (this-session (cdr (assoc :session this-params))))
	    (when
		(and
		 (< (point) (marker-position current-point))
		 (string= lang this-lang)
		 (src-block-in-session-p session))
	      (org-babel-execute-src-block arg)))
	;; move forward so we can find the next block
	(forward-line)))))


(defun org-babel-kill-session ()
  "Kill session for current code block."
  (interactive)
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (save-window-excursion
    (org-babel-switch-to-session)
    (kill-buffer)))


(defun org-babel-remove-result-buffer ()
  "Remove results from every code block in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (org-babel-remove-result))))


;;** Miscellaneous
(defun sa-ignore-headline (contents backend info)
  "Ignore headlines with tag `ignoreheading'.
This may mess up your labels, since the exporter still creates a label for it.
Argument CONTENTS Contents of the headline.
Argument BACKEND The backend exporter.
Argument INFO Parse-tree from org-mode."
  (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
                  (string-match "\\`.*ignoreheading.*\n"
                                (downcase contents)))
    (replace-match "" nil nil contents)))

(defun headline-nonumber (contents backend info)
  "Make headlines with nonumber."
  (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
                  (string-match "\\`.*nonumber.*\n"
                                (downcase contents)))
    (let ((output contents))
      (setq output (replace-regexp-in-string "section{" "section*{" contents))
      (setq output (replace-regexp-in-string "\\\\hfill{}\\\\textsc{nonumber}" "" output))
      output)))

(add-to-list 'org-export-filter-headline-functions 'sa-ignore-headline)
(add-to-list 'org-export-filter-headline-functions 'headline-nonumber)



(defun helm-insert-org-entity ()
  "Helm interface to insert an entity from `org-entities'.
F1 inserts utf-8 character
F2 inserts entity code
F3 inserts LaTeX code (does not wrap in math-mode)
F4 inserts HTML code
F5 inserts the entity code."
  (interactive)
  (helm :sources (reverse
		  (let ((sources '())
			toplevel
			secondlevel)
		    (dolist (element (append
				      '("* User" "** User entities")
				      org-entities-user org-entities))
		      (when (and (stringp element)
				 (s-starts-with? "* " element))
			(setq toplevel element))
		      (when (and (stringp element)
				 (s-starts-with? "** " element))
			(setq secondlevel element)
			(add-to-list
			 'sources
			 `((name . ,(concat
				     toplevel
				     (replace-regexp-in-string
				      "\\*\\*" " - " secondlevel)))
			   (candidates . nil)
			   (action . (("insert utf-8 char" . (lambda (x)
							       (mapc (lambda (candidate)
								       (insert (nth 6 candidate)))
								     (helm-marked-candidates))))
				      ("insert org entity" . (lambda (x)
							       (mapc (lambda (candidate)
								       (insert (concat "\\" (car candidate))))
								     (helm-marked-candidates))))
				      ("insert latex" . (lambda (x)
							  (mapc (lambda (candidate)
								  (insert (nth 1 candidate)))
								(helm-marked-candidates))))
				      ("insert html" . (lambda (x)
							 (mapc (lambda (candidate)
								 (insert (nth 3 candidate)))
							       (helm-marked-candidates))))
				      ("insert code" . (lambda (x)
							 (mapc (lambda (candidate)
								 (insert (format "%S" candidate)))
							       (helm-marked-candidates)))))))))
		      (when (and element (listp element))
			(setf (cdr (assoc 'candidates (car sources)))
			      (append
			       (cdr (assoc 'candidates (car sources)))
			       (list (cons
				      (format "%10s %s" (nth 6 element) element)
				      element))))))
		    sources))))

(define-key org-mode-map (kbd "s-y") 'helm-insert-org-entity)

;; setup english dictionary and spell check on windows.
(cond
 ((string= system-type "windows-nt")
  (setq-default ispell-program-name
		(expand-file-name
		 "Aspell-win32/bin/aspell.exe" starter-kit-dir))
  (flyspell-mode +1)
  (add-hook 'org-mode-hook 'turn-on-flyspell 'append)))


;;* Table of contents commands
(defalias 'toc 'helm-org-in-buffer-headings)
(defalias 'atoc 'helm-org-agenda-files-headings)

(defun helm-org-open-files-headings ()
  "Preconfigured helm for org files headings in open org-files."
  (interactive)
  (helm :sources (helm-source-org-headings-for-files
		  (loop for buffer in (buffer-list)
			if (and (buffer-file-name buffer)
				(string= "org"
					 (file-name-extension (buffer-file-name buffer))))
			collect (buffer-file-name buffer)))
        :candidate-number-limit 99999
        :buffer "*helm org headings*"))

(defalias 'otoc 'helm-org-open-files-headings)


(defalias 'top 'helm-top)





;;* The end
(message "jmax-org.el loaded")

(provide 'jmax-org)

;;; jmax-org.el ends here
