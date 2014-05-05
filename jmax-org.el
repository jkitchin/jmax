
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  You should not need to modify paths below here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-odt)
(require 'org-inlinetask)
(require 'org-mouse)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb) ; convenient switching between open org-buffers

(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition

(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

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

(setq org-use-speed-commands t)

;; I like to press enter to follow a link. mouse clicks also work.
(setq org-return-follows-link t)

;; default with images open
(setq org-startup-with-inline-images "inlineimages")

;; clocking setup http://www.gnu.org/software/emacs/manual/html_node/org/Clocking-work-time.html
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; store clock entries in a drawer
(setq org-clock-into-drawer t)

;; add <p for python expansion
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC python\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <por for python expansion with raw output
(add-to-list 'org-structure-template-alist
             '("por" "#+BEGIN_SRC python :results output raw\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <pv for python expansion with raw output
(add-to-list 'org-structure-template-alist
             '("pv" "#+BEGIN_SRC python :results value\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n?\n</src>"))

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
   (org . t)
   (dot . t)
   (plantuml . t)
   (R . t)))

;; no extra indentation
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)
;; make code blocks stand out a little from my gray80 background
(set-face-attribute 'org-block-background nil :background "gray")

;; language specific headers. I think this comes before the defaults
(setq org-babel-default-header-args:emacs-lisp 
      (cons '(:results . "value replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; set default :results to output
(setq org-babel-default-header-args
      (cons '(:results . "output replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
	    (assq-delete-all :exports org-babel-default-header-args)))

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; I do not like this mode
(auto-fill-mode -1)

;; turn off auto-fill in org-mode. It is not enough to turn it off
;; everywhere.
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; allow lists with letters in them.
(setq org-list-allow-alphabetical t)

;;;;;; capture
(define-key global-map "\C-cc" 'org-capture)

;; setup archive location in archive directory in current folder
(setq org-archive-location "archive/%s_archive::")

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

(setq org-agenda-custom-commands
      '(("w" "Weekly Review"
          (
           ;; put a random entry at the top
           (get-random-bibtex-entry)

           ;; deadlines
          (tags-todo "+DEADLINE<=\"<today>\""
                     ((org-agenda-overriding-header "Late Deadlines")
                      ;(org-agenda-tags-todo-honor-ignore-options t)
                      ;(org-agenda-todo-ignore-scheduled t)
                      ;(org-agenda-todo-ignore-deadlines t)
		      ))

          ;; scheduled  past due
          (tags-todo "+SCHEDULED<=\"<today>\""
                     ((org-agenda-overriding-header "Late Scheduled")
                      ;(org-agenda-tags-todo-honor-ignore-options t)
                      ;(org-agenda-todo-ignore-scheduled t)
                      ;(org-agenda-todo-ignore-deadlines t)
		      ))

	  ;; now the agenda
	  (agenda ""
		  ((org-agenda-overriding-header "weekly agenda")
		   (org-agenda-ndays 7)
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled nil)
		   (org-agenda-todo-ignore-deadlines nil)
		   (org-deadline-warning-days 0)
		   ))
	  (todo "TASK")
	  ;; and last a global todo list
          (todo "TODO"))) ;; review waiting items ...other commands
			     ;; here
	("h" "Work todos" tags-todo
         "-personal-doat={.+}-dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled t)))
        ("H" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("A" "Work todos with doat or dowith" tags-todo
         "-personal+doat={.+}|dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("j" "TODO dowith and TASK with"
         ((org-sec-with-view "TODO dowith")
          (org-sec-where-view "TODO doat")
          (org-sec-assigned-with-view "TASK with")
          (org-sec-stuck-with-view "STUCK with")))
        ("J" "Interactive TODO dowith and TASK with"
         ((org-sec-who-view "TODO dowith")))))

;; record time I finished a task when I change it to DONE
(setq org-log-done 'time)

;; support for links to microsoft docx,pptx,xlsx files
;; standard org-mode opens these as zip-files
;;  http://orgmode.org/manual/Adding-hyperlink-types.html
(defun org-msx-open (path)
       "Visit the msx file on PATH.

uses the dos command:
start  empty title path
"
       (shell-command
	(concat "start \"title\" " (shell-quote-argument path)) t))

(org-add-link-type 
 "attachfile" 
 (lambda (link-string) (org-open-file link-string))
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "")); no output for html
    ((eq format 'latex)
     ;; write out the latex bibliography command
     (format "\\attachfile{%s}" keyword)))))

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
	("numbers,super,sort&compress" "natbib" t)
	("" "natmove" nil)
	("" "url" t)
	("" "minted" nil)
	("" "underscore" t)	
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

(defcustom jmax-org-interactive-build
  t
  "Determines if pdfs are built with interaction from the user. nil means just build without user interaction. Anything else will show the user a window of the results of each build step, and ask if you should continue to the next step.")

(defun jmax-org-toggle-interactive-build ()
 "toggle state of jmax-org-interactive-build"
  (interactive)
  (if jmax-org-interactive-build
      (setq jmax-org-interactive-build nil)
    (setq jmax-org-interactive-build t)))

(defun jmax-org-pdflatex (tex-file)
  "run pdflatex on tex-file. This function checks for the presence of minted, and uses -shell-escape if needed. You can run this interactively, and you will be prompted for a tex file name."
  (interactive "fTex file: ")
  (message "running pdflatex on %s" tex-file)

  ;; get rid of any existing buffer
  (when (get-buffer "*pdflatex*")
    (kill-buffer "*pdflatex*"))

  ;; run pdflatex
  (let ((tex-buffer (find-file-noselect tex-file)))
    (set-buffer tex-buffer)
    (if (re-search-forward "{minted}" nil t)
	(call-process "pdflatex" nil "*pdflatex*" t "-shell-escape" "-interaction" "nonstopmode" tex-file)
      (call-process "pdflatex" nil "*pdflatex*" t "-interaction" "nonstopmode" tex-file))
    (kill-buffer tex-buffer))

  (when jmax-org-interactive-build
    (switch-to-buffer-other-frame "*pdflatex*")
    (end-of-buffer)
    (let ((search-upper-case nil))
      (occur "warning\\|undefined\\|error\\|missing"))))

(defun jmax-org-bibtex (tex-file)
  "Run bibtex8 on the tex-file."
  (interactive "fTex file: ")
  (message "running bibtex on %s" tex-file)

  ;; get rid of any existing buffer
  (when (get-buffer "*bibtex*")
    (kill-buffer "*bibtex*"))

  (let ((basename (file-name-sans-extension tex-file)))
    (call-process "bibtex8" nil "*bibtex*" t basename))

  (when jmax-org-interactive-build
    (switch-to-buffer-other-frame "*bibtex*")
    (end-of-buffer)
    (let ((search-upper-case nil))
      (occur "warning\\|error\\|missing"))))

(defun jmax-org-latex-pdf-process (quoted-tex-file)
  "Build a tex-file to pdf. The argument is called quoted-tex-file because this seems to be what org-mode passes to this function. The function strips the quotes out. Depending on the value of `jmax-org-interactive-build', you will get buffers of the intermediate output steps."
  (interactive "fTex file: ")
  ;; it seems the filename passed to this function from org-mode has
  ;; "" in it. we remove them here.
  (let* ((tex-file (replace-regexp-in-string "\"" "" quoted-tex-file))	  
	 (basename (file-name-sans-extension tex-file))
	 (tex-buffer (find-file-noselect tex-file))
	 (status)
	 ;; determine if we should run bibtex if there is a bibliography line
	 (run-bibtex-p (progn 
			 (set-buffer tex-buffer)
			 (beginning-of-buffer)
			 (re-search-forward "\\\\bibliography{" nil t))))
			 
    (setq status (catch 'status
      ;; run first pdflatex
      (jmax-org-pdflatex tex-file)    
      (when jmax-org-interactive-build
	(if (y-or-n-p "Continue to bibtex?")
	    (progn (kill-buffer "*pdflatex*") 
		   (kill-buffer "*Occur*")
		   (delete-frame))
	  (throw 'status nil)))

      ;; run bibtex if needed
      (message "bibtex needed = %s" run-bibtex-p)
      (when run-bibtex-p
	(jmax-org-bibtex tex-file)
	(when jmax-org-interactive-build
	  (if (y-or-n-p "Continue to pdflatex 2?")
	      (progn (kill-buffer "*bibtex*") 
		     (kill-buffer "*Occur*")
		     (delete-frame))
	    (throw 'status nil))))

      ;; Run pdflatex two more times
      (jmax-org-pdflatex tex-file)    
      (when jmax-org-interactive-build
	(if (y-or-n-p "Continue to pdflatex 3?")
	    (progn (kill-buffer "*pdflatex*") 
		   (kill-buffer "*Occur*")
		   (delete-frame))
	  (throw 'status nil)))

      (jmax-org-pdflatex tex-file)    
      (kill-buffer "*pdflatex*") 
      (kill-buffer "*Occur*")
      (delete-frame)
      (throw 'status "done")))

    (message "Finished with status = %s" status)

    (kill-buffer tex-buffer)))

(setq org-latex-pdf-process 'jmax-org-latex-pdf-process)

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

;; the real source is in the org-file. whenever the org file is newer
;; we build the el file.
(if(< (float-time (nth 5 (file-attributes "org-ref.el")))
	 (float-time (nth 5 (file-attributes "org-ref.org"))))
    (progn
      (org-babel-tangle-file (expand-file-name "org-ref.org" starter-kit-dir))
      (load-file (expand-file-name "org-ref.el" starter-kit-dir)))
  (require 'org-ref))

(require 'ox-cmu-qualifier)
(require 'ox-cmu-ms-report)
(require 'ox-cmu-dissertation)
(require 'ox-manuscript)
(require 'ox-archive)

(message "jmax-org.el loaded")
