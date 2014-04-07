;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  You should not need to modify paths below here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-use-speed-commands t)

(require 'ox-beamer)
(require 'ox-texinfo)
(require 'org-inlinetask)
(require 'org-mouse)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb) ; convenient switching between open org-buffers

(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition

(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

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
   (ruby . t)
   (org . t)
   (dot . t)
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

(require 'reftex)
(require 'reftex-cite)

;; I like a random bibtex entry in my agenda.
;(defun formatted-bibtex-entry ()
;  "return a bibtex entry as a formatted string. I hand-built the format.

;this treats all entries as a journal article."
;  (interactive)
;  (bibtex-beginning-of-entry)
;  (let* ((bibtex-expand-strings t)
;         (entry (bibtex-parse-entry t))
;         (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
;         (year  (reftex-get-bib-field "year" entry))
;         (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
;         (key (reftex-get-bib-field "=key=" entry))
;         (journal (reftex-get-bib-field "journal" entry))
;         (volume (reftex-get-bib-field "volume" entry))
;         (issue (reftex-get-bib-field "issue" entry))
;         (pages (reftex-get-bib-field "pages" entry))
;         (doi (reftex-get-bib-field "doi" entry))
;         (output))
;    (concat (when author author) ", "
;                         (when title title) ", "
;                         (when journal journal) ", "
;                         (when volume volume)
;                         (if issue (format "(%s), " issue) ", ")
;                         (when pages pages) ", "
;                         (when year (format "(%s)." year))
;                         (when doi (format " http://dx.doi.org/%s" doi)))))

;; this returns a string for my agenda.
;(defun get-random-bibtex-entry (&optional arg)
;  "for printing in my agenda"
;  (let ((keys) (lucky-key) (output))
;    (with-current-buffer
;	(find-file (car reftex-default-bibliography))
;      (setq keys (bibtex-parse-keys))
;      (setq lucky-key (car
;		       (nth
;			(random (safe-length keys)) keys)))
;      (goto-char (point-min))
;      (re-search-forward lucky-key)
;      (setq output (formatted-bibtex-entry))
;      (kill-buffer)
;      (format "%s\ncite:%s" output lucky-key))))

;(setq initial-scratch-message (get-random-bibtex-entry))
;; this only works on linux for some reason
(setq initial-major-mode 'org-mode)

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

;; function to open agenda
(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              )))
      (call-interactively 'org-agenda-list)))
  )

;; open agenda after 15 minutes of idle time
;(run-with-idle-timer 900 t 'jump-to-org-agenda)

;; Customize generic export
;; I reset this variable to get my hyperref setup
(defun index (element list)
  "return the index of element in list"
  (let ((i 0)
	(found nil))
    (dolist (el list i)
      (if (equal el element) 
	  (progn 
	    (setq found t)
	    (return i)))
      (setq i (+ i 1)))
    ;; return counter if found, otherwise return nil
    (if found i nil)))

;; this is for code syntax highlighting in export
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;; do not put in \hypersetup
;; use your own \hypersetup{pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}
(setq org-latex-with-hyperref nil)

; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %f"
        "makeindex %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch"))

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

(org-add-link-type "msx" 'org-msx-open)


;; Setup the frame configuration for following links.
(setq org-link-frame-setup (quote ((gnus . org-gnus-no-new-news)
                                   (file . find-file))))

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)


;; get our updated org-info
(add-to-list 'Info-additional-directory-list (expand-file-name "org-mode/info" starter-kit-dir))

;; use this code in emacs-lisp for folding code.
(global-set-key (kbd "C-M-]") (lambda () (interactive) (org-cycle t)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (ignore-errors
                                (end-of-defun)
                                (beginning-of-defun))
                              (org-cycle)))

(setq org-completion-use-ido t)

(require 'org-secretary)
(setq org-todo-keywords
       '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
         (sequence "GNUS(g)" "|" "DONE(d)")
         (sequence "TASK(f)" "|" "DONE(d)")))

;; It helps to distinguish them by color, like this:
;;
(setq org-todo-keyword-faces
       '(("TODO" . (:foreground "red" :weight bold))
         ("DONE" . (:foreground "ForestGreen"))
         ("GNUS" . (:foreground "RoyalBlue3"))
         ("TASK" . (:foreground "RoyalBlue3"))))


(message "jmax-org.el loaded")
