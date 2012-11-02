
(setq frame-title-format "jmax - awesome editing")

(global-visual-line-mode 1) ; how long lines are handled
;; turn on font-lock mode
(global-font-lock-mode t)

(show-paren-mode 1)
(line-number-mode 1)

;disable backup
(setq backup-inhibited t)

(setq inhibit-startup-screen t)

; answer with y/n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'org-list)
(require 'org-special-blocks)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key [f12] 'org-mode)

(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition

(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

(setq org-return-follows-link t)

; do not evaluate code on export
(setq org-export-babel-evaluate nil)


; register python in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

; enable prompt-free code running
(setq org-confirm-babel-evaluate nil)

; no extra indentation
(setq org-src-preserve-indentation t)
(setq org-startup-with-inline-images "inlineimages")

; use syntax highlighting in org-file
(setq org-src-fontify-natively t)

; set default :results to output
(setq org-babel-default-header-args
      (cons '(:results . "replace output")
	    (assq-delete-all :results org-babel-default-header-args)))

(setq org-babel-default-header-args
      (cons '(:exports . "both")
	    (assq-delete-all :exports org-babel-default-header-args)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:foreground "red" :underline t :weight bold))))
 '(org-link ((t (:inherit link :foreground "medium blue" :underline t)))))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-export-latex-default-packages-alist
      (quote
       (("AUTO" "inputenc" t)
	("" "fixltx2e" nil)
    ("" "url")
	("" "graphicx" t)
    ("" "minted" t)
    ("" "color" t)
	("" "longtable" nil)
	("" "float" nil)
	("" "wrapfig" nil)
	("" "soul" t)
	("" "textcomp" t)
    ("" "amsmath" t)
	("" "marvosym" t)
	("" "wasysym" t)
	("" "latexsym" t)
	("" "amssymb" t)
	("linktocpage,
  pdfstartview=FitH,
  colorlinks,
  linkcolor=blue,
  anchorcolor=blue,
  citecolor=blue,
  filecolor=blue,
  menucolor=blue,
  urlcolor=blue" "hyperref" t)
	("" "attachfile" t)
	"\\tolerance=1000")))

(setq org-export-latex-listings 'minted)
(setq org-export-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

; here is a way to get pydoc in a link: [[pydoc:numpy]]
(setq org-link-abbrev-alist
      '(("pydoc" . "shell:pydoc %s")))

;; these allow me to write mod:numpy or func:numpy.dot to get
;; clickable links to documentation
(org-add-link-type
 "mod"
 (lambda (arg)
   (shell-command (format "pydoc %s" arg) nil))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\texttt{%s}" path)))))

(org-add-link-type
 "func"
 (lambda (arg)
   (shell-command (format "pydoc %s" arg) nil))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\texttt{%s}" path)))))

;;; support for links to microsoft docx,pptx,xlsx files
;;; standard org-mode opens these as zip-files
;;  http://orgmode.org/manual/Adding-hyperlink-types.html
(org-add-link-type "msx" 'org-msx-open)

(defun org-msx-open (path)
       "Visit the msx file on PATH.

uses the dos command:
start  empty title path
"
       (shell-command
	(concat "start \"title\" " (shell-quote-argument path)) t))

(org-add-link-type "ashell" 'org-ashell-open)
(defun org-ashell-open (cmd)
"open an ashell:cmd link
[[ashell:xterm -e \"cd 0; ls && /bin/bash\"]]

I use this to run commands asynchronously in the shell. org-mode runs shell links in a blocking mode, which is annoying when you open an xterm."
(start-process-shell-command "ashell" "*scratch*" cmd))

;; -*- emacs-lisp -*-   [[color:red][in red]]
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
		    (progn (add-text-properties
			    0 (length path)
			    (list 'face `((t (:foreground ,path))))
			    path) path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))

;; -*- emacs-lisp -*-   [[incar:keyword]]
; this makes nice links in org-mode to the online documentation and
; renders useful links in output
(org-add-link-type
 "incar"
  (lambda (keyword)
    (shell-command (format "firefox http://cms.mpi.univie.ac.at/wiki/index.php/%s" keyword) nil))
  ; this function is for formatting
  (lambda (keyword link format)
   (cond
    ((eq format 'html)
     (format "<a href=http://cms.mpi.univie.ac.at/wiki/index.php/%s>%s</a>" keyword keyword))
    ((eq format 'latex)
     (format "\\href{http://cms.mpi.univie.ac.at/wiki/index.php/%s}{%s}"  keyword keyword)

))))

(org-add-link-type
 "image"
 (lambda (keyword)
   ()) ; do nothing. maybe figure out how to open a png or pdf
 (lambda (keyword link format)
   (cond
    ((eq format 'latex)
     (format "\\includegraphics{%s.pdf}" keyword)))))

;; http://sdpconfig.wordpress.com/2011/12/21/unwrapping-paragraphs-in-emacs/
(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

(defun double-space ()
  "make buffer look approximately double-spaced"
  (interactive)
  (setq line-spacing 10))

(defun single-space ()
  "make buffer single-spaced"
  (interactive)
  (setq line-spacing nil))

;http://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#File-Name-Expansion

(defun get-path()
  "opens dired so you can navigate to a file to insert a path to it in the current buffer"
  (interactive)
  ; store current buffer so we can change back to it
  (setq current_buffer (current-buffer))
  (setq buffer_name (buffer-file-name))
  ; now call dired to navigate to the path you want
  (dired ())
)

(defun insert-relative-path()
  "inserts the relative path between the original buffer and current file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename))
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (file-relative-name selected_file));inserts relative path
)

(defun insert-absolute-path()
  "Inserts the absolute path to the file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename)) ; this is the file the cursor is on
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (expand-file-name selected_file));inserts absolute path
)

(defun insert-buffer-filename()
  "Inserts filename associated with current buffer"
  (interactive)
  (insert (buffer-file-name))
)

(global-set-key "\C-cg" 'get-path )
(global-set-key "\C-cp" 'insert-relative-path)
(global-set-key "\C-cf" 'insert-buffer-filename)
