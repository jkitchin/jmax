;; we use our own org-mode
;; load my org-mode
(add-to-list 'load-path (expand-file-name "org-mode/lisp" starter-kit-dir))
(add-to-list 'load-path (expand-file-name "org-mode/contrib/lisp" starter-kit-dir))


(add-to-list 'load-path user-dir)
(when (file-exists-p user-dir)
  (mapc 'load (directory-files user-dir 't "^[^#].*el$")))

(require 'cl)           ;; common-lisp functions

(require 'saveplace)     ;; When you visit a file, point goes to the
                         ;; last place where it was when you
                         ;; previously visited the same file.

(require 'ffap)          ;; find-file-at-point

(require 'uniquify)      ;; overrides Emacs' default mechanism for
                         ;; making buffer names unique (using suffixes
                         ;; like <2>, <3> etc.) with a more sensible
                         ;; behaviour which use parts of the file
                         ;; names to make the buffer names
                         ;; distinguishable.

(require 'ansi-color)    ;; translates ANSI SGR (Select Graphic
                         ;; Rendition) escape sequences like "Esc [ 30
                         ;; m" into EmacsOverlays, TextProperties, or
                         ;; XEmacsExtents with face colours, bold,
                         ;; etc.

;; diminish keeps the modeline tidy
(require 'diminish)

(require 'undo-tree)
;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" starter-kit-dir)
      bookmark-save-flag 1)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'icicles)
;; reclaim C-c ' for org-mode
(setq icicle-top-level-key-bindings
      (remove '("'" icicle-occur t) icicle-top-level-key-bindings))

(icy-mode 1)



(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "user/ido.hist" starter-kit-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;; smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name "user/.smex-items" starter-kit-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; personal preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add this directory to the path for loading lisp files
(add-to-list 'load-path starter-kit-dir)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" starter-kit-dir))
(load-theme 'my t) ; my old theme from grad school. it looks like xemacs.

;;Tell the program who you are
(setq user-full-name "John Kitchin"
      andrewid "jkitchin"
      user-mail-address "jkitchin@andrew.cmu.edu")

;; setup to send email out by andrewid.
;; you will be prompted for a password, and asked to store 
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.andrew.cmu.edu"
      smtpmail-smtp-server smtpmail-default-smtp-server
      smtpmail-starttls-credentials `((,smtpmail-smtp-server 587 nil nil))
      smtpmail-auth-credentials `((,smtpmail-smtp-server 587 andrewid nil))
      smtpmail-smtp-service 587)

;; kill mail buffers when exiting
(setq  message-kill-buffer-on-exit t)

;; set the frame title to show file names
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq inhibit-startup-screen t) ;; stop showing startup screen
(tool-bar-mode 0)           ; remove the icons
(menu-bar-mode 1)           ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled.  This
                            ;; appears to wrap long lines visually,
                            ;; but not add line-returns

(global-font-lock-mode t)   ;; turn on font-lock mode everywhere

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression, 'parenthesis or 'mixed

(line-number-mode 1)  ;; turn linumbers on in mode-line

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no


;; Disable all version control. makes startup and opening files much faster
(setq vc-handled-backends nil)

;; http://emacsredux.com/blog/2013/04/05/recently-visited-files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
(setq save-place-file (expand-file-name "user/saved-places" starter-kit-dir))

(global-set-key (kbd "<f7>") 'recentf-open-files)


;; modified from http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
(defvar my-filelist nil "alist for files i need to open frequently. Key is a short abbrev, Value is file path.")
(setq my-filelist
      '(
        ("master" . "~/Dropbox/org-mode/master.org")
        (".emacs.d" . "~/Dropbox/.emacs.d" )
        ("blog" . "~/Dropbox/blogofile-jkitchin.github.com/_blog/blog.org")
        ("ese" . "~/Dropbox/books/ese-book/ese.org" )
        ("pycse" . "~/Dropbox/books/pycse/pycse.org")
        ("references" . "~/Dropbox/bibliography/references.bib")
        ("notes" . "~/Dropbox/bibliography/notes.org")
        ("journal" . "~/Dropbox/org-mode/journal.org")
        ("tasks" . "~/Dropbox/org-mode/tasks.org")
        ;; more here
        ) )

(defun my-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set in `my-filelist."
  (interactive
   (list (ido-completing-read "Open:" (mapcar (lambda (x) (car x)) my-filelist))))
  (find-file (cdr (assoc openCode my-filelist))))

(global-set-key [f9] 'my-open-file-fast)

;http://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#File-Name-Expansion

(defun get-path()
  "opens dired so you can navigate to a file to insert a path to it in the current buffer"
  (interactive)
  ; store current buffer so we can change back to it
  (setq current_buffer (current-buffer))
  (setq buffer_name (buffer-file-name)) ; filename current buffer points to
  ; now call dired to navigate to the path you want
  (dired ()))

(defun insert-relative-path()
  "inserts the relative path between the original buffer and current file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename))
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (file-relative-name selected_file)))

(defun insert-absolute-path()
  "Inserts the absolute path to the file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename)) ; this is the file the cursor is on
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (expand-file-name selected_file)))

(defun insert-buffer-filename()
  "Inserts filename associated with current buffer"
  (interactive)
  (insert (buffer-file-name)))

;; These do not seem to work in prelude
(global-unset-key "\C-cg")
(global-set-key "\C-cg" 'get-path)
(global-set-key "\C-cp" 'insert-relative-path)
(global-set-key "\C-cf" 'insert-buffer-filename)


(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see jorg-bib.el for use of these variables
(setq jorg-bib-bibliography-notes "~/Dropbox/bibliography/notes.org"
      jorg-bib-default-bibliography '("~/Dropbox/bibliography/references.bib")
      jorg-bib-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; python customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq python-indent-offset 4)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)


;; I want python to always show me the output. this advice makes that happen.
(defadvice python-shell-send-buffer (before switch-to-python-output activate)
  "Show python output in another frame after you run a script"
  (switch-to-buffer-other-frame
   (process-buffer (python-shell-get-or-create-process))))

;; http://www.yilmazhuseyin.com/blog/dev/emacs-setup-python-development/
(require 'flymake)
(when (load "flymake" t) 
     (defun flymake-pyflakes-init () 
       (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                          'flymake-create-temp-inplace)) 
      (local-file (file-relative-name 
               temp-file 
               (file-name-directory buffer-file-name)))) 
         (list "pyflakes" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks 
          '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "jorg-bib.el" starter-kit-dir))

(require 'kitchingroup-mode)
(kitchingroup-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; bbdb setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(setq bbdb-file (expand-file-name "user/bbdb" starter-kit-dir))
 
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; http://emacs-fu.blogspot.com/2009/08/managing-e-mail-addresses-with-bbdb.html
(setq 
    bbdb-offer-save 1                        ;; 1 means save-without-asking
    bbdb-use-pop-up t                        ;; allow popups for addresses
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

    bbdb-completion-type nil                 ;; complete on anything

    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially

    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA

    bbdb-elided-display t                    ;; single-line addresses
)

(require 'org)

(provide 'customization)
