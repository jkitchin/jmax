;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; You should not need to modify below here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas-global-mode +1)
(yas-load-directory (expand-file-name "snippets" starter-kit-dir))

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


;; sensible undo
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" starter-kit-dir)
      bookmark-save-flag 1)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; disable auto-fill
(auto-fill-mode -1)

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


(setq abbrev-file-name (expand-file-name "user/abbrev_defs" starter-kit-dir))
(setq save-abbrevs t) 
(setq-default abbrev-mode t)


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

;; I do not like autofill mode.
(auto-fill-mode -1)

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

;; Spell-checking on the fly
(flyspell-mode +1)  
(global-set-key (kbd "<f5>") 'flyspell-buffer)

(defun flyspell-check-next-highlighted-word ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "<f6>") 'flyspell-check-next-highlighted-word)
(global-set-key (kbd "C-<f6>") 'flyspell-check-previous-highlighted-word)

;; automatically show completions for execute-extended-command
(icomplete-mode 1)

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

(global-unset-key "\C-cg")
(global-set-key "\C-cg" 'get-path)
(global-set-key "\C-cp" 'insert-relative-path)
(global-set-key "\C-cf" 'insert-buffer-filename)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;(require 'kitchingroup-mode)
;(kitchingroup-mode +1)

(load-file (expand-file-name "email.el" starter-kit-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; bbdb 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq bbdb-file (expand-file-name "user/bbdb" starter-kit-dir))

(require 'bbdb)
(bbdb-initialize 'gnus 'message)


;; This is not defined in my bbdb/icicles installation. This lets me use tab-completion.
(defalias 'icicle-bbdb-complete-name 'bbdb-complete-mail) 

;; http://emacs-fu.blogspot.com/2009/08/managing-e-mail-addresses-with-bbdb.html
;; these seem to be v2 variables.
;; see http://www.emacswiki.org/emacs/UpgradeBBDB for new names
(setq 
;;    bbdb-offer-save 1                        ;; 1 means save-without-asking
;;    bbdb-use-pop-up t                        ;; allow popups for addresses
;;    bbdb-electric-p t                        ;; be disposable with SPC
;;    bbdb-popup-target-lines  1               ;; very small
    
;;    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
;;    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

;;    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
;;    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

    bbdb-completion-list t                 ;; complete on anything

;;    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially

;;    bbbd-message-caching-enabled t           ;; be fast
;;    bbdb-use-alternate-names t               ;; use AKA

;;    bbdb-elided-display t                    ;; single-line addresses
)

;; we use our own org-mode
;; load my org-mode
(add-to-list 'load-path (expand-file-name "org-mode/lisp" starter-kit-dir))
(add-to-list 'load-path (expand-file-name "org-mode/contrib/lisp" starter-kit-dir))
(require 'org)


;; We load all .el files in the user directory. No order is guaranteed.
(add-to-list 'load-path user-dir)
(when (file-exists-p user-dir)
  (mapc 'load (directory-files user-dir 't "^[^#].*el$")))

;; Load up org mode
(load-file (expand-file-name "jmax-org.el" starter-kit-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "jorg-bib.el" starter-kit-dir))

;; hide details in dired
(require 'dired-details+)
(setq dired-details-hidden-string "")






;; icicles should supposedly be loaded last
(require 'icicles)
;; reclaim C-c ' for org-mode
(setq icicle-top-level-key-bindings
      (remove '("'" icicle-occur t) icicle-top-level-key-bindings))

(icy-mode 1)

(provide 'jmax)
