;; * Packages
(defcustom jmax-user-theme 'leuven
  "User defined theme to load")

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

;; we load the custom file if it exists.
(setq custom-file (expand-file-name "user/custom.el" starter-kit-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" starter-kit-dir)
      bookmark-save-flag 1)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; disable auto-fill
(auto-fill-mode -1)

(require 'words)

;; hide details in dired
(require 'dired-details+)
(setq dired-details-hidden-string "")


(require 'jmax-mode)
(jmax-mode 1)


;; * personal preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; add this directory to the path for loading lisp files
(add-to-list 'load-path starter-kit-dir)

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
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

;; http://emacsredux.com/blog/2013/04/05/recently-visited-files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
(setq save-place-file (expand-file-name "user/saved-places" starter-kit-dir))
(global-set-key (kbd "<f7>") 'helm-recentf)

;; automatically show completions for execute-extended-command
(icomplete-mode 1)

;; * ido completion
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

;; ** smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/
(require 'ido-hacks nil t)
(if (commandp 'ido-vertical-mode)
    (progn
      (ido-vertical-mode 1)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;; * smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name "user/.smex-items" starter-kit-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; * Helm
;; http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p") 'helm-grep-mode-jump-other-window-backward)

(define-key helm-map (kbd "C-x 2") 'helm-select-2nd-action)
(define-key helm-map (kbd "C-x 3") 'helm-select-3rd-action)
(define-key helm-map (kbd "C-x 4") 'helm-select-4rd-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r l") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h r") 'helm-resume)
(global-set-key (kbd "C-c m") 'helm-all-mark-rings)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

  ;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; * utility functions
;http://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#File-Name-Expansion

(defun get-path()
  "Opens dired so you can navigate to a file to insert a path to it in the current buffer."
  (interactive)
  ; store current point so we can change back to it later
  (setq current_point (point-marker))
  ; now call dired to navigate to the path you want
  (dired nil))


(defun insert-relative-path()
  "Inserts the relative path between the original buffer and current file selected in dired."
  (interactive)
  (let ((selected_file (dired-get-filename)))
    (switch-to-buffer (marker-buffer current_point))
    (goto-char current_point)
    (insert (file-relative-name selected_file))))


(defun insert-absolute-path()
  "Inserts the absolute path to the file selected in dired to the previous buffer."
  (interactive)
  (let ((selected_file (dired-get-filename))) ; this is the file the cursor is on
    (switch-to-buffer (marker-buffer current_point))
    (goto-char current_point)
    (insert  (expand-file-name selected_file))))


(defun insert-path (&optional arg)
  "Insert relative path unless prefix is used, then absolute path"
  (interactive "P")
  (if (equal arg nil)
      (insert-relative-path)
    (insert-absolute-path)))


(defun insert-buffer-filename()
  "Inserts filename associated with current buffer."
  (interactive)
  (insert (buffer-file-name)))

(global-unset-key "\C-cg")
(global-set-key "\C-cg" 'get-path)
(global-set-key "\C-cp" 'insert-path)
(global-set-key "\C-cf" 'insert-buffer-filename)


(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))


(defun double-space ()
  "Make buffer look approximately double-spaced."
  (interactive)
  (setq line-spacing 10))


(defun single-space ()
  "Make buffer single-spaced."
  (interactive)
  (setq line-spacing nil))


(defun vc-git-push ()
  "Run git push."
  (interactive)
  (shell-command "git push"))

(global-set-key (kbd "C-x v p") 'vc-git-push)

;; * other loads

(load-file (expand-file-name "email.el" starter-kit-dir))

;; add matlab
(add-to-list 'load-path (expand-file-name "matlab" starter-kit-dir))
 (load-library "matlab-load")

;; Load up org mode
(load-file (expand-file-name "jmax-org.el" starter-kit-dir))

;; load course module
(add-to-list 'load-path (expand-file-name "techela" starter-kit-dir))
(require 'techela)

;; We load all .el files in the user directory. No order is guaranteed.
(add-to-list 'load-path user-dir)
(when (file-exists-p user-dir)
  (dolist (file (directory-files user-dir 't "^[^#].*el$"))
    (message "Loading %s" file)
    (load file)))

(require 'jmax-utils)

(require 'python-setup)

;; * Theme
;; load this last so that the user theme can be loaded
(add-to-list 'custom-theme-load-path (expand-file-name "themes" starter-kit-dir))
(load-theme jmax-user-theme t)

(provide 'jmax)
