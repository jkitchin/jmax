;;; jmax.el --- Customization for jmax        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/jmax
;; Version: 0.1
;; Keywords: org-mode
;; Package-Requires: ()

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

;;* Packages
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
(setq bookmark-default-file (expand-file-name "user/bookmarks" starter-kit-dir)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;* ido completion
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

;;** smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/
(require 'ido-hacks nil t)
(if (commandp 'ido-vertical-mode)
    (progn
      (ido-vertical-mode 1)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;;* smex, remember recently and most frequently used commands
;; (require 'smex)
;; (setq smex-save-file (expand-file-name "user/.smex-items" starter-kit-dir))
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;* Helm
;; http://tuhdo.github.io/helm-intro.html
(require 'helm)
(setq helm-command-prefix-key "C-c h")
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
					; rebind tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
					; make TAB works in terminal
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

;; * helm extensions
;; add some useful functions to helm-find-files

(defun helm-attach-to-email (candidate)
  (mml-attach-file candidate))


(defun helm-find-files-insert-path (target)
  "Insert relative path to TARGET."
  (insert (file-relative-name target)))


(defun helm-find-files-insert-absolute-path (target)
  "Insert absolute path to TARGET."
  (insert (expand-file-name target)))


(add-hook 'helm-find-files-before-init-hook
          (lambda ()

	    (helm-add-action-to-source
	     "Insert path"
	     'helm-find-files-insert-path
	     helm-source-find-files)

	    (helm-add-action-to-source
	     "Insert absolute path"
	     'helm-find-files-insert-absolute-path
	     helm-source-find-files)

	    (helm-add-action-to-source
	     "Attach file to email"
	     'helm-attach-to-email helm-source-find-files)

	    (helm-add-action-to-source
	     "Make directory"
	     (lambda (target)
	       (make-directory target))
	     helm-source-find-files)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* other loads

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
(when (and (file-exists-p user-dir)
	   jmax-load-user-dir)
  (dolist (file (directory-files user-dir 't "^[^#].*el$"))
    (message "Loading %s" file)
    (load file)))

(require 'jmax-utils)

(require 'python-setup)

(require 'jeldoc)

;;* Spell-checking
(when (setq-default ispell-program-name
		    (or (executable-find "hunspell")
			(executable-find "ispell")
			(executable-find "aspell")))

  (setq ispell-personal-dictionary (concat starter-kit-dir "user/.ispell"))
  (setq ispell-silently-savep t)


  ;; Spell-checking on the fly
  (flyspell-mode +1)

  (add-hook 'flyspell-incorrect-hook
	    (lambda (beg end sym)
	      (message "%s misspelled. Type %s to fix it."
		       (buffer-substring beg end)
		       (substitute-command-keys "\\[flyspell-check-previous-highlighted-word]"))
	      ;; return nil so word is still highlighted.
	      nil))


  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [s-mouse-1] #'flyspell-correct-word)))

  ;; enabling flyspell in special edit source blocks.
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name))) ;; (1)
      ad-do-it				  ;; (2)
      (setq buffer-file-name file-name))) ;; (3)

  ;; flyspell mode for spell checking everywhere
  (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

  ;; configure ispell to ignore some things
  (defun endless/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    ;; this next line approximately ignores org-ref-links
    (add-to-list 'ispell-skip-region-alist '("cite:" . " "))
    (add-to-list 'ispell-skip-region-alist '("label:" . " "))
    (add-to-list 'ispell-skip-region-alist '("ref:" . " "))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

  (add-hook 'org-mode-hook #'endless/org-ispell)

  (defun flyspell-check-next-highlighted-word ()
    "Move to next error and check it."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

  (defhydra spell (:color red)
    "spell"
    ("s" flyspell-check-previous-highlighted-word "previous")
    ("n" flyspell-check-next-highlighted-word "next")
    ("c" ispell-continue "cont")
    ("e" flyspell-goto-next-error "next error")
    ("w" ispell-word "word")
    ("b" ispell-buffer "buffer")
    ("q" nil "quit" :color blue))

  (global-set-key (kbd "s-s") 'flyspell-check-previous-highlighted-word)
  (global-set-key (kbd "M-s-s") 'spell/body))

(setq save-abbrevs t
      only-global-abbrevs t)
(setq-default abbrev-mode t)

(defun jmax-define-abbrev (abbreviation expansion)
  "Define an ABBREVIATION that globally expands to EXPANSION.
for example: cheme to Chemical Engineering. This is essentially
like `inverse-add-blobal-abbrev', but doesn't require the prefix
arg, and is easier to remember I think."
  (interactive "sAbbreviation: \nsExpansion: ")
  (when (string-match " " abbreviation)
    (error "No spaces allowed in an abbreviation"))
  (define-abbrev global-abbrev-table abbreviation expansion))


;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p global-abbrev-table local-abbrev-table)
        bef aft))))




;;* Theme
;; load this last so that the user theme can be loaded
(add-to-list 'custom-theme-load-path (expand-file-name "themes" starter-kit-dir))
(load-theme jmax-user-theme t)

(provide 'jmax)
;;; jmax.el ends here
