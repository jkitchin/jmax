;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(require 'package)

(setq package-archives
      '(("elpy" . "http://jorgenschaefer.github.io/packages/")
	("org"         . "http://orgmode.org/elpa/")
	("gnu"         . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	("melpa" . "http://melpa.org/packages/" )
	))

(setq package-user-dir (expand-file-name "elpa"  starter-kit-dir))

;; we need this specific version of highlight-indentation. This is a little
;; hackery to make sure we get the version from the elpy repo.
(unless (file-directory-p (expand-file-name "elpa/highlight-indentation-0.5.0" starter-kit-dir))
  (let ((package-archives '(("elpy" . "http://jorgenschaefer.github.io/packages/"))))
    (package-initialize)
    (package-refresh-contents)
    (package-install 'highlight-indentation)))

(package-initialize)

(defvar starter-kit-packages
  (list 'flx-ido 'ido-ubiquitous 'smex
	'dired-details 'dired-details+
        'yasnippet
        'magit
        'auctex 'reftex
        'undo-tree
	'diminish
	'eimp
	'elpy 'pydoc
	'org-plus-contrib
	'jedi 'jedi-direx
	'helm 'helm-themes 'helm-bibtex
	'f ; file functions https://github.com/rejeep/f.el
	's ; string functions
	'dash ; list functions
	'ht ; hash functions
        'rainbow-mode
	'ace-jump-mode 'ace-isearch
	'use-package
	'hydra 'key-chord
	)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p starter-kit-packages)
  (package-refresh-contents)
  (dolist (package starter-kit-packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

(provide 'packages)
