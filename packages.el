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

(package-initialize)

(defvar starter-kit-packages
  (list 'flx-ido 'ido-ubiquitous 'smex
        'yasnippet
        'magit
        'auctex 'reftex
        'undo-tree
	'diminish
	'eimp
	'elpy
	'org-plus-contrib
	'jedi 'jedi-direx
	'helm 'helm-themes
	'f ; file functions https://github.com/rejeep/f.el
	's ; string functions
	'dash ; list functions
	'ht ; hash functions
        'rainbow-mode
	'ace-jump-mode 'ace-isearch
	'use-package
	)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p starter-kit-packages)
  (package-refresh-contents)
  (dolist (package starter-kit-packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

(provide 'packages)
