;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(require 'package)

(setq package-archives
      '(("org"         . "http://orgmode.org/elpa/")
	("gnu"         . "http://elpa.gnu.org/packages/")
;	("original"    . "http://tromey.com/elpa/")
	("melpa" . "http://melpa.org/packages/" )
;	("marmalade"   . "http://marmalade-repo.org/packages/")
	))

(setq package-user-dir (expand-file-name "elpa"  starter-kit-dir))

(package-initialize)

(defvar starter-kit-packages
  (list 'flx-ido 'ido-ubiquitous 'smex
        'yasnippet
        'magit
	'bbdb 'bbdb-ext
        'auctex 'reftex
        'undo-tree
	'diminish
	'eimp
	;'icicles
	'org-plus-contrib
	'elpy
	'jedi 'jedi-direx
	'helm
	'f ; file functions https://github.com/rejeep/f.el
	's ; string functions
	'dash ; list functions
	'ht ; hash functions
        'rainbow-mode)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p starter-kit-packages)
  (package-refresh-contents)
  (dolist (package starter-kit-packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

(provide 'packages)
