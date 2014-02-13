;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
	("original"    . "http://tromey.com/elpa/")
	("org"         . "http://orgmode.org/elpa/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade"   . "http://marmalade-repo.org/packages/")))

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
	'icicles
	'elpy
        'rainbow-mode)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p starter-kit-packages)
  (package-refresh-contents)
  (dolist (package starter-kit-packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

(provide 'packages)
