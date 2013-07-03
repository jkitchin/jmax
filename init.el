;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; remember this directory
(defconst starter-kit-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "directory where the starterkit is installed")

; add this directory to the path for loading lisp files
(add-to-list 'load-path starter-kit-dir)

; make sure we use our org-mode
(add-to-list 'load-path (concat starter-kit-dir "/org-mode/lisp"))
(add-to-list 'load-path (concat starter-kit-dir "/org-mode/contrib/lisp"))

;; load up the starter kit
(require 'org)
(org-babel-load-file (expand-file-name "jmax.org" starter-kit-dir))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6946664b9631989390f8383d538d809a9b093696" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
