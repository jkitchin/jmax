;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; remember this directory
(setq starter-kit-dir
      (file-name-directory (or load-file-name (buffer-file-name))))

(add-to-list 'load-path starter-kit-dir)

; make sure we use our org-mode
(add-to-list 'load-path (concat starter-kit-dir "org-mode/lisp"))
(add-to-list 'load-path (concat starter-kit-dir "/org-mode/contrib/lisp"))

;; load up the starter kit
(org-babel-load-file (expand-file-name "jmax.org" starter-kit-dir))

;;; init.el ends here
