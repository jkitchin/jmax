;; kitchingroup minor mode
;; I should figure out some good stuff to put here for the group.

(require 'easymenu)

(defvar kitchingroup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") '(lambda () (ding)))
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-file-log)
    (define-key map (kbd "s-m b") 'magit-blame-mode)

    map)
  "Keymap for kitchingroup mode.")

(defun kg-open-my-org ()
  "Open my org-file"
  (find-file my-kitchingroup-org-file))

(easy-menu-define my-menu kitchingroup-mode-map "My own menu"
  '("KitchinGroup"
    ["email" my-function t]
    ("org-mode"
     ["open my file" open-my-file t]
     ["get my agenda" my-agenda t])
    ("bibtex"
     ["validate bibtex file" my-obscure-function t])))


(define-minor-mode kitchingroup-mode
  "Minor mode for kitchingroup

\\{kitchingroup-mode-map}"
  :lighter " KG"
  :keymap kitchingroup-mode-map)

(provide 'kitchingroup-mode)
;;; kitchingroup-mode.el ends here
