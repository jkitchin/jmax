;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; python customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try the launch pad python-mode
(add-to-list 'load-path (expand-file-name "python-mode" starter-kit-dir))
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


(setq python-indent-offset 4)

;; turn off yasnippets in python mode
(add-hook 'python-mode-hook #'(lambda () (yas-global-mode -1)))

;; I want python to always show me the output. this advice makes that happen.
(defadvice python-shell-send-buffer (after switch-to-python-output activate)
  "Show python output in another frame after you run a script"
  (switch-to-buffer-other-frame "*Python*"))


(elpy-enable)

;; this is my pydoc
(require 'pydoc)
