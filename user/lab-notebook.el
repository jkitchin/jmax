

(defun dwin-vc ()
  "Do what I need in vc next.
Shows the diff in one window, a commit message in another one."
  (interactive)

  (when (file-exists-p (buffer-file-name))
    ;; register the file if it is not
    (unless (vc-registered (buffer-file-name))
      (vc-register))
    
    ;; do next thing, probably commit
    (vc-next-action nil)))



;;;###autoload
(define-minor-mode lab-notebook-mode
  "Toggle lab notebook mode.
This will prompt you to commit a file when you kill a buffer
"
  ;; initial value
  nil
  ;; indicator
  :lighter " LN"
  ;; keybindings
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f5>") 'export-bibtex)
            map)
  ;; body
  (add-hook 'kill-buffer-hook 'dwin-vc nil 'make-it-local))
 
(provide 'lab-notebook-mode)
