;;; lab-notebook.el --- lab-notebook-mode



;;; Commentary:
;; 

;;; Code:

(defun dwin-vc ()
  "Do what I need in vc next.
Add current file if not in vc, then prompt for commit message"
  (interactive)
  (save-buffer)
  (when (file-exists-p (buffer-file-name))
    ;; register the file if it is not
    (unless (vc-registered (buffer-file-name))
      (vc-register)))
    
  ;; Now commit it
  (vc-checkin `(,(buffer-file-name))
	      (vc-backend (buffer-file-name))
	      (read-string "Commit log: ")
	      t
	      ))



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
            ;(define-key map (kbd "<f5>") 'export-bibtex)
            map)
  ;; body
  (add-hook 'kill-buffer-hook 'dwin-vc nil 'make-it-local))
 

(provide 'lab-notebook)

;;; lab-notebook.el ends here
