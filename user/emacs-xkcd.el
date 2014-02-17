
(require 'json)
(require 'url)

(defvar xkcd-alt nil)
(defvar xkcd-cur nil)

(defgroup xkcd nil
  "A xkcd reader for Emacs")

(defcustom xkcd-cache-dir "~/.emacs.d/xkcd/"
  "Directory to cache images and json files to."
  :group 'xkcd
  :type 'directory)


(define-minor-mode xkcd-mode
  "Minor mode for viewing xkcd in Emacs"
  :lighter " xkcd"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<right>") 'xkcd-next)
	    (define-key map (kbd "<left>") 'xkcd-prev)
	    (define-key map (kbd "C-c r") 'xkcd-rand)
	    (define-key map (kbd "C-c t") 'xkcd-alt-text)
	    (define-key map (kbd "q") 'xkcd-kill-buffer)
	    map))

(defun xkcd-kill-buffer ()
  "kill the xkcd buffer"
  (interactive)
  (kill-buffer "*xkcd*")
  (xkcd-mode -1))

(defun xkcd-get-json (url &optional num)
  (let ((json nil))
    (let ((file (concat xkcd-cache-dir (number-to-string num) ".json")))
      (if (and (file-exists-p file) (not (eq num 0)))
	  (with-current-buffer (find-file-literally file) ;; File already exists in the cache
	    (setq json (buffer-substring-no-properties (point-min) (point-max)))
	    (kill-buffer (current-buffer))
	    json)
	(let ((buffer (url-retrieve-synchronously url)))
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (re-search-forward "^$")
	    (setq json (buffer-substring-no-properties (+ (point) 1) (point-max)))
	    (kill-buffer (current-buffer)))
	  json)))))

(defun xkcd-download (url num)
  "Download the image linked by URL. If the file arleady exists, do nothing"
  ;;check if the cache directory exists
  (if (not (file-exists-p xkcd-cache-dir))
      (make-directory xkcd-cache-dir))
  (let ((name (concat xkcd-cache-dir (number-to-string num) ".png")))
    (if (file-exists-p name)
	nil
      (url-copy-file url name))))

(defun xkcd-cache-json (num json-string)
  "Save comic json to cache directory"
  (let ((name (concat xkcd-cache-dir (number-to-string num) ".json")))
   (if (file-exists-p name)
      nil
    (with-current-buffer (find-file name)
      (insert json-string)
      (save-buffer)
      (kill-buffer (current-buffer))))))

(defun xkcd-get (num)
  "Get the xkcd number NUM"
  (interactive "nEnter comic number: ")
  (get-buffer-create "*xkcd*")
  (switch-to-buffer "*xkcd*")
  (if buffer-read-only
      (toggle-read-only))
  (erase-buffer)
  (if (and (boundp 'xkcd-mode) (not xkcd-mode))
      (xkcd-mode))
  (setq xkcd-cur num)
  (let ((out (if (eq num 0)
		 (xkcd-get-json "http://xkcd.com/info.0.json" 0)
	       (xkcd-get-json (concat "http://xkcd.com/" (number-to-string num)
				      "/info.0.json") num)))
	(img nil)
	(num nil)
	(title nil))
    (setq num (cdr (assoc 'num (json-read-from-string out))))
    (setq img (cdr (assoc 'img (json-read-from-string out))))
    
    ;; FIXME: This looks pretty ugly.
    (message "Downloading comic...")
    (xkcd-download img num)
    (setq title (format "%d: %s" (cdr (assoc 'num (json-read-from-string out)))
			(cdr (assoc 'safe_title (json-read-from-string out)))))
    (insert (concat title "\n"))
    (insert-image (create-image
		   (concat xkcd-cache-dir
			   (number-to-string
			    (cdr
			     (assoc 'num (json-read-from-string out)))) ".png") 'png))
    (if (eq xkcd-cur 0)
	(setq xkcd-cur (cdr (assoc 'num (json-read-from-string out)))))
    (xkcd-cache-json num out)
    (setq xkcd-alt (cdr (assoc 'alt (json-read-from-string out))))
    (read-only-mode)
    (message title)))

(defun xkcd-next ()
  "Get next xkcd"
  (interactive)
  (xkcd-get (+ xkcd-cur 1)))

(defun xkcd-prev ()
  "Get previous xkcd"
  (interactive)
  (xkcd-get (- xkcd-cur 1)))

(defun xkcd-rand ()
  "Show random xkcd"
  (interactive)
  (xkcd-get (random (cdr (assoc 'num (json-read-from-string
				      (xkcd-get-json "http://xkcd.com/info.0.json" 0)))))))
  
(defun xkcd-get-latest ()
  "Get the latest xkcd"
  (interactive)
  (if (and (boundp 'xkcd-mode) (not xkcd-mode))
      (xkcd-mode))
  (xkcd-get 0))

(defun xkcd-alt-text ()
  (interactive)
  (message xkcd-alt))

(provide 'emacs-xkcd)
;;; emacs-xkcd.el ends here
