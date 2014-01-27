(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

(defun double-space ()
  "make buffer look approximately double-spaced"
  (interactive)
  (setq line-spacing 10))

(defun single-space ()
  "make buffer single-spaced"
  (interactive)
  (setq line-spacing nil))
