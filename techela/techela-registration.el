;; These contents are at http://techela.cheme.cmu.edu/techela. It is raw emacs-lisp data structure.
;;
;; (("s15-06640" . ((title . "Molecular Simulation")
;;		 (instructor . "John Kitchin")
;;		 (admin . "jkitchin@andrew.cmu.edu")
;;		 (url . "s15-06640@techela.cheme.cmu.edu:course")))
;;  ("f14-06625" . ((title . "Chemical Reactive Systems")
;;		 (instructor . "John Kitchin")
;;		 (admin . "jkitchin@andrew.cmu.edu")
;;		 (url . "f14-06625@techela.cheme.cmu.edu:course"))))


(defvar ta-techela-data nil
  "Data for available courses. This is obtained from http://techela.cheme.cmu.edu/techela. We cache that data in this variable.")

(defun ta-get-techela-data ()
  "Loads data from the techela site containing available courses."
  ;; we save the data the first time we get it, so we do not have to contact the server every time.
  (or ta-techela-data
      (setq ta-techela-data
	    (read (with-current-buffer
		      (url-retrieve-synchronously "http://techela.cheme.cmu.edu/techela")
		    (buffer-substring url-http-end-of-headers (point-max)))))))


(defun tq-config-get-user-courses ()
  "Get a list of available courses."
  (mapcar 'car (ta-get-techela-data)))


(defun tq-get-course-admin-email (course)
  "Return email of course admin.
This is usually who pub keys should be sent to."
  (interactive (list (ido-completing-read
		      "Course: "
		      (tq-config-get-user-courses))))
  (cdr (assoc 'admin (cdr (assoc course (ta-get-techela-data))))))
