



(defvar kitchingroup-root "~/Dropbox/kitchingroup"
  "root directory where kitchingroup is")

(defvar kitchingroup
  '("ahallenb"
    "jboes"
    "jdmichae"
    "mcurnan"
    "mehakc"
    "meihengl"
    "ngovinda"
    "qingqif"
    "silles"
    "wenqiny"
    "zhongnax")
  "list of andrewids in the group")









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

(defun kitchingroup-list-deadlines ()
  "Show an agenda of deadlines for the next 30 days from the current file."
  (interactive)
  (let ((org-agenda-custom-commands))
    (setq org-agenda-custom-commands
          '(("d" "Upcoming deadlines" agenda ""
             ((org-agenda-time-grid nil)
              (org-deadline-warning-days 30)
              (org-agenda-entry-types '(:deadline))))))
    (org-agenda "" "d" "<")))
