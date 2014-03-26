
;; gnus
(setq gnus-startup-file (concat starter-kit-dir "user/newsrc")
      gnus-select-method '(nntp "news.gmane.org"))

      ;; the next line reads local mail on gilgamesh
;      gnus-secondary-select-methods '((nnml "")))

;; save rss data here so it is available on other computers


;; http://www.xsteve.at/prg/gnus/
(setq gnus-directory (concat starter-kit-dir "user/News/"))
(setq message-directory (concat gnus-directory "mail")
      nnml-directory (concat gnus-directory "nnml-mail")
      gnus-article-save-directory (concat gnus-directory "saved")
      gnus-kill-files-directory (concat gnus-directory "scores")
      gnus-cache-directory (concat gnus-directory "cache"))
;; for rss feeds
(setq nnrss-directory (concat gnus-directory  "rss"))
