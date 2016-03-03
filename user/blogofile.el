;; emacs-lisp code to prepare blogofile posts. The YAML header is
;; constructed from properties of the heading. The title of the post
;; comes from the heading.

;; John Kitchin

(require 'ox-html)

(defvar bf-root-dir "~/blogofile-jkitchin.github.com/"
  "Directory where all the files are stored")

(defvar bf-blog-directory (concat bf-root-dir "org/")
  "We copy org files here.")

(defvar bf-image-directory (concat bf-root-dir "img/")
  "Directory where all images are stored. as of 9/29/2013 this is not used anymore")

(defvar bf-media-directory (concat bf-root-dir "media/")
  "Directory where all media are stored. as of 9/29/2013 all images are stored in here")

(defvar bf-posts-directory (concat bf-root-dir "_posts/")
  "Directory where all html versions of posts are stored.")

(defvar bf-url-base "http://jkitchin.github.io/"
  "Base url of the whole site")

(defvar bf-image-url-base "/img/"
  "Base url where images are found")

(defvar bf-media-url-base "/media/"
  "Base url where media (files linked to including images, data, etc...) are found")

(defun bf-get-post-title ()
  "return title property or heading of current section with
properties stripped"
  (interactive)
  (let ((title (if (org-entry-get nil "title")
		   (org-entry-get nil "title") ; prefer title property
		 ;; or get heading with no tags or todos
		 (org-get-heading t t))))
    (substring-no-properties title)))

(defun bf-get-YAML-heading ()
  "returns a string of the YAML heading from the current section

all keys and values are taken from properties."
  (interactive)

  ;; first we get the properties from the entry
  (setq title (bf-get-post-title))

  (setq date (if (org-entry-get nil "date")    ; has a date property
		 (org-entry-get nil "date")    ; then
	       (format-time-string "%Y/%m/%d %H:%M:%S") ; else current time
	       ))

  (setq updated (if (org-entry-get nil "updated")    ; has a date property
		 (org-entry-get nil "updated")    ; then
	       (format-time-string "%Y/%m/%d %H:%M:%S") ; else current time
	       ))

  ;; the property is a string
  (setq categories (if (org-entry-get nil "categories")
		       (org-entry-get nil "categories")    ;then
		     '())) ; else

  ;; tags this way is a list
  (setq tags (if (org-entry-get nil "tags")
		 (org-entry-get nil "tags")
	       ;; else
	       '()))

  ;; now construct the YAML header
  (mapconcat 'identity
	     `("---"
	       ,(concat "title: " title)
	       ,(concat "date: " date)
	       ,(concat "updated: " updated)
	       ,(if 'categories (concat "categories: " categories))
	       ,(if 'tags (concat "tags: " tags))
	       "---"
	       "" "" "")
	     "\n"))

(defun bf-get-post-media-folder ()
  "compute media folder name, create it if needed, and return the path"
  (interactive)
  (message "making media folder")
  (if (org-entry-get nil "date")
      (setq date (org-entry-get nil "date")) ; then
    ;; if there is no date property, we set date to current date and set the property
    (progn                                   ; else
      (setq date (format-time-string "%Y/%m/%d %H:%M:%S"))
      (org-entry-put nil "date" date)))

  ;; we need to get the title
  (setq title (bf-get-post-title))

  ;; construct the directory to save in
  (setq post-dir (format "%s" (concat (mapconcat 'identity (bf-parse-date date) "-")
                                 "-"
                                 (mapconcat 'identity (split-string title) "-")
                                 "/")))

  (setq media-dir (format "%s" (concat bf-media-directory post-dir)))
  (setq media-url (format "%s" (concat bf-media-url-base post-dir)))


  (make-directory media-dir t) ; make if needed
  `(,media-dir ,media-url))


(defun bf-get-link-urls ()
  "Parse the org-buffer and generate a list of urls for each link. Copy files as needed to where they need to go. Return the list of urls. This does not work for urls in footnotes."
  (interactive)
  (message "getting links")
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((parsetree (org-element-parse-buffer))
	   (counter 0)
	   (MD (bf-get-post-media-folder))
	   (media-dir (nth 0 MD))
	   (media-url (nth 1 MD)))
      ;; parse tree
      (setq url-list (org-element-map parsetree 'link
		       (lambda (link)
			 (message "%s" link)
			 (let* ((type (nth 0 link))
				(plist (nth 1 link))
				(content (nth 2 link))
				(path (plist-get plist :path))
				(type (plist-get plist ':type))
				(fname (car (last (split-string path "/")))))
			   (message "type=%s" type)
			   (message "content=%s" content)
			   (message "path=%s" path)
			   (message "fname=%s" fname)
			   ;; construct urls for different types of links
			   ;; "fuzzy" links are not working. I think these are internal links.
			   (cond
			    ;; image
			    ((and (string= type "file")
				  (file-name-extension fname)
				  (string-match
				   "png"
				   (file-name-extension fname)))
			     (progn
			       (copy-file path (concat media-dir fname) t)
			       (format "<img src=\"%s%s\"> "
				       media-url fname)))
			    ;; regular file with content
			    ((and (string= type "file")
				  content (file-exists-p fname))
			     ;; (message-box "file+content")
			     (progn (copy-file path (concat media-dir fname) t)
				    (format "<a href=\"%s%s\">%s</a> "
					    media-url fname content)))
			    ;; regular file with no content
			    ((and (string= type "file")
				  (file-exists-p fname))
			     ;; (message-box "file no content")
			     (progn (copy-file path (concat media-dir fname) t)
				    (format "<a href=\"%s%s\">%s</a> "
					    media-url fname fname)))
			    ;; URLS with content
			    ((and (string-match "http" type) content)
			     (format "<a href=\"%s\">%s</a> "
				     (plist-get plist :raw-link) content))
			    ;; urls with no content
			    ((string-match "http" type)
			     (format "<a href=\"%s\">%s</a> "
				     (plist-get plist :raw-link)
				     (plist-get plist :raw-link)))
			    ;; URLS with content
			    ((and (string-match "https" type) content)
			     (format "<a href=\"%s\">%s</a> "
				     (plist-get plist :raw-link) content))
			    ;; urls with no content
			    ((string-match "https" type)
			     (format "<a href=\"%s\">%s</a> "
				     (plist-get plist :raw-link)
				     (plist-get plist :raw-link)))
			    ;; no formatting for other types
			    (t
			     ;; (message-box "other type: %s" link)
			     "nil"))))))
      (widen))
    (message "done with url-list")
    url-list))

(defun ox-mrkup-filter-link (text back-end info)
  "filter links to return the URLs I need for blogofile. You need to have created bf-link-counter and url-list"
  (let ((url (nth  bf-link-counter url-list)))
    (if (string= url "nil")
	(setq output text) ; no change
      (setq output (format "%s" url)))

    ;; increment counter
    (setq bf-link-counter (+ bf-link-counter 1))
    output))

(defun bf-get-HTML ()
  (let (			 ;(bibliography (org-ref-get-html-bibliography))
	(org-export-filter-link-functions '(ox-mrkup-filter-link))
	(async nil)
	(subtreep t)
	(visible-only nil)
	(body-only t)
	(ext-plist '())
	(bf-link-counter 0)
	(url-list (bf-get-link-urls))
	(html))
    (org-html-export-as-html async subtreep visible-only body-only ext-plist)
    ;; now get the output into the org output
    (switch-to-buffer "*Org HTML Export*")
    (end-of-buffer)
    (setq html (buffer-string))
    (kill-buffer "*Org HTML Export*")
    html))

(require 'browse-url)
;; (defun bf-copyright (url-to-org)
;;   (format "<p>Copyright (C) %s by John Kitchin. See the <a href=\"/copying.html\">License</a> for information about copying.<p>
;; <p><a href=\"%s\">org-mode source</a></p>
;; <p>Org-mode version = %s</p>"
;;	  (format-time-string "%Y")
;;	  url-to-org
;;	  (org-version)))

(defun bf-copyright ()
  (format "<p>Copyright (C) %s by John Kitchin. See the <a href=\"/copying.html\">License</a> for information about copying.<p>
<p>Org-mode version = %s</p>"
	  (format-time-string "%Y")
	  (org-version)))

(defun bf-get-post-html ()
  "Return a string containing the YAML header, the post html, my copyright line, and a link to the org-source code."
  (interactive)
  (let ((url-to-org (bf-get-url-to-org-source))
	(yaml (bf-get-YAML-heading))
	(body (bf-get-HTML)))

    (with-temp-buffer
      (insert yaml)
      (insert body)
      (insert (format "<p>Copyright (C) %s by John Kitchin. See the <a href=\"/copying.html\">License</a> for information about copying.<p>
<p><a href=\"%s\">org-mode source</a></p>
<p>Org-mode version = %s</p>"
		      (format-time-string "%Y")
		      url-to-org
		      (org-version)))
      (buffer-string))))

(defun bf-get-permalink ()
  "return permalink for the post"
  (interactive)
    (if (org-entry-get nil "date")
      (setq date (org-entry-get nil "date")) ; then
    ;; if there is no date property, we set date to current date and set the property
    (progn                                   ; else
      (setq date (format-time-string "%Y/%m/%d %H:%M:%S"))
      (org-entry-put nil "date" date)))

  ;; we need to get the title
  (setq title (bf-get-post-title))

  ;; construct the filename
  (setq permalink (format "%s" (concat bf-url-base
		       "blog/"
		       (mapconcat 'identity (bf-parse-date date) "/")
		       "/"
		       (let ((s (mapconcat 'identity (split-string title) "-")))
			 (concat (capitalize (substring s 0 1)) (substring s 1)))
		       "/index.html")))
  (org-entry-put nil "permalink" permalink)
  permalink)

(defun bf-get-post-filename ()
  "derive the filename of the html file for the post from the current heading

the filename has this form:
year-month-day-words-in-title.html

if there is a date property, we use that, assuming it is formatted as year-month-day
otherwise, we use the current date, and set a property for date.

this function does not reflect any directory organization in _org. It only creates a name.

For example, a heading in this file
_org/python/test.org

would lead to this post filename
2013-01-20-test.html
"
  (interactive)
  (if (org-entry-get nil "date")
      (setq date (org-entry-get nil "date")) ; then
    ;; if there is no date property, we set date to current date and set the property
    (progn                                   ; else
      (setq date (format-time-string "%Y/%m/%d %H:%M:%S"))
      (org-entry-put nil "date" date)))

  ;; we need to get the title
  (setq title (bf-get-post-title))

  ;; construct the filename
  (format "%s" (concat bf-posts-directory
		       (mapconcat 'identity (bf-parse-date date) "-")
		       "-"
		       (mapconcat 'identity (split-string title) "-")
		       ".html")))

(defun bf-get-url-to-org-source ()
  "return the url of the full path on blogsite to the org-source code. this should be /org/year/month/day/title.org"
  (interactive)
  (setq title (bf-get-post-title))
  (setq date (if (org-entry-get nil "date")    ; has a date property
		 (org-entry-get nil "date")    ; then
	       (format-time-string "%Y/%m/%d %H:%M:%S"))) ; else

  ;; this is the URL to where the org file will be on the blog
  (setq path (concat "/org/"
		     (mapconcat 'identity (bf-parse-date date) "/")))

  (concat path
	  "/"
	  (mapconcat 'identity (split-string title) "-")
	  ".org"))

(defun bf-parse-date (datestring)
  "get year, month and day out of date property"
  (let ((date (car (split-string datestring " "))))
    (split-string date "/")))


(defun bf-copy-org-to-blog ()
  "copy the org-file to blog/year/month/day/title.org"
  (interactive)
  (setq title (bf-get-post-title))
  (setq date (if (org-entry-get nil "date")    ; has a date property
		 (org-entry-get nil "date")    ; then
	       (format-time-string "%Y/%m/%d") ; else
	       ))
  ;; now make directories to contain the org file. The org-source is saved at
  ;; /org/year/month/day/post.org
  (setq path (concat bf-blog-directory
		     (mapconcat 'identity (bf-parse-date date) "/")))

  (make-directory path t)
  ; now we need to save the org file
  ; first we get a filename with complete path
  (setq org-file (concat bf-blog-directory
			 (mapconcat 'identity (bf-parse-date date) "/")
			 "/"
			 (mapconcat 'identity (split-string title) "-")
			 ".org"))

  ;; we only write out the current subtree
  (save-restriction
  (org-narrow-to-subtree)
  (let ((contents (buffer-string)))
    (with-temp-file org-file
      (insert contents)))
  (widen)))

(defun bf-blogpost ()
  "post the current heading in _posts"
  (interactive)
  ;; we have to get all data before we put the post together. these
  ;; functions all rely on point being in an org section
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (setq thisb (current-buffer))
      (setq post-filename (bf-get-post-filename))
      (org-entry-put nil "updated" (format-time-string "%Y/%m/%d %H:%M:%S"))
      (save-buffer)
      (bf-copy-org-to-blog)
      (let ((content (bf-get-post-html)))
	(with-temp-file post-filename
	  (insert content)))
      ;; clean up

      (switch-to-buffer thisb)
      (goto-char (point-min))		; beginning of buffer
      (widen)))

  (org-todo 'done)			; should mark headline done
  ;; now build and deploy
  (with-current-directory
   "~/blogofile-jkitchin.github.com"
   (shell-command "make build && make deploy")))


(global-set-key [f8] 'bf-blogpost)
