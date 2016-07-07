;;; nikola.el --- HTML export for the Nikola blog

;;; Commentary:
;; Goal: export org-mode headings to an html post
;;
;; Create the html post, and a directory containing any images/files
;; and the org-source.

(require 'ox-html)

(defvar nikola-root "/home-research/jkitchin/dft-book/blog/source/"
  "Directory where Nikola blog is stored.")

(defvar nikola-posts-directory (concat nikola-root "posts/")
  "Directory where posts are stored.")

(defvar nikola-files-directory (concat nikola-root "files/media/")
  "Directory where post files will go.
Each post gets its own directory in here.")

;; Here is what we are trying to get
;; <!--
;; .. title: Second post
;; .. slug: second-post
;; .. date: 2016-05-23 21:10:00 UTC-04:00
;; .. tags:
;; .. category:
;; .. link:
;; .. description:
;; .. type: text
;; -->

;; Write your post here.

(defun nikola-title ()
  "Get post title from org-heading."
  (substring-no-properties (nth 4 (org-heading-components))))


(defun nikola-slug ()
  "Slugify the title.
Downcases all words, and replaces spaces with -."
  (s-join "-" (mapcar 'downcase (s-split " " (nikola-title) t))))


(defun nikola-post-files-dir ()
  "Return directory to put files in for the post."
  (let ((dir (file-name-as-directory (concat nikola-files-directory
					     (nikola-slug)))))
    (make-directory dir t)
    dir))


(defun nikola-date ()
  "Return formatted date string."
  (format-time-string "%Y-%m-%d %H:%M:%S"))


(defun nikola-tags ()
  "Get tags on entry."
  (s-join
   ","
   (append
    (org-get-tags-at)
    (when (> (length (org-element-map (org-element-parse-buffer) 'latex-fragment 'identity))
	     0)
      '("mathjax")))))


(defun nikola-categories ()
  "Get categories from heading."
  (org-entry-get (point) "categories"))

(defun nikola-link-processor (tree backend info)
  "Copies link files to the right place and changes path to point to them.
Note you cannot call many nikola- functions in here because the buffer is not
right. There are some global variables to handle that."

  (org-element-map tree 'link
    (lambda (link)
      (when (string= "file" (org-element-property :type link))
	(let* ((path (org-element-property :path link)) ; original path
	       (fname (file-name-nondirectory path)) ; plain filename
	       (new-fname (expand-file-name fname *link-dir*))
	       (new-path (format  "/media/%s/%s" *slug* fname)))

	  (copy-file path new-fname t)
	  (org-element-put-property link :path new-path)))))
  tree)

(defun nikola-link-filter (text backend info)
  "Remove file: from image links.
I need an absolute path for images, but org-mode puts a file: in
the link then. We remove it here."
  (if (string-match "^<img" text)
      (replace-regexp-in-string "file:" "" text)
    text))

(defun nikola-body-html ()
  "Get HTML of body.
We need a filter to handle links."
  (save-excursion
    (let ((*link-dir* (nikola-post-files-dir))
	  (*slug* (nikola-slug))
	  (org-export-filter-parse-tree-functions '(nikola-link-processor))
	  (org-export-filter-link-functions '(nikola-link-filter))
	  (async nil)
	  (subtreep t)
	  (visible-only nil)
	  (body-only t)
	  (ext-plist '())
	  (html))
      (org-html-export-as-html async subtreep visible-only body-only ext-plist)
      ;; now get the output into the org output
      (setq html (with-current-buffer "*Org HTML Export*" (buffer-string)))
      (kill-buffer "*Org HTML Export*")
      html)))


(defun nikola-post-html ()
  "Get post-html."
  (format "<!--
.. title: %s
.. slug: %s
.. date: %s
.. tags: %s
.. category: %s
.. link:
.. description:
.. type: text
-->

%s"
	  (or (nikola-title) "")
	  (or (nikola-slug) "")
	  (or (nikola-date) "")
	  (or (nikola-tags) "")
	  (or (nikola-categories) "")
	  (or (nikola-body-html) "")))


(defun nikola-post-filename ()
  "Returns post filename."
  (concat nikola-posts-directory (nikola-slug) ".html"))


(defun nikola-write-post ()
  "Writes post to file."
  (interactive)
  (let* ((fdir (file-name-as-directory (nikola-slug)))
	 (org-file (expand-file-name
		    (concat (nikola-slug) ".org") ; org-filename
		    (expand-file-name (nikola-slug)
				      nikola-files-directory)))
	 (org-link (format "/media/%s/%s"
			   (nikola-slug)
			   (concat (nikola-slug) ".org")))
	 (org-src (save-restriction
		    (org-narrow-to-subtree)
		    (buffer-string)))
	 ;; these go last because the mess with the buffer.
	 (fname (nikola-post-filename))
	 (html (nikola-post-html)))


    (with-temp-file fname
      (insert html)
      (insert (format "
<a href=\"%s\">org-source</a>"
		      org-link)))

    ;; make file directories
    (with-temp-file org-file
      (insert org-src))
    (org-todo "DONE")
    (org-entry-put
     (point)
     "POSTED"
     (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun nikola-deploy ()
  "Write post, build and deploy."
  (interactive)
  (save-buffer)
  (nikola-write-post)
  (shell-command "nikola build")
  (shell-command "nikola github_deploy"))

(provide 'nikola)

;;; nikola.el ends here
