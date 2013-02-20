;; emacs-lisp code to convert org to jekyll posts.  The YAML header is
;; constructed from properties of the heading. The title of the post
;; comes from the heading. 

(require 'org-e-html)

(defvar jblog-image-directory "~/Dropbox/jkitchin.github.com/source/images/"
  "Directory where all images are stored.")

(defvar jblog-posts-directory "~/Dropbox/jkitchin.github.com/source/_posts/"
  "Directory where all html versions of posts are stored.")

(defvar jblog-blog-directory "~/Dropbox/jkitchin.github.com/source/blog/"
  "Directory where jekyll puts blog posts. We copy org files here.")

(defvar jblog-url-base "http://jkitchin.github.com/"
  "Base url of the whole site")

(defvar jblog-image-url-base "http://jkitchin.github.com/images/"
  "Base url where images are found")

(defun jblog-get-post-title ()
  "return title property or heading of current section with
properties stripped"
  (interactive)
  (let ((title (if (org-entry-get nil "title")
		   (org-entry-get nil "title") ; prefer title property
		 ;; or get heading with no tags or todos
		 (org-get-heading t t))))
    ; strip the text properties.
    (set-text-properties 0 (length title) nil title)
    (format "%s" title)))

(defun jblog-get-YAML-heading ()
  "returns a string of the YAML heading from the current section

all keys and values are taken from properties."
  (interactive)
  
  ;; first we get the properties from the entry
  (setq title (jblog-get-post-title))
  (setq date (if (org-entry-get nil "date")    ; has a date property
		 (org-entry-get nil "date")    ; then
	       (format-time-string "%Y-%m-%d") ; else
	       ))
  
  (setq layout (if (org-entry-get nil "layout")    
		   (org-entry-get nil "layout")    ;then
		 (format "post") ; else use post as default layout
		 ))
  
  ;; the property is a string
  (setq categories (if (org-entry-get nil "categories")    
		       (org-entry-get nil "categories")    ;then
		     '())) ; else

  ;; tags this way is a list
  (setq tags (if (org-get-tags-at) 
		 (format "[%s]" (mapconcat 'identity (org-get-tags-at) ","))
	;else
	'()))
  
  ;; now construct the YAML header
  (mapconcat 'identity
	     `("---"
	       ,(concat "layout: " layout)
	       ,(concat "title: " title)
	       ,(concat "date: " date)
	       ,(concat "categories: " categories)
	       ,(concat "tags: " tags)
	       "---"
	       "" "" "")
	     "\n"))

(defun jblog-get-HTML () 
  "use the new org export mechanism to get html string with corrected image links. This function does not save the post!

the creates a buffer *Org E-HTML Export* with the content in it."
  (interactive)
  ;; temporarily replace org-e-html-format-inline-image so it puts the
  ;; right links to images in. in the blog, the link is a full URL, not a path
  (flet ((org-e-html-format-inline-image 
	  (src &optional
	       caption 
	       label 
	       attr 
	       standalone-p)
	  (progn
	    ;; src contains the image filename
	    ;; copy image to images/src
	    (let ((file src)
		  (newname (concat jblog-image-directory src))
		  (ok-if-already-exists t))
	      ;; create parent directories
	      (when (not (file-directory-p (file-name-directory newname)))
		(message (format 
			  "%s does not exist. Making it!" 
			  (file-name-directory newname)))
		(make-directory (file-name-directory newname) t))
	      (copy-file file newname ok-if-already-exists))
		
	    ;; construct url that is returned
	    (format "<p><img src=\"%s\"><p>" (concat jblog-image-url-base src)))))
	  
    ;; body in flet with temporary image function
    (let ((subtreep t)
	  (visible-only nil)
	  (body-only t)
	  (ext-plist '()))
      (org-e-html-export-as-html subtreep visible-only body-only ext-plist)))
  ;; now switch to the *Org E-HTML Export* buffer and get the string
  (setq cb (current-buffer))
  (set-buffer "*Org E-HTML Export*")
  (setq output (buffer-string))
  (set-buffer cb)
  (format "%s" output))

(defun jblog-get-post-html ()
  "Return a string containing the YAML header, the post html, my copyright line, and a link to the org-source code."
  (interactive)
  (let ((url-to-org (jblog-get-url-to-org-source))
	(yaml (jblog-get-YAML-heading))
	(body (jblog-get-HTML)))
	
    (with-temp-buffer
      (insert yaml)
      (insert body)
      (insert 
       (format "<p>Copyright (C) %s by John Kitchin. See the <a href=\"/copying.html\">License</a> for information about copying.<p>" 
	       (format-time-string "%Y")))
      (insert (format "<p><a href=\"%s\">org-mode source</a><p>"
		      url-to-org))
      (buffer-string))))

(defun jblog-get-post-filename ()
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
      (setq date (format-time-string "%Y-%m-%d"))
      (org-entry-put nil "date" date)))
  
  ;; we need to get the title
  (setq title (jblog-get-post-title))
    
  ;; construct the filename
  (format "%s" (concat jblog-posts-directory
		       date 
		       "-" 
		       (mapconcat 'identity (split-string title) "-")
		       ".html")))

(defun jblog-get-url-to-org-source ()
  "return the url of the full path on blogsite to the org-source code"
  (interactive)
  (setq title (jblog-get-post-title))
  (setq date (if (org-entry-get nil "date")    ; has a date property
		 (org-entry-get nil "date")    ; then
	       (format-time-string "%Y-%m-%d"))) ; else
	       
  ;; this is the URL to where the org file will be on the blog
  (setq path (concat jblog-url-base 
		     "blog/" 
		     (mapconcat 'identity (split-string date "-") "/")))

  (concat path "/" (mapconcat 'identity (split-string title) "-")  ".org"))

(defun jblog-copy-org-to-blog ()
  "copy the org-file to blog/year/month/day/title.org"
  (interactive)
  (setq title (jblog-get-post-title))
  (setq date (if (org-entry-get nil "date")    ; has a date property
		 (org-entry-get nil "date")    ; then
	       (format-time-string "%Y-%m-%d") ; else
	       ))
  ;; now make directories to contain the org file. The org-source is saved at
  ;; /blog/year/month/day/post.org
  (setq path (concat jblog-blog-directory 
		     (mapconcat 'identity (split-string date "-") "/")))
  (make-directory path t)
  ; now we need to save the org file
  ; first we get a filename with complete path
  (setq org-file (concat jblog-blog-directory 
			 (mapconcat 'identity (split-string date "-") "/")
			 "/"
			 (mapconcat 'identity (split-string title) "-")
			 ".org"))

  ;; we only write out the current subtree
  (org-narrow-to-subtree)
  (let ((contents (buffer-string)))
    (with-temp-file org-file
      (insert contents)))      
  (widen))

(defun jblog-blogpost ()
  "post the current heading in _posts"
  (interactive)
  ;; we have to get all data before we put the post together. these
  ;; functions all rely on point being in an org section
  (setq post-filename (jblog-get-post-filename))
  (org-entry-put nil "last-published" (format-time-string "%Y-%m-%d"))
  (save-buffer)
  (jblog-copy-org-to-blog)
  (let ((content (jblog-get-post-html)))
    (with-temp-file post-filename 
      (insert content)))
  ;; clean up
  (kill-buffer "*Org E-HTML Export*"))

(defun jblog-generate-blog ()
  (interactive)
  ; should probably change to a directory
  (shell-command "rake generate"))

(defun jblog-deploy-blog ()
  (interactive)
  ; should probably change to a directory
  (shell-command "rake deploy"))

(defun j-generate-and-publish-blog () 
  (interactive) 
  ; should probably change to a directory
  (jblog-generate-blog)
  (jblog-deploy-blog))
