;;; org-colored-text.el --- Colored text for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ov)

(org-add-link-type
 "color"
 (lambda (path)
   "No follow action.")
 (lambda (color description backend)
   (cond
    ((eq backend 'html)
     (let ((rgb (assoc color color-name-rgb-alist))
	   r g b)
       (if rgb
	   (progn
	     (setq r (* 255 (/ (nth 1 rgb) 65535.0))
		   g (* 255 (/ (nth 2 rgb) 65535.0))
		   b (* 255 (/ (nth 3 rgb) 65535.0)))
	     (format "<span style=\"color: rgb(%s,%s,%s)\">%s</span>"
		     (truncate r) (truncate g) (truncate b)
		     (or description color)))
	 (format "No Color RGB for %s" color)))))))

(defun next-color-link (limit)
  (when (re-search-forward
	 "color:[a-zA-Z]\\{2,\\}" limit t)
    (forward-char -2)
    (let* ((next-link (org-element-context))
	   color beg end post-blanks)
      (if next-link
	  (progn
	    (setq color (org-element-property :path next-link)
		  beg (org-element-property :begin next-link)
		  end (org-element-property :end next-link)
		  post-blanks (org-element-property :post-blank next-link))
	    (set-match-data
	     (list beg
		   (- end post-blanks)))
	    (ov-clear beg end 'color)
	    (ov beg
		(- end post-blanks)
	     'color t
	     'face
	     `((:foreground ,color)))
	    (goto-char end))
	(goto-char limit)
	nil))))


(add-hook 'org-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '((next-color-link (0 'org-link t)))
	     t)))


(provide 'org-colored-text)
;;; org-colored-text.el ends here
