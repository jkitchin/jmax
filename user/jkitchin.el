(require 'ox-cmu-memo)

(defvar kitchingroup-root "~/Dropbox/kitchingroup"
  "root directory where kitchingroup is")

(defvar kitchingroup
  '("ahallenb"
    "hthiruma"
    "jboes"
    "jdmichae"
    "mcurnan"
    "mehakc"
    "meihengl"
    "ngovinda"
    "siddhard"
    "vnaik"
    "wenqiny"
    "zhaofenc"
    "zhongnax")
  "list of andrewids in the group")

(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

;; [[http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/][A peek at Emacs 24.4: Focus Hooks - Emacs Redux]]
;; save all buffers when you lose emacs focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

; (load-theme 'leuven)

(when (or (string= system-name "Johns-MacBook-Air.local")
	  (string= system-name "johns-air.wv.cc.cmu.edu"))
  (set-face-attribute 'default nil :height 125))

;; set i(a)spell options on different machines
(setq ispell-personal-dictionary (concat starter-kit-dir "user/.ispell"))
(when (string= system-name "gilgamesh.cheme.cmu.edu")
  (setq-default ispell-program-name "aspell"))

(when (or (string= system-name "JKITCHIN-PC")
	  (string= system-name "CHEME-PC")
	  (string= system-name "KITCHINX61TABLE")
	  (string= system-name "KITCHIN-TABLET"))
  (setq-default ispell-program-name "C:/Program Files/Aspell/bin/aspell.exe"))

(when	  (string= system-name "JKITCHIN-2012")
  (setq-default ispell-program-name "c:/Program Files (x86)/Aspell/bin/aspell.exe"))

;; (ignore-errors
;;   (require 'emacs-xkcd)
;;   (setq xkcd-cache-dir "~/Dropbox/kitchingroup/jmax/user/xkcd/")
;;   (xkcd-get-latest))


;; modified from http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
(defvar my-filelist nil
  "alist for files I need to open frequently. Key is a short abbrev, Value is file path. Uses completion.")

(setq my-filelist
      '(
        ("master" . "~/Dropbox/org-mode/master.org")
        (".emacs.d" . "~/Dropbox/kitchingroup/jmax" )
        ("blog" . "~/blogofile-jkitchin.github.com/_blog/blog.org")
        ("ese" . "~/Dropbox/books/ese-book/ese.org" )
	("passwords" . "~/Dropbox/org-mode/passwords.org.gpg")
        ("Pycse" . "~/Dropbox/books/pycse/pycse.org")
        ("references" . "~/Dropbox/bibliography/references.bib")
        ("notes" . "~/Dropbox/bibliography/notes.org")
        ("journal" . "~/Dropbox/org-mode/journal.org")
        ("tasks" . "~/Dropbox/org-mode/tasks.org")
        ;; more here
        ))

(defun my-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set in `my-filelist."
  (interactive
   (list (ido-completing-read
	  "Open:"
	  (mapcar (lambda (x) (car x)) my-filelist))))
  (find-file (cdr (assoc openCode my-filelist))))

(global-set-key [f9] 'my-open-file-fast)

(global-set-key [f10] 'org-ref-open-bibtex-notes)
(global-set-key [f11] 'org-ref-open-bibtex-pdf)
(global-set-key [f12] 'org-ref-open-in-browser)


(setq org-default-notes-file "~/Dropbox/org-mode/notes.org")

;; see http://orgmode.org/manual/Template-elements.html#Template-elements
;; (add-hook 'org-capture-after-finalize-hook 'org-capture-goto-last-stored)
(setq org-capture-templates
      '(
        ("t"   ; key
         "Todo"; description
         entry ; type
         (file "~/Dropbox/org-mode/tasks.org") ;target
         "* TODO %?\n  %i\n  %a")
	;; ; a message in mu4e
	("r"
	 "Reply To:"
	 entry
	 (file "~/Dropbox/org-mode/tasks.org")
	 "* TODO Reply to %:from on %:subject
     SCHEDULED: %t
Captured on %U
link to message: %a\n")

        ("g" "TODO from gnus" entry (file "~/Dropbox/org-mode/gnus.org")
         "* GNUS %:subject
  DEADLINE: %t

 \nLink: %a\n")
	("e" "TODO from gnus email" entry (file "~/Dropbox/org-mode/email.org")
         "* TODO EMAIL %:subject
  DEADLINE: %t
TO:   %:to
DATE: %:date

\nLink: %a\n

%?")


        ("j" "Journal" entry (file+datetree "~/Dropbox/org-mode/journal.org" "Journal")
         "* %?\nEntered on %U\n  %i\n  %a")

        ("c" "Contacts" entry (file "~/Dropbox/org-mode/contacts.org")
         "* %(org-contacts-template-name)
    :PROPERTIES:
    :EMAIL: %(org-contacts-template-email)
    :PHONE:
    :ALIAS:
    :NICKNAME:
    :IGNORE:
    :ICON:
    :NOTE:
    :ADDRESS:
    :BIRTHDAY:
    :END:")))


(add-to-list
 'org-capture-templates
 '("e" "Elfeed" entry (file "~/Dropbox/org-mode/elfeed.org")
   "*  %:description
  DEADLINE: %t

 \nLink: %a\n\n"))

;; These are the majority of my agenda files
(setq org-agenda-files '("~/Dropbox/org-mode"))

;; My student files
(setq org-agenda-files (append
                        org-agenda-files
			(mapcar (lambda (x) (format "~/Dropbox/kitchingroup/students/%s/%s.org" x x)) kitchingroup)))


;; push string to clipboard
(defun j-copy-to-clipboard (arg)
  "copy arg  to clipboard. args are prestored and completed."
  (interactive
   (list (ido-completing-read "Copy:"
			      (mapcar (lambda (x) (car x))
				      '(("calendar"  . "http://bit.ly/jkitchin-calendar")
					("discretionary acct" . "Oracle string: 14552.1.5001110"))))))

  (interactive)
  (with-temp-buffer
    (insert (cdr (assoc arg my-strings)))
    (kill-ring-save (point-min) (point-max))))

(require 'reftex)
(require 'reftex-cite)

;; I like a random bibtex entry in my agenda.
(defun formatted-bibtex-entry ()
  "return a bibtex entry as a formatted string. I hand-built the format.

this treats all entries as a journal article."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((bibtex-expand-strings t)
         (entry (bibtex-parse-entry t))
         (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
         (year  (reftex-get-bib-field "year" entry))
         (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
         (key (reftex-get-bib-field "=key=" entry))
         (journal (reftex-get-bib-field "journal" entry))
         (volume (reftex-get-bib-field "volume" entry))
         (issue (reftex-get-bib-field "issue" entry))
         (pages (reftex-get-bib-field "pages" entry))
         (doi (reftex-get-bib-field "doi" entry))
         (output))
    (concat (when author author) ", "
                         (when title title) ", "
                         (when journal journal) ", "
                         (when volume volume)
                         (if issue (format "(%s), " issue) ", ")
                         (when pages pages) ", "
                         (when year (format "(%s)." year))
                         (when doi (format " http://dx.doi.org/%s" doi)))))

;; this returns a string for my agenda.
(defun get-random-bibtex-entry (&optional arg)
  "for printing in my agenda"
  (interactive)
  (let ((keys) (lucky-key) (output))
    (with-current-buffer
	(find-file (car reftex-default-bibliography))
      (setq keys (bibtex-parse-keys))
      (setq lucky-key (car
		       (nth
			(random (safe-length keys)) keys)))
      (goto-char (point-min))
      (re-search-forward lucky-key)
      (setq output (formatted-bibtex-entry))
      (kill-buffer)
      (format "%s\ncite:%s" output lucky-key))))

(setq initial-scratch-message (get-random-bibtex-entry))

;; function to open agenda
(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              )))
      (call-interactively 'org-agenda-list)))
  )

;; open agenda after 15 minutes of idle time
(run-with-idle-timer 900 t 'jump-to-org-agenda)


;; (require 'org-secretary)
(setq org-todo-keywords
       '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
         (sequence "GNUS(g)" "|" "DONE(d)")
         (sequence "TASK(f)" "|" "DONE(d)")))

;; It helps to distinguish them by color, like this:
;;
(setq org-todo-keyword-faces
       '(("TODO" . (:foreground "red" :weight bold))
         ("DONE" . (:foreground "ForestGreen"))
         ("GNUS" . (:foreground "RoyalBlue3"))
         ("TASK" . (:foreground "RoyalBlue3"))))



(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)



(setq org-contacts-files '("~/Dropbox/org-mode/contacts.org" "/Users/jkitchin/techela-admin/f14-06625/gitolite-admin/roster.org"))
(org-contacts-gnus-insinuate)




;; (add-to-list 'load-path (expand-file-name "user/gnorb/lisp" starter-kit-dir))

;; (add-to-list 'org-capture-templates '("r" "Work-related Reply" entry (file+headline "~/Dropbox/org-mode/email+gnus.org" "Emails")
;;   "** REPLY %:fromname\n%?Received on %:date-timestamp-inactive, link: %a"
;;   :gnus-attachments t))
;; (setq gnorb-gnus-new-todo-capture-key "r")


;; themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes/emacs-color-theme-solarized" starter-kit-dir))

(add-to-list 'custom-theme-load-path (expand-file-name "themes/replace-colorthemes" starter-kit-dir))

(add-to-list 'load-path (expand-file-name "themes/tomorrow-theme/Gnu Emacs" starter-kit-dir))

(add-to-list 'custom-theme-load-path (expand-file-name "themes/tomorrow-theme/Gnu Emacs" starter-kit-dir))



;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p global-abbrev-table local-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)


;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key


;; Not all super keys works since this is bound to the windows key.
;; maybe some of this can be fixed with autohotkey, but

;; bind hyper-e to execute a block.
(local-set-key (kbd "H-e") 'org-babel-execute-src-block)

;; on Mac, super is the same as command
(global-set-key (kbd "s-b") 'bury-buffer)



;; encryption
(require 'epa-file)
(unless (memq epa-file-handler file-name-handler-alist)
  (epa-file-enable))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "jkitchin@andrew.cmu.edu")




(defun jcs-get-link (link)
  "Retrieve URL from current Safari page and prompt for description.
Insert an Org link at point."
  (interactive "sLink Description: ")
  (let ((result (shell-command-to-string
		 "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
    (insert (format "[[%s][%s]]" (org-trim result) link))))

;; Spell-checking on the fly
(flyspell-mode +1)
(global-set-key (kbd "<f5>") 'flyspell-buffer)

(defun flyspell-check-next-highlighted-word ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "<f6>") 'flyspell-check-next-highlighted-word)
(global-set-key (kbd "C-<f6>") 'flyspell-check-previous-highlighted-word)

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)


;; this only works on linux for some reason
(setq initial-major-mode 'org-mode)
(switch-to-buffer "*scratch*")
(org-mode)


(setq org-tags-column -50)


(setq initial-frame-alist '((width . 100) (height . 54)))

(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe)
        (right-fringe)
        (tool-bar-lines . 0)
        (width . 100)
        (height . 52)
        ))


(org-add-link-type
 "orcid"
 (lambda (link-string)
   (browse-url
    (format "http://orcid.org/%s" link-string))))

(org-add-link-type
 "ResearcherID"
 (lambda (link-string)
   (browse-url
    (format "http://www.researcherid.com/rid/%s" link-string))))

(org-add-link-type
 "google"
 (lambda (link-string)
   (browse-url
    (format
     "http://www.google.com/search?q=%s"
     (url-hexify-string link-string)))))


(require 'twittering-mode)
(setq twittering-use-master-password t)


(setq helm-bibtex-bibliography "~/Dropbox/bibliography/references.bib")
(setq helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs")

(setq helm-bibtex-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

(setq helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

(defun helm-bibtex-format-org-ref (keys)
  "insert selected KEYS as cite link. Append KEYS if you are on a link."
  (let ((el (org-element-context)))
    (message "%s" el)
    (if (eq 'link (car el))
	(progn
	  (goto-char (org-element-property :end el))
	  (concat "," (s-join "," keys)))
      (concat "cite:" (s-join "," keys)))))

(setq helm-bibtex-format-citation-functions
      '((org-mode . helm-bibtex-format-org-ref)))



(defun hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :sources `(
		    ((name . "Mail and News")
		     (candidates . (("Mail" . (lambda ()
						(if (get-buffer "*mu4e-headers*")
						    (progn
						      (switch-to-buffer "*mu4e-headers*")
						      (delete-other-windows))

						  (mu4e))))
				    ("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
				    ("RSS" . elfeed)
				    ("Agenda" . (lambda () (org-agenda "" "w")))))
		     (action . (("Open" . (lambda (x) (funcall x))))))
		    ((name . "My Locations")
		    (candidates . (("master" . "~/Dropbox/org-mode/master.org")
				   (".emacs.d" . "~/Dropbox/kitchingroup/jmax" )
				   ("blog" . "~/blogofile-jkitchin.github.com/_blog/blog.org")
				   ("ese" . "~/Dropbox/books/ese-book/ese.org" )
				   ("passwords" . "~/Dropbox/org-mode/passwords.org.gpg")
				   ("Pycse" . "~/Dropbox/books/pycse/pycse.org")
				   ("references" . "~/Dropbox/bibliography/references.bib")
				   ("notes" . "~/Dropbox/bibliography/notes.org")
				   ("journal" . "~/Dropbox/org-mode/journal.org")
				   ("tasks" . "~/Dropbox/org-mode/tasks.org")))
		    (action . (("Open" . (lambda (x) (find-file x))))))

		   ((name . "My org files")
		    (candidates . ,(f-entries "~/Dropbox/org-mode"))
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   helm-source-bookmarks
		   helm-source-bookmark-set)))

(global-set-key [f9] 'hotspots)

;; [[https://github.com/grettke/home/blob/master/ALEC.txt][home/ALEC.txt at master · grettke/home]]
(setq org-catch-invisible-edits 'error)


;; try autocomplete again for emacs-lisp
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)

;; [[http://www.emacswiki.org/emacs/auto-capitalize.el][EmacsWiki: auto-capitalize.el]]

(autoload 'auto-capitalize-mode "auto-capitalize"
  "Toggle `auto-capitalize' minor mode in this buffer." t) ;
(autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
  "Turn on `auto-capitalize' minor mode in this buffer." t)
(autoload 'enable-auto-capitalize-mode "auto-capitalize"
  "Enable `auto-capitalize' minor mode in this buffer." t)

;; To turn on (unconditional) capitalization in all Text modes, add
;; this to your site-lisp/default.el or ~/.emacs file:
;; (add-hook 'text-mode-hook 'turn-on-auto-capitalize-mode)
;; To enable (interactive) capitalization in all Text modes, add this
;; to your site-lisp/default.el or ~/.emacs file:
(add-hook 'text-mode-hook 'enable-auto-capitalize-mode)

; https://github.com/abo-abo/worf/blob/master/worf.el
;(add-to-list 'load-path (expand-file-name "worf"))
;(require 'worf)



;; https://github.com/abo-abo/lispy#configuration-instructions
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;; [[http://emacsredux.com/blog/2013/06/21/eval-and-replace/][Eval and Replace - Emacs Redux]]
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


; adapted from [[https://vmtyler.com/applescript-markdown-ready-screenshots/][AppleScript Markdown-Ready Screenshots | VMTyler.com]]
(defun jmax-screenshot (fname &optional arg)
  "Prompt for a FNAME and then take a screenshot saved at that name.
Only works on Mac OSX."
  (interactive "FFilename:nP")
   (when arg
    (do-applescript "tell application \"System Events\" to keystroke \"z\" using {control down}"))
  (do-applescript
   (mapconcat
    'identity
    `(,(format "set screenshotFilePath to \"%s\"" (expand-file-name fname))
      "do shell script \"screencapture \" & \"-s\" & \" \" & quoted form of screenshotFilePath"
      ,(concat "set result to \"[[" fname "]]\"")
      "set the clipboard to result")
    "\n")
   )
  (insert (format "[[%s]]" fname)))


(defun jmax-snagit (arg)
  "Launch Snagit and take a screenshot. With prefix ARG, minimize Emacs first."
  (interactive "P")
  (when arg
    (do-applescript "tell application \"System Events\" to keystroke \"z\" using {control down}")
    ; sleep a little to give window time to disappear
    (sleep-for 1))
  (do-applescript
   "tell application \"Snagit\" to activate
tell application \"System Events\" to keystroke \"C\" using {shift down, control down}"))


;; https://github.com/dfeich/org-screenshot#sec-2
;; (load-file (expand-file-name
;;	    "user/org-screenshot/org-screenshot.el"
;;	    starter-kit-dir))
;; (setq org-screenshot-relative-links t)
;; (setq org-screenshot-command-line "screencapture -i %f")


(setq org-mac-grab-Mail-app-p nil
     org-mac-grab-Outlook-app-p nil
     org-mac-grab-Addressbook-app-p nil
     org-mac-grab-Firefox-app-p nil
     org-mac-grab-Firefox+Vimperator-p nil
     org-mac-grab-Skim-app-p nil
     org-mac-Skim-highlight-selection-p nil)
(require 'org-mac-link)
(defalias 'omgl 'org-mac-grab-link)


(setq jmax-user-theme 'leuven)


;; adapted from [[http://doc.rix.si/org/fsem.html][Hardcore Freestyle Emacs]]

;; (menu-bar-mode 1)
;; (tool-bar-mode 0)
;; (scroll-bar-mode 0)
(column-number-mode t)
(rainbow-mode 1)
(diminish 'rainbow-mode)
(global-auto-revert-mode t)

(global-linum-mode 0)
(global-whitespace-mode 0)
;; (global-relative-line-numbers-mode 0)
(global-hl-line-mode 0)
;; (column-highlight-mode 0)
(hl-line-mode 0)

(defun rrix/enable-hl-line ()
       (hl-line-mode 1))

(mapc (function (lambda (mode)
                 (message (symbol-name mode))
                 (add-hook mode 'rrix/enable-hl-line)))
      '(erc-mode-hook
        gnus-group-mode-hook
        gnus-summary-mode-hook
        org-agenda-mode-hook
        eshell-mode-hook))

(require 'diminish)
(diminish 'visual-line-mode "")

(global-visual-line-mode)
(setq-default fill-column 80
              whitespace-line-column 80)

(require 'whitespace)
(diminish 'whitespace-mode "ᗣ")
(diminish 'global-whitespace-mode "ᗣ")
(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(define-key global-map (kbd "RET") 'newline-and-indent)

(defun rrix/setup-text-mode ()
       "function that applies all of my text-mode customizations"
       (whitespace-mode 1))
(add-hook 'text-mode-hook 'rrix/setup-text-mode)

(setq whitespace-style '(indentation::space
                         space-after-tab
                         space-before-tab
                         trailing
;;                         lines-tail
                         tab-mark
                         face
                         tabs))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(setq linum-delay t
      linum-eager nil)
(add-hook 'prog-mode-hook '(lambda () (linum-mode 1)))

(require 'flymake)
(flymake-mode 0)
(diminish 'flymake-mode "")

(require 'hideshow)
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
(diminish 'hs-minor-mode "F")

(diminish 'abbrev-mode "Abrv")

(require 'git-messenger)
(add-hook 'prog-mode-hook (lambda ()
			    (local-set-key (kbd "C-c v p")
					   'git-messenger:popup-message)))

(setq elfeed-feeds
      '("http://feeds.feedburner.com/acs/accacs"
	"http://feeds.feedburner.com/acs/enfuem"
	"http://feeds.feedburner.com/acs/esthag"
	"http://feeds.feedburner.com/acs/jacsat"
	"http://feeds.feedburner.com/acs/jpcbfk"
	"http://feeds.feedburner.com/acs/jpccck"
	"http://feeds.feedburner.com/acs/jpclcd"
	"http://feeds.feedburner.com/acs/cmatex"
	"http://feeds.feedburner.com/acs/jctcce"
	"http://feeds.feedburner.com/acs/jcisd8"
	"http://feeds.feedburner.com/acs/iecred"
	"http://feeds.aps.org/rss/recent/prl.xml"
	"http://feeds.aps.org/rss/recent/prb.xml"
	"http://www.sciencemag.org/rss/current.xml"
	"http://feeds.nature.com/nature/rss/current"
	"http://feeds.nature.com/nmat/rss/current"
	"http://onlinelibrary.wiley.com/rss/journal/10.1002/(ISSN)1521-3773"
	"http://scitation.aip.org/rss/content/aip/journal/jcp/latestarticles;jsessionid=6k76xb11z253.x-aip-live-06?fmt=rss" ; J. Chem. Phys.
	"http://planet.emacsen.org/atom.xml"
	"http://pipes.yahoo.com/pipes/pipe.run?_id=41ff1c5d2d98c068757bc2648c93e23e&_render=rss"
	"http://planetpython.org/rss20.xml"
	"http://planet.scipy.org/rss20.xml"
	))


(setq elfeed-search-title-max-width 150)
(setq elfeed-search-trailing-width 30)
;; A snippet for periodic update for feeds (3 mins since Emacs start, then every
;; half hour):

;; start up elfeed and schedule it to update
(elfeed-update)
(run-at-time 180 1800 (lambda () (unless elfeed-waiting (elfeed-update))))


(require 'ace-isearch)
(global-ace-isearch-mode +1)

(setq paradox-github-token "b03b0a11823e204be4fedda922233e2a00f5ab96")


;; http://sachachua.com/blog/series/emacs-kaizen/
(require 'use-package)
(use-package ace-jump-zap
  :ensure ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))



(defun google-calendar-quick-add ()
  "Add appointement to google calendar. Use selection if active, else prompt"
  (interactive)
  (browse-url
   (format
    "http://www.google.com/calendar/event?ctext=+{%s}+&action=TEMPLATE&pprop=HowCreated%%3AQUICKA"
    (if (region-active-p)
	(buffer-substring (region-beginning) (region-end))
      (read-string "Title: ")))))


;;; http://oremacs.com/2015/01/14/repeatable-commands/
(defun def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST."
  (lexical-let ((keymap (make-sparse-keymap))
                (func (cdar alist)))
    (mapc (lambda (x)
            (define-key keymap (car x) (cdr x)))
          alist)
    (lambda (arg)
      (interactive "p")
      (funcall func arg)
      (set-transient-map keymap t))))

(global-set-key (kbd "<f2> g")
                (def-rep-command
                    '(("g" . text-scale-increase)
                      ("l" . text-scale-decrease))))

(global-set-key (kbd "<f2> l")
                (def-rep-command
                    '(("l" . text-scale-decrease)
                      ("g" . text-scale-increase))))



(setq diary-file (expand-file-name "user/diary" starter-kit-dir))
(setq diary-number-of-entries 5)	; 5 days of entries should be shown
(setq org-agenda-include-diary t)

(message "Done with jkitchin")
