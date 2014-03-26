(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see jorg-bib.el for use of these variables
(setq jorg-bib-bibliography-notes "~/Dropbox/bibliography/notes.org"
      jorg-bib-default-bibliography '("~/Dropbox/bibliography/references.bib")
      jorg-bib-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

;;Tell the program who you are and setup for email
(setq user-full-name "John Kitchin"
      andrewid "jkitchin"
      user-mail-address "jkitchin@andrew.cmu.edu"
      ;; specify how email is sent
      send-mail-function 'smtpmail-send-it
      ;; used in message mode
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.andrew.cmu.edu"
;      smtpmail-starttls-credentials `((,smtpmail-smtp-server 587 nil nil))
      smtpmail-smtp-service 587)

;; set i(a)spell options on different machines
(when (string= system-name "gilgamesh.cheme.cmu.edu")
  (setq-default ispell-program-name "aspell"
		ispell-personal-dictionary (concat starter-kit-dir "user/.ispell")))

(when (or (string= system-name "JKITCHIN-PC")
	  (string= system-name "KITCHIN-TABLET"))
  (setq-default ispell-program-name "C:/Program Files/Aspell/bin/aspell.exe"))


(require 'emacs-xkcd)

(setq xkcd-cache-dir "~/Dropbox/kitchingroup/jmax/user/xkcd/")
(xkcd-get-latest)


;; modified from http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
(defvar my-filelist nil 
  "alist for files I need to open frequently. Key is a short abbrev, Value is file path. Uses completion.")

(setq my-filelist
      '(
        ("master" . "~/Dropbox/org-mode/master.org")
        (".emacs.d" . "~/Dropbox/kitchingroup/jmax" )
        ("blog" . "~/Dropbox/blogofile-jkitchin.github.com/_blog/blog.org")
        ("ese" . "~/Dropbox/books/ese-book/ese.org" )
        ("pycse" . "~/Dropbox/books/pycse/pycse.org")
        ("references" . "~/Dropbox/bibliography/references.bib")
        ("notes" . "~/Dropbox/bibliography/notes.org")
        ("journal" . "~/Dropbox/org-mode/journal.org")
        ("tasks" . "~/Dropbox/org-mode/tasks.org")
        ;; more here
        ))

(defun my-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set in `my-filelist."
  (interactive
   (list (ido-completing-read "Open:" (mapcar (lambda (x) (car x)) my-filelist))))
  (find-file (cdr (assoc openCode my-filelist))))

(global-set-key [f9] 'my-open-file-fast)




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

        ("g" "TODO from gnus" entry (file "~/Dropbox/org-mode/gnus.org")
         "* GNUS %:subject
  DEADLINE: %t
  
 \nLink: %a\n")

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



(setq org-agenda-files '("~/Dropbox/org-mode"
                         "~/Dropbox/kitchingroup"))

(setq org-agenda-files (append 
                        org-agenda-files 
                        (file-expand-wildcards "~/Dropbox/kitchingroup/students/*/*.org")))
