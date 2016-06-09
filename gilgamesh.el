;; Helm interfaces to gilgamesh

(defun gilgamesh-net ()
  "Open buffer  with network traffic on nodes"
  (interactive)
  (let* ((results (split-string
		   (shell-command-to-string "beostat -R") "\n"))
	 (output '())
	 node rate)
    (dolist (line results)
      (when (string-match "= Node: \\(.*\\) (index" line)
	(setq node (match-string 1 line)))
      (when (string-match "Network rate \\([0-9]+\\) bytes/second" line)
	(setq rate (/ (string-to-number
		       (match-string 1 line)) (* 1024.0 1024.0)))
	(add-to-list 'output (cons node rate) t)))

    (switch-to-buffer "*gilgamesh node traffic*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (dolist (node output)
      (insert (format "%8s: %1.2f Mb/s\n"
		      (format "[[node:%s][%s]]"
			      (car node)
			      (car node))
		      (cdr node))))
    (setq buffer-read-only t)
    (use-local-map (copy-keymap text-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))))

(defun gilgamesh-pbsnodes (node)
  "Open buffer with output from pbsnodes on NODE"
  (interactive "sNode: ")
  (switch-to-buffer "*Node*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (shell-command-to-string (format "pbsnodes %s" node)))

   ;; now find jobs, and replace them with links.
  (goto-char (point-min))
  (let ((found-jobs '()))
    (while (re-search-forward "[0-9]*/\\([0-9]*.gilgamesh.cheme.cmu.edu\\)" nil t)
      (add-to-list 'found-jobs (match-string 1))
      (setf (buffer-substring (match-beginning 0) (match-end 0))
	    (format "[[job:%s][%s]]" (match-string 1) (match-string 0)))
      ;; go to end of link we just inserted to avoid finding this link again.
      (re-search-forward "]]"))
    (goto-char (point-max))
    (insert "
Unique jobs
==========\n")
    (loop for job in found-jobs
	  do

	  (insert (format "[[job:%s][%s]]\n%s\n\n" job job
			  (caddr (split-string (shell-command-to-string
						(format "qstat %s" job))
					       "\n"))))))
  (org-mode)
  (setq buffer-read-only t)
  (use-local-map (copy-keymap text-mode-map))
  (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))

(org-add-link-type
 "node"
 (lambda (link-string)
   (gilgamesh-pbsnodes link-string)))

(org-add-link-type
 "job"
 (lambda (jobid)
   (let ((choice (read-event (format "%s\n[i]nfo  [d]el: "
				     (shell-command-to-string
				      (format "qstat %s" jobid))))))
     (cond
      ((equal choice ?i)
       (qstat-f jobid))
       ;(shell-command (format "qstat -f %s" link-string)))
      ((equal choice ?d)
       (shell-command (format "qdel %s" jobid)))))))

(defun helm-job-candidates ()
  "Candidates from qstat"
  (loop for jobstring in (cddr (split-string (shell-command-to-string "qstat") "\n"))
	collect
	(cons jobstring (car (split-string jobstring)))))


(defun qstat-f (jobid)
  "Orgified output of qstat -f jobid"
  (interactive "sJobid: ")
  (switch-to-buffer "*qstat -f*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert
   (shell-command-to-string (format "qstat -f %s" jobid)))
  (goto-char (point-min))
  (when (re-search-forward "Error_Path = gilgamesh.cheme.cmu.edu:\\(.*\\s-*.*\\)" nil t)
    (setf (buffer-substring (match-beginning 0) (match-end 0))
	  (format "[[file:%s][%s]]"
		  (replace-regexp-in-string
		   "\\s-" "" (match-string 1))
		  (replace-regexp-in-string
		   "\\s-" "" (match-string 1))))
    (goto-char (point-min)))

    (when (re-search-forward "Output_Path = gilgamesh.cheme.cmu.edu:\\(.*\\s-*.*\\)" nil t)
      (setf (buffer-substring (match-beginning 0) (match-end 0))
	    (format "[[file:%s][%s]]"
		    (replace-regexp-in-string
		     "\\s-" "" (match-string 1))
		    (replace-regexp-in-string
		     "\\s-" "" (match-string 1))))
      (goto-char (point-min)))

    (when (re-search-forward "PBS_O_WORKDIR=\\(.*\\s-*.*\\)," nil t)
      (setf (buffer-substring (match-beginning 1) (match-end 1))
	    (format "[[file:%s][%s]] "
		    (replace-regexp-in-string
		     "\\s-" "" (match-string 1))
		    (replace-regexp-in-string
		     "\\s-" "" (match-string 1))))
      (goto-char (point-min)))

    (insert
     "[q] quit [h] hold [r] release hold [d] delete job [n] next job [p] previous job [Q] qstat\n\n")

    (org-mode)
    (setq buffer-read-only t)
    (use-local-map (copy-keymap text-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))
    (local-set-key "h" `(lambda () (interactive)
			 (helm-qhold-job ,jobid)
			 (qstat-f ,jobid)))
    (local-set-key "r" `(lambda () (interactive)
			 (helm-qrls-job ,jobid)
			 (qstat-f ,jobid)))
    (local-set-key "d" `(lambda ()
			 (interactive)
			 (helm-delete-job ,jobid)
			 (kill-buffer)))
    (local-set-key "n" `(lambda ()
			  "Go to next jobid"
			  (interactive)
			  ;; we use butlast to trim an empty line from the end of the output
			  (let* ((jobids (butlast
					  (loop for jobstring in (cddr (split-string (shell-command-to-string "qstat") "\n"))
						collect (car (split-string jobstring)))))
				 (i)
				 (next-i))
			    (setq i (-find-index (lambda (el) (string= el ,jobid)) jobids))
			    (setq next-i (if (or (null i) (= i (- (length jobids) 1)))
					     0
					   (+ 1 i)))
			    (qstat-f (nth next-i jobids)))))
    (local-set-key "p" `(lambda ()
			  "Go to previous jobid"
			  (interactive)
			  (let* ((jobids (butlast (loop for jobstring in (cddr (split-string (shell-command-to-string "qstat") "\n"))
							collect (car (split-string jobstring)))))
				 (i)
				 (next-i))
			    (setq i (-find-index (lambda (el) (string= el ,jobid)) jobids))
			    (setq next-i (if (or (= i 0) (null i))
					     (- (length jobids) 1)
					   (- i 1)))
			    (qstat-f (nth next-i jobids)))))
	(local-set-key "Q" 'qstat))


(defun helm-qdel-job (jobid)
  (shell-command (format "qdel %s" jobid)))

(defun helm-qhold-job (jobid)
  (shell-command
   (concat "qhold -h " (completing-read "Hold: " '("u" "o" "s") nil nil "u") " " jobid)))

(defun helm-qrls-job (jobid)
  (shell-command
   (concat "qrls -h " (completing-read "Hold: " '("u" "o" "s")nil nil "uos") " " jobid)))

(setq helm-queue-source
      '((name . "qstat")
	(candidates . helm-job-candidates)
	(action . (("info" . qstat-f)
		   ("delete" . (lambda (jobid)
				 (mapc 'helm-qdel-job (helm-marked-candidates))))
		    ("hold" . (lambda (jobid)
				(mapc 'helm-qhold-job (helm-marked-candidates))))
		    ("release hold" .(lambda (jobid)
				       (mapc 'helm-qrls-job (helm-marked-candidates))))))))

(setq helm-gilgamesh-actions
      '((name . "Gilgamesh")
	(candidates . (("Node net traffic" . gilgamesh-net)))
	(action . (("default action" . (lambda (x) (funcall x)))))))

(defun qstat ()
  "Helm interface to jobs in the queue."
  (interactive)
  (helm :sources '(helm-gilgamesh-actions helm-queue-source)))


(setq helm-top-command "env COLUMNS=%s ps aux | bpstat -P master | sort -k 3 -n -r ")
