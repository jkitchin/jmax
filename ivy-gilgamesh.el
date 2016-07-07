;;; ivy-gilgamesh.el --- Interact with gilgamesh with ivy


;;; Commentary:
;;

;;; Code:

(defun ivy-top ()
  "Run top on the head node. Default action is to kill process."
  (interactive)
  (let ((output (s-split
		 "\n"
		 (shell-command-to-string
		  "ps aux | bpstat -P master | sort -k 3 -n -r"))))
    (ivy-read "line: " output
	      :action
	      '(1
		("k" (lambda (line)
		       (let* ((fields (s-split " " line t))
			      (pid (elt fields 1)))
			 (message "%s" pid)
			 (when  (y-or-n-p (format  "Kill %s\n%s" pid line))
			   (shell-command
			    (concat "echo "
				    (shell-quote-argument (read-passwd "Password? "))
				    (format
				     " | sudo -S kill -9 %s" pid))))))
		 "kill")))))

(defun ivy-qstat ()
  "Run qstat through ivy interface.
ivy actions to delete, info, hold, release."
  (interactive)
  (let ((output (s-split "\n"  (shell-command-to-string "qstat -n -1"))))
    (ivy-read "line:" output
	      :action '(1
			("d" (lambda (line)
			       (let ((jobid (car (s-split
						  "\\."
						  (car (s-split " " line t))))))
				 (message "Deleting %s" jobid)
				 (shell-command
				  (format "qdel %s" jobid))))
			 "delete")
			("i" (lambda (line)
			       (shell-command
				(format "qstat -f %s"
					;; 1395210.gilgames
					(car (s-split
					      "\\."
					      (car (s-split " " line t)))))))
			 "info")
			("h" (lambda (line)
			       (let ((jobid (car (s-split
						  "\\."
						  (car (s-split " " line t))))))
				 (shell-command
				  (format "qhold -uos %s" jobid)))
			       )
			 "hold")
			("r" (lambda (line)
			       (let ((jobid (car (s-split
						  "\\."
						  (car (s-split " " line t))))))
				 (shell-command
				  (format "qrls -uos %s" jobid))))
			 "release")))))

(defun ivy-node-ps (node)
  "Run ps on a NODE to see processes."
  (interactive
   (list (completing-read "Node: " (loop for i from 1 to 30
					 collect (format "%s" i)) nil t)))
  (let ((output (s-split
		 "\n"
		 (shell-command-to-string
		  (format "bpsh %s -m top -bn 1" node)))))
    (ivy-read "line:" output
	      :action
	      '(1
		("k" (lambda (line)
		       (let ((pid (car (s-split " " line t))))
			 (when  (y-or-n-p (format  "Kill %s" pid))
			   (shell-command
			    (concat "echo "
				    (shell-quote-argument (read-passwd "Password? "))
				    (format
				     " | sudo -S kill -9 %s" pid))))))
		 "kill")))))


(defun ivy-pbsnodes ()
  "Run pbsnodes using ivy for actions."
  (interactive)
  (let* ((xml (with-temp-buffer
		(insert (shell-command-to-string "pbsnodes -x"))
		(car (xml-parse-region (point-min) (point-max)))))
	 (nodes (xml-get-children xml 'Node))
	 (names (loop for node in nodes
		      collect  (car (last (assoc 'name node)))))
	 (loads (loop for node in nodes
		      collect
		      (let ((status (car (last (assoc 'status node)))))
			(if (null status)
			    nil
			  (string-match "loadave=\\(.*?\\),"
					status)
			  (string-to-number (match-string 1 status))))))
	 (jobids (loop for node in nodes
		       collect
		       (let ((status (car (last (assoc 'status node)))))
			 (if (null status)
			     nil
			   (string-match "jobs=\\(.*?\\),"
					 status)
			   (s-split " " (match-string 1 status) t))))))

    (ivy-read "Node: "
	      (mapcar* (lambda (name load jobids)
			 (list (format "%-3s: %5s" name load)
			       (cons 'name name)
			       (cons 'jobids jobids)))
		       names loads jobids)
	      :action
	      '(1
		("a" (lambda (node)
		       (shell-command (format "pbsnodes %s"
					      (cdr (assoc 'name node))))
		       (switch-to-buffer "*Shell Command Output*"))
		 "list attributes")
		("o" (lambda (node)
		       (shell-command (format "pbsnodes -o %s"
					      (cdr (assoc 'name node)))))
		 "offline")
		("c" (lambda (node)
		       (shell-command (format "pbsnodes -c %s"
					      (cdr (assoc 'name node)))))
		 "clear")
		("j" (lambda (node)
		       (ivy-read "Job: " (cdr (assoc 'jobids node))))
		 "jobs on node")))))


(defun ivy-net ()
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
	(add-to-list 'output (list
			      (format "%-3s: %1.2f Mb/s"
				      node rate)
			      node rate))))

    (ivy-read "Node: " (sort output (lambda (a b) (> (elt a 2)
						     (elt b 2))))
	      :action
	      '(1
		("p"  (lambda (node)
			(ivy-node-ps (elt node 0)))
		 "ivy-node-ps")
		("h" (lambda (node)
		       (gilgamesh-pbsnodes (elt node 0)))
		 "gilgamesh-pbsnodes")))))

(provide 'ivy-gilgamesh)

;;; ivy-gilgamesh.el ends here
