(org-add-link-type
 "arXiv"
 ;; clicking
 (lambda (link-string) (browse-url (format "http://arxiv.org/abs/%s" link-string)))
 ;; formatting
(lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "")); no output for html
    ((eq format 'latex)
     ;; write out the latex command
     (format "\\url{http://arxiv.org/abs/%s}" keyword)))))

;;  	arXiv:cond-mat/0410285

;; NIH links
(org-add-link-type
 "pmcid"
 ;; clicking
 (lambda (link-string) (browse-url (format "http://www.ncbi.nlm.nih.gov/pmc/articles/%s" link-string)))
 ;; formatting
(lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "")); no output for html
    ((eq format 'latex)
     ;; write out the latex command
     (format "\\url{http://www.ncbi.nlm.nih.gov/pmc/articles/%s}" keyword)))))

;; pmcid:PMC3498956

