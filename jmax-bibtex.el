;;; jmax-bibtex.el --- jmax-bibtex utilities

;;; Commentary:
;; Requires: s.el, dash.el, org-ref.el, doi-utils.el

;;; Code:

(defvar jmax-bibtex-abbreviations
  '(("ACAT" "ACS Catalysis" "ACS Catal.")
    ("AM" "Acta Materialia" "Acta Mater.")
    ("AMM" "Acta Metallurgica et Materialia" "Acta Metall. Mater.")
    ("AMiner" "American Mineralogist" "Am. Mineral.")
    ("AngC" "Angewandte Chemie-International Edition" "Angew. Chem. Int. Edit.")
    ("APLM" "APL Materials" "APL Mat.")
    ("ACBE" "Applied Catalysis B: Environmental" "Appl. Catal. B-Environ.")
    ("APL" "Applied Physics Letters" "Appl. Phys. Lett.")
    ("ASS" "Applied Surface Science" "Appl. Surf. Sci.")
    ("CL" "Catalysis Letters" "Catal. Lett.")
    ("CST" "Catalysis Science & Technology" "Catal. Sci. Technol.")
    ("CT" "Catalysis Today" "Catal. Today")
    ("CPL" "Chemical Physics Letters" "Chem. Phys. Lett")
    ("CR" "Chemical Reviews" "Chem. Rev.")
    ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
    ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
    ("CM" "Chemistry of Materials" "Chem. Mater.")
    ("CSA" "Colloids and Surfaces, A: Physicochemical and Engineering Aspects" "Colloids Surf., A")
    ("CPMS" "Computational Materials Science" "Comp. Mater. Sci.")
    ("CPC" "Computer Physics Communications" "Comput. Phys. Commun.")
    ("CGD" "Crystal Growth \\& Design" "Cryst. Growth Des.")
    ("CEC" "CrystEngComm" "CrystEngComm")
    ("ECST" "ECS Transactions" "ECS Trans.")
    ("EES" "Energy \\& Environmental Science" "Energy Environ. Sci.")
    ("HPR" "High Pressure Research" "High Pressure Res.")
    ("IC" "Inorganic Chemistry" "Inorg. Chem.")
    ("IECR" "Industrial \\& Engineering Chemistry Research" "Ind. Eng. Chem. Res.")
    ("JJAP" "Japanese Journal of Applied Physics" "Jpn. J. Appl. Phys.")
    ("JMatR" "Journal of  Materials Research" "J. Mater. Res.")
    ("JALC" "Journal of Alloys and Compounds" "J. Alloy Compd.")
    ("JAC" "Journal of Applied Crystallography" "J. Appl. Crystallogr.")
    ("JAP" "Journal of Applied Physics" "J. Appl. Phys.")
    ("JC" "Journal of Catalysis" "J. Catal.")
    ("JCP" "Journal of Chemical Physics" "J. Chem. Phys.")
    ("JCC" "Journal of Computational Chemistry" "J. Comput. Chem.")
    ("JCG" "Journal of Crystal Growth" "J. Crys. Growth")
    ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
    ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
    ("JMSL" "Journal of Materials Science Letters" "J. Mater. Sci. Lett.")
    ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
    ("JPE" "Journal of Phase Equilibria" "J. Phase Equilib.")
    ("JPCS" "Journal of Physics and Chemistry of Solids" "J. Phys. Chem. Solids")
    ("JPCM" "Journal of Physics: Condensed Matter" "J. Phys.: Condens. Matter")
    ("JSSC" "Journal of Solid State Chemistry" "J. Solid State Chem.")
    ("JACerS" "Journal of the American Ceramic Society" "J. Am. Ceram. Soc.")
    ("JACS" "Journal of the American Chemical Society" "J. Am. Chem. Soc.")
    ("JES" "Journal of The Electrochemical Society" "J. Electrochem. Soc.")
    ("JES" "Journal of The Electrochemical Society" "J. Electrochem. Soc.")
    ("JEaC" "Journal of Electroanalytical Chemistry" "J. Electroanal. Chem.")
    ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
    ("JVST" "Journal of Vacuum Science \\& Technology A" "J. Vac. Sci. Technol. A")
    ("ML" "Materials Letters" "Mater. Lett.")
    ("MSE-BS" "Materials Science and Engineering B" "Mat. Sci. Eng. B-Solid")
    ("MOLSIM" "Molecular Simulation" "Mol. Sim.")
    ("Nature" "Nature" "Nature")
    ("NM" "Nature Materials" "Nat. Mater.")
    ("PML" "Philosophical Magazine Letters" "Phil. Mag. Lett.")
    ("PMA" "Philosophical Magazine A" "Phil. Mag. A")
    ("PA" "Physica A: Statistical Mechanics and its Applications" "Physica A")
    ("PB" "Physica B-Condensed Matter" "Physica B")
    ("PCCP" "Physical Chemistry Chemical Physics" "Phys. Chem. Chem. Phys.")
    ("PSSB" "physica status solidi (b)" "Phys. Status Solidi B")
    ("PRA" "Physical Review A" "Phys. Rev. A")
    ("PRB" "Physical Review B" "Phys. Rev. B")
    ("PRL" "Physical Review Letters" "Phys. Rev. Lett.")
    ("PCM" "Physics and Chemistry of Minerals" "Phys. Chem. Miner.")
    ("PSurfSci" "Progress in Surface Science" "Prog. Surf. Sci.")
    ("Science" "Science" "Science")
    ("SABC" "Sensors and Actuators B: Chemical" "Sensor. Actuat. B-Chem.")
    ("SS" "Surface Science" "Surf. Sci.")
    ("EPJB" "The European Physical Journal B" "Eur. Phys. J. B")
    ("JPC" "The Journal of Physical Chemistry" "J. Phys. Chem.")
    ("JPCB" "The Journal of Physical Chemistry B" "J. Phys. Chem. B")
    ("JPCC" "The Journal of Physical Chemistry C" "J. Phys. Chem. C")
    ("JPCL" "The Journal of Physical Chemistry Letters" "J. Phys. Chem. Lett.")
    ("JCP" "The Journal of Chemical Physics" "J. Chem. Phys.")
    ("TSF" "Thin Solid Films" "Thin Solid Films")
    ("TC" "Topics in Catalysis" "Top. Catal.")
    ("WR" "Water Research" "Water Res."))
  "List of (string journal-full-name journal-abbreviation). Find abbreviations at http://cassi.cas.org/search.jsp.")


(defun jmax-bibtex-generate-longtitles ()
  "generates longtitles.bib which are @string definitions of the
full journal names in `jmax-bibtex-abbreviations'."
  (interactive)
  (with-temp-file "longtitles.bib"
    (dolist (row jmax-bibtex-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
		      (nth 0 row)
		      (nth 1 row))))))


(defun jmax-bibtex-generate-shorttitles ()
    "generates shorttitles.bib which are @string definitions of the
abbreviated journal names in `jmax-bibtex-abbreviations'."
  (interactive)
  (with-temp-file "shorttitles.bib"
    (dolist (row jmax-bibtex-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
		      (nth 0 row)
		      (nth 2 row))))))


(defun jmax-stringify-journal-name (&optional key start end)
  "replace journal name in a bibtex entry with a string. The
strings are defined in `jmax-bibtex-abbreviations'. The optional
arguments allow you to use this with `bibtex-map-entries'"
  (interactive)
  (bibtex-beginning-of-entry)
  (when
      (string= "article"
	       (downcase
		(cdr (assoc "=type=" (bibtex-parse-entry)))))
    (let* ((full-names (mapcar
			(lambda (row)
			  (cons  (nth 1 row) (nth 0 row)))
			jmax-bibtex-abbreviations))
	   (abbrev-names (mapcar
			  (lambda (row)
			    (cons  (nth 2 row) (nth 0 row)))
			  jmax-bibtex-abbreviations))
	   (journal (s-trim (bibtex-autokey-get-field "journal")))
	   (bstring (or
		     (cdr (assoc journal full-names))
		     (cdr (assoc journal abbrev-names)))))
      (when bstring
	(bibtex-set-field "journal" bstring t)
        (bibtex-fill-entry)))))


(defvar jmax-nonascii-latex-replacements
  '(("í" . "{\\\\'i}")
    ("é" . "{\\\\'e}")
    ("á" . "{\\\\'a}")
    ("ø" . "{\\\\o}")
    ("ü" . "{\\\\\"u}")
    ("ñ" . "{\\\\~n}")
    ("å" . "{\\\\aa}")
    ("ö" . "{\\\\\"o}")
    ("ó" . "{\\\\'o}")
    ("ú" . "{\\\\'u}")
    ("İ" . "{\\\\.I}")
    ("ğ" . "{\\\\u{g}}")
    ("δ" . "$\\\\delta$")
    ("ç" . "{\\\\c{c}}")
    ("≤" . "$\\le$")
    ("<" . "$\\lt$")
    ("θ" . "$\\theta$")
    ("→" . "$\\rightarrow$")
    ("⇌" . "$\\leftrightharpoons$")
    ("×" . "$\\times$")
    ;; I think this is a non-ascii space.
    (" " . " ")
    (" " . " ")
    ("–" . "-")
    ("−" . "-"))
  "Cons list of non-ascii characters and their LaTeX representations")
  
(defun jmax-replace-nonascii ()
  "hook function to replace non-ascii characters in a bibtex
entry. "
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x) (car x)) jmax-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
	(replace-match (cdr (assoc char jmax-nonascii-latex-replacements))))
      (goto-char (point-min)))))

(add-hook 'org-ref-clean-bibtex-entry-hook 'jmax-replace-nonascii)


(defvar jmax-lower-case-words
  '("a" "an" "on" "and" "for"
    "the" "of" "in")
  "List of words to keep lowercase")


(defun jmax-title-case-article (&optional key start end)
  "Convert a bibtex entry article title to title-case. The
arguments are optional, and are only there so you can use this
function with `bibtex-map-entries' to change all the title
entries in articles."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
	 (words (split-string title))
	 (start 0))
    (when
	(string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (setq words (mapcar
		   (lambda (word)
		     (if (or
			  ;; match words containing {} or \ which are probably
			  ;; LaTeX or protected words
			  (string-match "\\$\\|{\\|}\\|\\\\" word)
			  ;; these words should not be capitalized, unless they
			  ;; are the first word
			  (-contains? jmax-lower-case-words (s-downcase word)))
			 word
		       (s-capitalize word)))
		   words))

      ;; Check if first word should be capitalized
      (when (-contains? jmax-lower-case-words (car words))
	(setf (car words) (s-capitalize (car words))))

      (setq title (mapconcat 'identity words " "))
      
      ;; Capitalize letters after a dash
      (while 
	  (string-match "[a-zA-Z]-\\([a-z]\\)" title start)
	(let ((char (substring title (match-beginning 1) (match-end 1))))
	  (setf (substring title (match-beginning 1) (match-end 1))
		(format "%s" (upcase char)))
	  (setq start (match-end 1))))
	    
      ;; this is defined in doi-utils
      (bibtex-set-field
       "title"
       title)
      (bibtex-fill-entry))))

(add-hook 'org-ref-clean-bibtex-entry-hook 'jmax-title-case-article)

(defun jmax-sentence-case-article (&optional key start end)
  "Convert a bibtex entry article title to sentence-case. The
arguments are optional, and are only there so you can use this
function with `bibtex-map-entries' to change all the title
entries in articles.

This is not a perfect function. It splits the title on whitespace.
"
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
	 (words (split-string title))
	 (start 0))
    (when
	(string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (setq words (mapcar
		   (lambda (word)
		     (if
			 ;; match words containing {} or \ which are probably
			 ;; LaTeX or protected words
			 (string-match "\\$\\|{\\|}\\|\\\\" word)
			 word
		       (s-downcase word)))
		   words))
      
      ;; capitalize first word
      (setf (car words) (s-capitalize (car words)))

      ;; join the words
      (setq title (mapconcat 'identity words " "))

      ;; capitalize a word after a :, eg. a subtitle, and protect it
      (while
	  (string-match "[a-z]:\\s-+\\([A-Z]\\)" title start)
	(let ((char (substring title (match-beginning 1) (match-end 1))))
	  (setf (substring title (match-beginning 1) (match-end 1))
		(format "{%s}" (upcase char)))
	  (setq start (match-end 1))))
	    
      ;; this is defined in doi-utils
      (bibtex-set-field
       "title" title)
       
      (bibtex-fill-entry))))




(defun jmax-bibtex-next-entry (&optional n)
  "Jump to the beginning of the next bibtex entry. N is a prefix
argument. If it is numeric, jump that many entries
forward. Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
		     (bibtex-beginning-of-entry)))
    (forward-char)
    (bibtex-next-entry))

  ;; search forward for an entry 
  (when 
      (re-search-forward bibtex-entry-head nil t (and (numberp n) n))
    ;; go to beginning of the entry
    (bibtex-beginning-of-entry)))


(defun jmax-bibtex-previous-entry (&optional n)
  "Jump to beginning of the previous bibtex entry. N is a prefix
argument. If it is numeric, jump that many entries back."
  (interactive "P")
  (bibtex-beginning-of-entry)
 (when 
     (re-search-backward bibtex-entry-head nil t (and (numberp n) n))
   (bibtex-beginning-of-entry)))


(defun jmax-bibtex-mode-keys ()
  "Modify keymaps used by `bibtex-mode'."
  (local-set-key (kbd "M-n") 'jmax-bibtex-next-entry)
  (local-set-key (kbd "M-p") 'jmax-bibtex-previous-entry))

;; add to bibtex-mode-hook
(add-hook 'bibtex-mode-hook 'jmax-bibtex-mode-keys)


(provide 'jmax-bibtex)

;;; jmax-bibtex.el ends here
