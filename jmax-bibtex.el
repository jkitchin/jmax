;;; jmax-bibtex.el --- jmax-bibtex utilities

;;; Commentary:
;; 

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
    ("JPCB" "The Journal of Physical Chemistry  B" "J. Phys. Chem. B")
    ("JPCC" "The Journal of Physical Chemistry C" "J. Phys. Chem. C")
    ("JCP" "The Journal of Chemical Physics" "J. Chem. Phys.")
    ("TSF" "Thin Solid Films" "Thin Solid Films")
    ("TC" "Topics in Catalysis" "Top. Catal.")
    ("WR" "Water Research" "Water Res."))
  "List of (string journal-full-name journal-abbreviation).")


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

(defun replace-nonascii ()
  "hook function to replace non-ascii characters"
  (interactive)
  (let ((chars '(("í" . "{\\\\'i}")
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
		 ("–" . "-")
		 ("−" . "-"))))
    (save-restriction
      (bibtex-narrow-to-entry)
      (goto-char (point-min))
      (dolist (char (mapcar (lambda (x) (car x)) chars))
	(while (re-search-forward char nil t)
	  (replace-match (cdr (assoc char chars))))
	(goto-char (point-min))))))

(add-hook 'org-ref-clean-bibtex-entry-hook 'replace-nonascii)


(defun title-case-article (&optional key start end)
  "Convert a bibtex entry article title to title-case. The
arguments are optional, and are only there so you can use this
function with `bibtex-map-entries' to change all the title
entries in articles."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
	 (words (split-string title))
	 (lower-case-words '("a" "an" "on" "and" "for"
			     "the" "of" "in")))
    (when
	(string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (setq words (mapcar
		   (lambda (word)
		     (if (or
			  ;; match words containing {} or \ which are probably
			  ;; LaTeX or protected words
			  (string-match "{\\|}\\|\\\\" word)
			  ;; these words should not be capitalized, unless they
			  ;; are the first word
			  (-contains? lower-case-words (s-downcase word)))
			 word
		       (s-capitalize word)))
		   words))

      ;; Check if first word should be capitalized
      (when (-contains? lower-case-words (car words))
	(setf (car words) (s-capitalize (car words))))
	    
      ;; this is defined in doi-utils
      (bibtex-set-field
       "title"
       (mapconcat 'identity words " "))
      (bibtex-fill-entry))))

(add-hook 'org-ref-clean-bibtex-entry-hook 'title-case-article)

(defun sentence-case-article (&optional key start end)
  "Convert a bibtex entry article title to sentence-case. The
arguments are optional, and are only there so you can use this
function with `bibtex-map-entries' to change all the title
entries in articles.

This is not a perfect function. It splits the title on whitespace.
"
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
	 (words (split-string title)))
    (when
	(string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (setq words (mapcar
		   (lambda (word)
		     (unless
			 ;; match words containing {} or \ which are probably
			 ;; LaTeX or protected words
			 (string-match "{\\|}\\|\\\\" word)
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



(provide 'jmax-bibtex)

;;; jmax-bibtex.el ends here
