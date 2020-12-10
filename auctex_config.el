;; Prompt for macro name when typing "\"
(setq TeX-electric-escape t)

;;Disable prompt for section labels
(setq LaTeX-section-label '(("part")
			    ("chapter")
			    ("section")
			    ("subsection")
			    ("subsubsection")))

;;Enable full parsing
(setq TeX-auto-regexp-list 'TeX-auto-full-regexp-list)

;;Enable parse on load and save
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;Insert \(\) when dollar is pressed und put point between them
(setq TeX-electric-math (cons "\\(" "\\)"))

;;Don't put an empty pair of braces to a macro without arguments
(setq TeX-insert-braces nil)

;;Enable correlation between source and output
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

;;Set Okular as default viewer for dvi and pdf
(setq TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty")
				   ((output-dvi style-pstricks) "dvips and gv")
				   (output-dvi "Okular")
				   (output-pdf "Okular")
				   (output-html "xdg-open")))

;;Don't ask bevore saving a document
(setq TeX-save-query nil)



;;Intaractive Function, wich sets up a new LaTeX document
(defun letup ()
  (interactive)

  (setq german t)
  (setq header nil)
  (setq graphics nil)
  
  (when (y-or-n-p "english?") (setq german nil))
  
  (if (y-or-n-p "twocolumn?")
      (insert "\\documentclass[twocolumn]{article}\n\n")
      (insert "\\documentclass{article}\n\n")
  )
  
  (when (y-or-n-p "headings?") (insert "\\usepackage{fancyhdr}\n") (setq header t))

  (when german (insert "\\usepackage[ngerman]{datetime}\n"))

  (when (y-or-n-p "small margin?") (insert "\\usepackage[margin=2cm]{geometry}"))

  (when (y-or-n-p "graphicx?") (insert "\\usepackage{graphicx}\n\n") (setq graphics t))

  (when graphics (insert "\\graphicspath{ {" (read-directory-name "Image path: ") "} }\n\n" ))

  (when (y-or-n-p "parindent 0?") (insert "\\setlength{\\parindent}{0em}\n\\setlength{\\parskip}{1em}\n\n"))
  
  (when (y-or-n-p "sans serif?") (insert "\\renewcommand{\\familydefault}{\\sfdefault}\n\n"))
  
  (when german (insert "\\newdateformat{myformat}{\\THEDAY{. }\\monthnamengerman[\\THEMONTH], \\THEYEAR}\n"))
  (unless (y-or-n-p "pagenumbering?") nil (insert "\\pagenumbering{gobble}\n"))
  (when header
    (insert "\\pagestyle{fancy}\n")
    (if (y-or-n-p "Surname first?") (insert "\\lhead{Marcher Simon}\n") (insert "\\lead{Simon Marcher}\n"))
    (insert "\\chead{" (read-string "Header text: ") "}\n")
    (insert "\\rhead{\\today}\n")
    )

  (insert "\n\\begin{document}\n\n\n\n\\end{document}")
  (backward-char 16))
