(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-section-label
   '(("part")
     ("chapter")
     ("section")
     ("subsection")
     ("subsubsection")))
 '(TeX-PDF-mode t)
 '(TeX-auto-regexp-list 'TeX-auto-full-regexp-list)
 '(TeX-electric-escape t)
 '(TeX-electric-math '("\\(" . "\\)"))
 '(TeX-insert-braces nil)
 '(TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "Okular")
     (output-pdf "Okular")
     (output-html "xdg-open")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(blink-matching-paren t)
 '(custom-enabled-themes '(misterioso))
 '(custom-safe-themes
   '("57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" default))
 '(fci-rule-color "#14151E")
 '(package-selected-packages
   '(doom-themes helpful counsel ivy-rich which-key doom-modeline swiper diminish ivy use-package platformio-mode afternoon-theme frame-tabs auctex))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3")))
 '(vc-annotate-very-old-color nil))

;;Load theme
(load-theme 'misterioso)

;;Set shortcut for switching buffer
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;;ensure that all needed packages are installed
(setq use-package-always-ensure t)

;;initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;;enable use-package
(require 'use-package)

;;Disable splash screen and banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t) 

;;Enable Sublimity
(add-to-list 'load-path "~/.emacs.d/sublimity/")
(require 'sublimity)
(require 'sublimity-scroll)
(sublimity-mode 1)

;;disable line numbering in eshell and org mode
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

;;Enable IDO
;;(ido-mode 1)
;;(setq ido-everywhere t)
;;(setq ido-enable-flex-matching t)
;;(setq ido-file-extensions-order '(".tex"))

;;Enable diminish
(use-package diminish)

;;enable counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

;;Enable IVY and swiper
(use-package swiper)
(use-package ivy
  :config
  (ivy-mode 1)
  :bind (("C-s" . swiper)))

;;enable ivy-rich
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;;don't show ivy-mode in modeline
(diminish 'ivy-mode)

;;enable which-key
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

;;enable helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;enable doom-modeline
(use-package doom-modeline :ensure t
  :init (doom-modeline-mode 1))

;;enable doom-themes
(use-package doom-themes)

;;Finish redrawing of display, before input events are handled to reduce screen tearing
;;(setq redisplay-dont-pause t) Turned off becauso it caused strange flickering

;;Insert new lines when reaching bottom
(setq next-line-add-newlines t)

;;Turn line numbers mode on by default
(global-display-line-numbers-mode 1)

;;Don't wrap in the middle of a word
(global-visual-line-mode t)

;;Change cursor style to line, instead of block
(setq-default cursor-type 'bar) 

;;Set cursor color to white
(set-cursor-color "#ffffff")

;;Replace selection when typing
(delete-selection-mode 1)

;;enable all-the-icons
(use-package all-the-icons)

;;define default directory
(setq default-directory "~/Documents/4ahme/")

(setq TeX-save-query nil)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(add-hook 'plain-TeX-mode-hook
	  (lambda () (set (make-local-variable 'TeX-electric-math)
			  (cons "$" "$"))))
(add-hook 'LaTeX-mode-hook
	  (lambda () (set (make-local-variable 'TeX-electric-math)
			  (cons "\\(" "\\)"))))

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
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
