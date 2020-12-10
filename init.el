;; Load all custom auctex configuration
(load "~/.emacs.d/auctex_config.el")

;;Load theme
(load-theme 'misterioso)

;; Disable UI elements
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Set keybindings
(use-package general) 
(general-define-key
 "C-M-j" 'counsel-switch-buffer)

;;ensure that all needed packages are installed
(setq use-package-always-ensure t)

;;initialize package sources
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

(require 'xah-fly-keys)

;(xah-fly-keys-set-layout 'koy)

;(xah-fly-keys 1)

;;define default directory
(setq default-directory "~/Documents/4ahme/")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
