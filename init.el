;; Set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Load all custom auctex configuration
(load "~/.emacs.d/auctex_config.el")

;; UI settings
(load-theme 'misterioso)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq-default cursor-type 'bar)
(set-cursor-color "#ffffff")

;;; Disable line numbering in certain modes
(dolist (mode '("eshell" "org" "inferior-emacs-lisp"))
  (add-hook (intern (concat mode "-mode-hook")) (lambda () (display-line-numbers-mode 0))))

;; Editing settings
(setq next-line-add-newlines t)
(delete-selection-mode 1)
(global-visual-line-mode t)
(show-paren-mode t)

;; Packages
;;; General - More powerful keybinding functions
(use-package general) 
(general-define-key
 "C-M-j" 'counsel-switch-buffer)

;;; Lispy - for better elisp editing
(use-package lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode t))) 

;;; Sublimity - Smoother scrolling
(use-package sublimity)
(require 'sublimity-scroll)
(sublimity-mode t)

;;; Ivy - A generic completion mechanism
(use-package ivy
  :config
  (ivy-mode 1)
  :bind (("C-s" . swiper)))

;;; Counsel - A collection of Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

;;; Ivy-rich - A more friendly interface for ivy
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;;; Swiper - An Ivy-enhanced search tool
(use-package swiper)

;;; Which-key - A minor-mode which displays avalable keybindings
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

;;; Helpful - A more helpful help page
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;; Doom-modeline - A fancier modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package all-the-icons)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline helpful which-key ivy-rich sublimity lispy general xah-fly-keys use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
