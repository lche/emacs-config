;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "https://marmalade-repo.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")
			   ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evil
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :bind (:map evil-insert-state-map
	 ("C-u" . evil-scroll-up)
	 :map evil-normal-state-map
	 ("C-u" . evil-scroll-up)
	 :map evil-visual-state-map
	 ("C-u" . evil-scroll-up)
	 :map evil-motion-state-map
	 ("C-u" . evil-scroll-up))
  :config
  (evil-set-initial-state 'term-mode 'emacs))
(evil-mode)

(use-package counsel
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hydra 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-evil-normal (evil-normal-state-map ",")
    "Hydra for evil-normal mode"
    ("f" find-file "find-file")
    ("/" swiper "swiper")
    ("b" switch-to-buffer "switch-to-buffer")
    ("h" mark-whole-buffer "mark-whole-buffer")
    ("x" counsel-M-x "counsel-M-x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :bind (
  ("\C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  :map read-expression-map
  ("C-r" . counsel-expression-history)
  :map evil-normal-state-map
  ("M-x" . counsel-M-x))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))
(ivy-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :ensure t
  :config
  (setq general-default-keymaps 'evil-normal-state-map)
  (setq my-leader1 ",")
  (setq my-leader2 "M-;")

  (general-define-key :prefix my-leader1
		      :non-normal-prefix my-leader2
		      "f" 'find-file
		      "/" 'swiper
		      "b" 'switch-to-buffer
		      "h" 'mark-whole-buffer
		      "x" 'counsel-M-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Appearance
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(toggle-scroll-bar -1)

(use-package switch-window
  :ensure t
  :bind
  ("C-x o" . switch-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Misc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't want the default 'gnu' style:
;; if(foo)
;;   {
;;      bar++;
;;   }
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4 
      indent-tabs-mode nil)
			       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; External Init Files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-exists-p "~/.emacs.d/init/init_PAN.el")
    (load "~/.emacs.d/init/init_PAN.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Themes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zerodark-theme
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zerodark)))
 '(custom-safe-themes
   (quote
    ("e3f648bb477a2e2332124f5ca8bd070e8624f152be6b4478668a69e5de7510ff" default)))
 '(electric-pair-mode t)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (zerodark-theme switch-window evil-surround general ace-window emms which-key ivy magit evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


