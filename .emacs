;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load Path
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

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
                         ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
    (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-capture-templates '(("w" "Inbox [work]" entry
			       (file+headline "~/org/work/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("h" "Inbox [home]" entry
			       (file+headline "~/org/home/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("e" "Tickler [work]" entry
			       (file+headline "~/org/work/tickler.org" "Tickler")
			       "* TODO %i%?")
			      ("j" "Tickler [home]" entry
			       (file+headline "~/org/home/tickler.org" "Tickler")
			       "* TODO %i%?")))

(add-hook 'org-mode-hook 'toggle-truncate-lines)
(add-hook 'org-mode-hook 'toggle-word-wrap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :ensure t)
(require 'helm-mode)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Appearance and Behavior
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(toggle-scroll-bar -1)

(use-package switch-window
  :ensure t
  :bind
  ("C-x C-o" . switch-window))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :requires pylint)
(elpy-enable)

(use-package whitespace
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing)))

(use-package hl-line
  :config
  (set-face-attribute hl-line-face nil :underline t))

(use-package linum-mode
  :hook prog-mode)

(use-package smartparens
  :ensure t
  :requires smartparens-config
  :hook prog-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Misc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom-set Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zerodark)))
 '(custom-safe-themes
   (quote
    ("bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" "e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "ff79b206ad804c41a37b7b782aca44201edfa8141268a6cdf60b1c0916343bd4" default)))
 '(global-hl-line-mode t)
 '(org-agenda-files (quote ("~/org/home/tickler.org")))
 '(package-selected-packages
   (quote
    (magit web-mode helm smartparens neotree undo-tree zerodark-theme elpy flycheck auto-complete switch-window use-package)))
 '(show-paren-mode t)
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

