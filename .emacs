;; .emacs.el

;; Give stack trace for debugging info
(setq debug-on-error t)

;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; Package
(require 'package)
(setq package-enable-at-startup nil)

(setq package-user-dir "/Users/lche/.emacs.d/elpa/")
(setq default-directory "/Users/lche/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Save face-customization settings to init_look_and_feel.el
(setq custom-file "~/.emacs.d/init/init_look_and_feel.el")

;; Load other init files
(load "~/.emacs.d/init/init_coding.el")
(load "~/.emacs.d/init/init_misc.el")
(load "~/.emacs.d/init/init_look_and_feel.el")
(load "~/.emacs.d/init/init_helm.el")

;; ;; Force Emacs to recognize bash environment
;; (let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
;;   (setq exec-path 
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))

;; Prevent emacs from creating backup files
(setq make-backup-files nil)
