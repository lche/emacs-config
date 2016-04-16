;; .emacs.el
;; Lauren Che
;; 04/08/2016

;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Load other init files
(load "~/.emacs.d/init/init_org.el")
(load "~/.emacs.d/init/init_coding.el")
(load "~/.emacs.d/init/init_kbd.el")
(load "~/.emacs.d/init/init_misc.el")
(load "~/.emacs.d/init/init_look_and_feel.el")
(load "~/.emacs.d/init/init_discover.el")
(load "~/.emacs.d/init/init_PAN.el")

;; Force Emacs to recognize bash environment
(let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Package repo setting
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Prevent emacs from creating backup files
(setq make-backup-files nil)

;; Preserve the following buffers when executing 'desktop-clear'
(setq desktop-clear-preserve-buffers-regexp '(todo.org, sodium, replay, panbox))

;; Open todo.org upon start up
(find-file "~/Documents/todo.org")
