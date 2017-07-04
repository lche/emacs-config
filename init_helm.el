(setq initial-scratch-message (concat initial-scratch-message
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\
;; This Emacs is Powered by \`HELM' using\n\
;; emacs program \"$EMACS\".\n\
;; This is a minimal \`helm' configuration to discover \`helm' or debug it.\n\
;; You can retrieve this minimal configuration in \"$CONF_FILE\" \n\
;; Some originals emacs commands have been replaced by own \`helm' commands:\n\n\
;; - \`find-file'(C-x C-f)            =>\`helm-find-files'\n\
;; - \`occur'(M-s o)                  =>\`helm-occur'\n\
;; - \`list-buffers'(C-x C-b)         =>\`helm-buffers-list'\n\
;; - \`completion-at-point'(M-tab)    =>\`helm-lisp-completion-at-point'[1]\n\
;; - \`dabbrev-expand'(M-/)           =>\`helm-dabbrev'\n\n\
;; - \`execute-extended-command'(M-x) =>\`helm-M-x'\n\n
;; Some others native emacs commands are \"helmized\" by \`helm-mode'.\n\
;; [1] Coming with emacs-24.4 \`completion-at-point' is \"helmized\" by \`helm-mode'\n\
;; which provide helm completion in many other places like \`shell-mode'.\n\
;; You will find embeded help for most helm commands with \`C-h m'.\n\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"))

(setq package-user-dir (directory-file-name
                        (file-name-directory
                         (directory-file-name default-directory))))
(unless (member "helm.el" (directory-files default-directory))
  (setq package-load-list '((helm-core t) (helm t) (async t) (popup t)))
  (package-initialize))
(add-to-list 'load-path (file-name-directory (file-truename "$0")))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(blink-cursor-mode -1)
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))

(helm-flx-mode +1)
(require 'helm-fuzzier)
(helm-fuzzier-mode 1)
