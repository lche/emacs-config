;; init_misc.el
;; Lauren Che
;; 04/08/2016

;;;; Multi-term
(require 'multi-term)
(setq mult-term-program "/bin/bash")
(setq term-unbind-key-list '("C-x" "C-h" "M-x"))
(add-to-list 'term-bind-key-alist '("C-c C-x" "C-r" "C-z" . term-send-raw))

;; Google-this
(require 'google-this)
(google-this-mode 1)
