;; init_misc.el

;; Multi-term
(use-package multi-term
  :ensure t
  :config
  (setq mult-term-program "/bin/bash")
  (setq term-unbind-key-list '("C-x" "C-h" "M-x" "M-h" "M-l"))
  (add-to-list 'term-bind-key-alist '("C-c C-x" "C-r" "C-z" . term-send-raw)))

;; Org Mode
(use-package org
  :ensure t)

;; Allow copy/paste between terminal emacs and MacOSX
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx"
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

