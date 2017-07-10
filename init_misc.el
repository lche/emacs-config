;; init_misc.el

;; Multi-term
(use-package multi-term
  :ensure t
  :config
  (setq mult-term-program "/bin/bash")
  (setq term-unbind-key-list '("C-x" "C-h" "M-x" "M-h" "M-l"))
   '(term-bind-key-alist
   (quote
    (("C-c C-x" "C-z" . term-send-raw)
     ("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . term-send-reverse-search-history)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete)))))


;; Org Mode
(use-package org
  :ensure t
  :init 
  ;; Add styling to #+BEGIN_SRC/#+END_SRC delimiters
  (defface org-block-begin-line
    '((t (:underline "#d26937" :foreground "#d26937" :background "grey")))
    "Face used for the line delimiting the begin of source blocks.")

  (defface org-block-background
    '((t (:background "#0c1014")))
    "Face used for the source block background.")

  (defface org-block-end-line
    '((t (:overline "#d26937" :foreground "#d26937" :background "#")))
    "Face used for the line delimiting the end of source blocks.")
  :config
  ;; Apply syntax highlighting to code blocks deliminted by
  ;; #+BEGIN_SRC/#+END_SRC
  (setq org-src-fontify-natively t))

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


(use-package palette
  :ensure t
  :init
  (use-package hexrgb
    :ensure t))

;; Color the background of a hexcode with its corresponding color
(use-package rainbow-mode
  :ensure t)
