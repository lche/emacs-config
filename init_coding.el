;; init_coding.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Formatting
(setq c-default-style "bsd")          ; Default "Allman" style.
(setq c-basic-offset 4)               ; Default indent of 4 spaces.
(setq tab-width c-basic-offset)       ; Tab indents to C offset.
(setq tab-stop-list                   ; Set tab stop every 4 characters.
      (number-sequence tab-width (* tab-width 30) tab-width))

;; Turn on line numbers
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'electric-pair-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set Scheme program to stk
(set-variable (quote scheme-program-name) "stk")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elpy: IDE environment
(use-package elpy
  :ensure t
  :config
  (package-initialize)
  (elpy-enable))

;; Pyflakes: for error checking
(use-package flycheck-pyflakes
  :ensure t
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

;; Indentation
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))
(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
