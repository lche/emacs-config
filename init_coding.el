;; init_coding.el
;; Lauren Che
;; 04/08/2016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iedit: Edit multiple instances of a word in a buffer
;(require 'iedit)
;(defun iedit-dwim (arg)
;  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;  (interactive "P")
;  (if arg
;      (iedit-mode)
;    (save-excursion
;      (save-restriction
;        (widen)
;        ;; this function determines the scope of `iedit-start'.
;        (if iedit-mode
;            (iedit-done)
;          ;; `current-word' can of course be replaced by other
;          ;; functions.
;          (narrow-to-defun)
;          (iedit-start (current-word) (point-min) (point-max)))))))
;(global-set-key (kbd "C-;") 'iedit-dwim)
;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set Scheme program to stk
(set-variable (quote scheme-program-name) "stk")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elpy: IDE environment
(package-initialize)
(elpy-enable)

;; Pyflakes: for error checking
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)

;; Indentation
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
