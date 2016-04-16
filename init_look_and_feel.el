;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; Turn off humongous tool bar at the top
(tool-bar-mode -1)

;; Turn off scroll bar
(scroll-bar-mode -1)

;; Turn off wrapping
(set-default 'truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; Theme
(custom-set-variables
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476"
     "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4"
     "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9"
     "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(org-agenda-files (quote ("~/Documents/todo.org")))
 '(python-check-command "/usr/local/bin/pyflakes")
 '(quack-programs
   (quote
    ("stk" "Scheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi"
     "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme"
     "quack" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48"
     "scsh" "sisc" "stklos" "sxi"))))

;; Rainbow Delimiters
(require 'rainbow-delimiters)
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "IndianRed1"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "tan1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "LightGoldenrod1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "SpringGreen1"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "CadetBlue1"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "MediumPurple1")))))
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; frame-cmds
(require 'frame-cmds)

;; buffer-move
(require 'windmove)
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

;; switch-window
(require 'switch-window)
(global-set-key (kbd "C-x C-o") 'switch-window)
