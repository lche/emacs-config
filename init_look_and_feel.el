;; init_look_and_feel.el

;; Turn off ugly stuff
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package neotree
  :ensure t)

;; Move window around
(use-package windmove
  :ensure t
  :bind (("<C-s-up>" . buf-move-up)
         ("<C-s-down>" . buf-move-down)
         ("<C-s-left>" . buf-move-left)
         ("<C-s-right>" . buf-move-right)))

;; Switch to window by number
(use-package switch-window
  :ensure t
  :bind (("C-x C-o" . switch-window)))

;; Tab bar at the top
(use-package tabbar
  :ensure t
  :bind (("M-j" . tabbar-backward)
         ("M-k" . tabbar-forward))
  :config
  (set-face-attribute
   'tabbar-default nil
   :background "#245361"
   :foreground "#245361"
   :box '(:line-width 1 :color "#q245361" :style nil))
  (set-face-attribute
   'tabbar-unselected nil
   :background "#245361"
   :foreground "#99d1ce"
   :box '(:line-width 5 :color "#245361" :style nil))
  (set-face-attribute
   'tabbar-selected nil
   :background "#99d1ce"
   :foreground "#0c1014"
   :box '(:line-width 5 :color "#99d1ce" :style nil))
  (set-face-attribute
   'tabbar-highlight nil
   :background "#d26937"
   :foreground "#0c1014"
   :underline nil
   :box '(:line-width 5 :color "#d26937" :style nil))
  (set-face-attribute
   'tabbar-button nil
   :box '(:line-width 1 :color "#0c1014" :style nil))
  (set-face-attribute
   'tabbar-separator nil
   :background "#0c1014"
   :height 0.6)

  ;; Change padding of the tabs
  ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
  ;; adding spaces
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
		      (format "[%s]  " (tabbar-tab-tabset tab))
		    (format "%s  " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
	  label
	(tabbar-shorten
	 label (max 1 (/ (window-width)
			 (length (tabbar-view
				  (tabbar-current-tabset)))))))))

  (tabbar-mode 1))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-menu-buffer ((t (:weight bold))))
 '(font-lock-comment-face ((t (:foreground "#245361"))))
 '(mode-line ((t (:background "#d26937" :foreground "#0c1014" :box nil))))
 '(outline-1 ((t (:foreground "#d26937"))))
 '(powerline-active1 ((t (:background "#195466" :foreground "#99d1ce"))))
 '(powerline-active2 ((t (:background "#091f2e" :foreground "#599cab"))))
 '(powerline-inactive1 ((t (:background "#195466" :foreground "#0c1014"))))
 '(powerline-inactive2 ((t (:background "#0c1014" :foreground "#195466")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (gotham)))
 '(custom-safe-themes
   (quote
    ("ed8cf6d52a2ba9ed7a29a8aac81d83c362a9b62f48b558932a77130163fe9972" "f7cf9cbf5d649c4661ea8c68bf73dd9c89ce7d24cfded5601db86b9ac88bbc94" "b34636117b62837b3c0c149260dfebe12c5dad3d1177a758bb41c4b15259ed7e" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "8e7ca85479dab486e15e0119f2948ba7ffcaa0ef161b3facb8103fb06f93b428" "cadc97db0173a0d0bfc40473cab4da462af0ba8d60befd0a4879b582bcbc092d" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "36d92f830c21797ce34896a4cf074ce25dbe0dabe77603876d1b42316530c99d" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "8c75217782ccea7e9f3ad2dae831487a5fb636d042263d0a0e0438d551da3224" default)))
 '(custom-theme-load-path
   (quote
    ("/Users/lche/.emacs.d/init/" "/Users/lche/.emacs.d/elpa/airline-themes-20160203.1510/" "/Users/lche/.emacs.d/elpa/meacupla-theme-20151027.1517/" "/Users/lche/.emacs.d/elpa/monokai-theme-20170314.1612/" "/Users/lche/.emacs.d/elpa/smart-mode-line-powerline-theme-20160111.932/" "/Users/lche/.emacs.d/elpa/smart-mode-line-20160306.1103/" "/Users/lche/.emacs.d/elpa/tangotango-theme-20150702.104/" "/Users/lche/.emacs.d/elpa/zenburn-theme-20160320.1311/" "/Users/lche/.emacs.d/elpa/moe-theme-20170111.1838/" "/Users/lche/.emacs.d/elpa/gotham-theme-20170521.417/" custom-theme-directory t)) t)
 '(default-input-method "korean-hangul3")
 '(electric-pair-mode t)
 '(elfeed-feeds
   (quote
    ("http://www.schneier.com/blog/index.rdf" "http://www.social-engineer.org/feed/" "http://securityaffairs.co/wordpress/feed" "http://www.hackingarticles.in/feed/" "http://bhconsulting.ie/securitywatch/?feed=rss2" "http://www.infosecisland.com/rss.html" "http://feeds.feedburner.com/WhitehatSecurityBlog" "http://rssnewsapps.ziffdavis.com/pcmagsecurity.xml" "http://feeds.feedburner.com/inftoint" "https://blog.elearnsecurity.com/rss" "http://www.forbes.com/security/index.xml" "http://krebsonsecurity.com/feed/" "http://www.darkreading.com/rss/all.xml" "http://www.bleepingcomputer.com/feed/" "http://feeds.feedburner.com/GrahamCluleysBlog" "http://taosecurity.blogspot.com/atom.xml" "http://feeds.feedburner.com/TroyHunt" "http://packetstormsecurity.org/tools.xml" "http://thehackernews.com/feeds/posts/default" "http://www.us-cert.gov/current/index.rdf" "http://feeds.feedburner.com/Liquidmatrix" "http://feeds.feedblitz.com/thesecurityledger" "http://feeds.feedburner.com/HaveIBeenPwnedLatestBreaches" "http://feeds.feedburner.com/DavidLongenecker" "http://feeds.arstechnica.com/arstechnica/security" "https://tisiphone.net/feed/" "http://itsecurityguru.org/feed/" "http://feeds.feedburner.com/pauldotcom/XBIC" "http://feeds.feedburner.com/tripwire-state-of-security" "https://labsblog.f-secure.com/feed/" "http://www.sans.org/reading-room/rss/?portal=a2a5d0436f44d8cbd06b6cbdcdac3951" "http://feeds.feedburner.com/HOTforSecurity" "http://threatpost.com/en_us/rss.xml" "http://googleonlinesecurity.blogspot.com/atom.xml" "http://nakedsecurity.sophos.com/feed/" "http://www.symantec.com/xml/rss/srblogs.jsp" "http://blog.malwarebytes.org/feed/" "http://www.securelist.com/en/rss/allupdates" "http://blogs.akamai.com/atom.xml" "http://www.zonealarm.com/blog/index.php/feed/" "http://blog.cloudflare.com/rss.xml" "http://blog.eset.com/feed" "http://feeds.feedburner.com/SpiderlabsAnterior" "https://isc.sans.edu/dailypodcast.xml" "https://blogs.technet.microsoft.com/mmpc/feed/")))
 '(fci-rule-color "#383838")
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(indent-tabs-mode nil)
 '(jedi:complete-on-dot t t)
 '(line-number-mode nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (elfeed gotham-theme subatomic-theme jedi dash smartparens)))
 '(package-user-dir "/Users/lche/.emacs.d/elpa")
 '(password-cache-expiry 120)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(powerline-default-separator (quote arrow))
 '(read-quoted-char-radix 16)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-separator (quote (0.5)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

