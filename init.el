;; Load other files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(load "general-setup.el")
(load "package-setup.el")
(if (file-readable-p (expand-file-name "~/.emacs.d/lisp/local-settings.el"))
    (load "local-settings.el"))


;;Automatic additions from Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output nil)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
 '(fci-rule-color "#383838")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inhibit-default-init t)
 '(initial-buffer-choice t)
 '(minimap-hide-fringes t)
 '(minimap-highlight-line nil)
 '(minimap-mode t)
 '(minimap-recenter-type (quote free))
 '(minimap-window-location (quote right))
 '(protect-buffer-bury-p nil)
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
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blank-empty ((t nil)))
 '(blank-hspace ((t (:background "grey24" :foreground "gray40"))))
 '(blank-indentation ((t (:background "gray14" :foreground "gray40"))))
 '(blank-line ((t (:background "gray20" :foreground "gray40"))))
 '(blank-newline ((t (:background "gray14" :foreground "gray40" :weight bold))))
 '(blank-space ((t (:background "gray14" :foreground "gray40"))))
 '(blank-space-after-tab ((t (:background "gray20" :foreground "gray40"))))
 '(blank-tab ((t (:background "gray14" :foreground "gray40"))))
 '(blank-trailing ((t (:background "gray20" :foreground "gray40" :weight bold))))
 '(minimap-active-region-background ((t (:background "DarkOrange4")))))
