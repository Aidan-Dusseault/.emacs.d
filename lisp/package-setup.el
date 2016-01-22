;;Initialization and configuration for packages
;;Contains:
;;Aggressive-indent
;;Auto-complete
;;Emmet-mode
;;Flycheck
;;Haskell-mode
;;Ido
;;Indent-guide
;;Linum
;;Magit
;;Mic-paren
;;Powerline
;;Projectile
;;Rainbow-delimiters
;;Rainbow-mode
;;Smartparens
;;Smooth-scrolling
;;Undo-tree
;;Uniquify
;;Yasnippet
;;Web-mode

;;Aggressive-indent
(add-hook 'prog-mode-hook #'aggressive-indent-mode)

;;Auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;;Emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;Flycheck
(require 'flycheck)
(global-flycheck-mode)

;;Haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;Ido
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;Indent-guide
(require 'indent-guide)
(add-hook 'prog-mode-hook #'indent-guide-mode)
(setq indent-guide-recursive t)

;; Line numberingg
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%2d \u2502 ")

;;Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;Mic-paren
(require 'mic-paren)
(paren-activate)

;;Projectile
(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

;;Powerline
(require 'powerline)
(powerline-vim-theme)

;;Rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;Rainbow-mode
(require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;;Smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;;Smooth-scrolling
(require 'smooth-scrolling)

;; Undo-tree
(global-undo-tree-mode t)

;;Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;Yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(add-to-list 'yas/root-directory "~/.emacs.d/git/yasnippet-snippets")

;;Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(setq web-mode-enable-auto-pairing nil)
