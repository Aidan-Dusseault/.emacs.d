;;Initialization and configuration for packages
;;Contains:
;;Auto-complete
;;Emmet-mode
;;Flycheck
;;Haskell-mode
;;Ido
;;Indent-guide
;;Linum
;;Magit
;;Powerline
;;Projectile
;;Rainbow-delimiters
;;Rainbow-mode
;;Smartparens
;;Undo-tree
;;Uniquify
;;Yasnippet
;;Web-mode

;;Auto-complete
;; (ac-config-default)
;; (add-to-list 'load-path "~/.emacs.d/ac-dict")

;;Emmet-mode
(add-hook 'sgml-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  #'emmet-mode) ;; enable Emmet's css abbreviation.

;;Flycheck
(global-flycheck-mode)

;;Haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;Ido
(ido-mode t)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;Indent-guide
(add-hook 'prog-mode-hook #'indent-guide-mode)
(setq indent-guide-recursive t)

;; Line numbering
(add-hook 'prog-mode-hook #'linum-mode)
(setq linum-format "%2d \u2502 ")

;;Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;Pabbrev
(add-hook 'prog-mode-hook #'pabbrev-mode)

;;Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

;;Powerline
(if (display-graphic-p)
    (progn
      ;; if graphic
      (powerline-default-theme))
  ;; else (optional)
  (powerline-vim-theme))

;;Rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;Rainbow-mode
(add-hook 'prog-mode-hook #'rainbow-mode)

;;Smartparens
(smartparens-global-mode t)

;; Undo-tree
(global-undo-tree-mode t)

;;Uniquify
(setq uniquify-buffer-name-style 'forward)

;;Yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(add-to-list 'yas/root-directory "~/.emacs.d/git/yasnippet-snippets")
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M- ") 'yas-expand)

;;Web-mode
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
