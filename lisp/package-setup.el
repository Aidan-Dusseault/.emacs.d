;;Initialization and configuration for packages
;;Contains:
;;Auto-complete
;;Emmet-mode
;;Flycheck
;;Haskell-mode
;;Indent-guide
;;Linum
;;Magit
;;Mic-paren
;;Multi-term
;;Pabbrev
;;Powerline
;;Projectile
;;Rainbow-delimiters
;;Rainbow-mode
;;Smartparens
;;Swiper
;;Sr-speedbar
;;Undo-tree
;;Uniquify
;;Yasnippet
;;Web-mode


;;Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(define-key ac-completing-map (kbd "A-K") 'ac-next)
(define-key ac-completing-map (kbd "A-I") 'ac-previous)
(define-key ac-mode-map (kbd "A-h") 'auto-complete)
(define-key ac-completing-map (kbd "A-H") 'ac-isearch)

;;Emmet-mode
(add-hook 'sgml-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  #'emmet-mode) ;; enable Emmet's css abbreviation.

;;Flycheck
(global-flycheck-mode)

;;Haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;Indent-guide
(add-hook 'prog-mode-hook #'indent-guide-mode)
(setq indent-guide-recursive t)

;; Line numbering
(add-hook 'prog-mode-hook #'linum-mode)
(setq linum-format "%d ")

;;Magit
(global-set-key (kbd "A-g") 'magit-status)
(global-set-key (kbd "A-G") 'magit-dispatch-popup)

;;Mic-paren
(paren-activate)

;;Multi-term
(if (not (eq system-type 'windows-nt))
    (setq multi-term-program "/bin/bash")
  (defun multi-term-get-buffer (&optional special-shell dedicated-window)
    "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input.
If option DEDICATED-WINDOW is `non-nil' will create dedicated `multi-term' window ."
    (with-temp-buffer
      (let ((shell-name (or multi-term-program ;shell name
                            (getenv "SHELL")
                            (getenv "ESHELL")
                            "/bin/sh"))
            (index 1)                     ;setup new term index
            term-name)                    ;term name
        (if dedicated-window
            (setq term-name multi-term-dedicated-buffer-name)
          ;; Compute index.
          (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
            (setq index (1+ index)))
          ;; switch to current local directory,
          ;; if in-existence, switch to `multi-term-default-dir'.
          (cd (or default-directory (expand-file-name multi-term-default-dir)))
          ;; adjust value N when max index of term buffer is less than length of term list
          (setq term-name (format "%s<%s>" multi-term-buffer-name index)))
        ;; Try get other shell name if `special-shell' is non-nil.
        (if special-shell
            (setq shell-name (read-from-minibuffer "Run program: " shell-name)))
        ;; Make term, details to see function `make-term' in `term.el'.
        (shell nil (concat "*" term-name "*")))))
    )
(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-dedicated-close-back-to-open-buffer-p t)
(global-set-key (kbd "M-'") 'multi-term-dedicated-toggle)
(global-set-key (kbd "A-'") 'multi-term-dedicated-toggle)

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
      ;; if graphic
    (powerline-default-theme)
  ;; else (optional)
  (powerline-vim-theme))
;;Correct colours on osx
(if (eq window-system 'ns)
    (setq ns-use-srgb-colorspace nil)
  )

;;Rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;Rainbow-mode
(add-hook 'prog-mode-hook #'rainbow-mode)

;;Smartparens
(smartparens-global-mode t)

;;Swiper
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
(define-key key-translation-map (kbd "A-n") (kbd "C-s"))
(define-key key-translation-map (kbd "A-N") (kbd "C-r"))

;;Sr-speedbar
(global-set-key (kbd "M-s") 'sr-speedbar-toggle)

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
(define-key yas-minor-mode-map (kbd "M-j") 'yas-expand)

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
