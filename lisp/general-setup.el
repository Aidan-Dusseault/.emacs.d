;; Set the starting position and width and height of Emacs Window
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 175))

;;silence beeping
(setq ring-bell-function #'ignore)

;;ido
(require 'ido)
(ido-mode t)

;; Line numbering
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; make buffer names unique even if the files have the same names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; set default directory
(setq default-directory "~/")

;better mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
 
;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)
 
;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)
 
;; Use windmove bindings
;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings) 
 
(set-frame-font "-apple-Fira_Mono-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
 
;; Display continuous lines
(setq-default truncate-lines nil)
;; Do not use tabs for indentation
(setq-default indent-tabs-mode nil)
;;set tab width
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)


(menu-bar-mode t)
 
;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)
 
;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)


;;smartparens
(require 'smartparens-config)
(smartparens-global-mode t)


;;backups and auto-saves in temp directory
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


;;powerline
(require 'powerline)
(powerline-default-theme)

;;smooth scrolling
(require 'smooth-scrolling)

;;yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(add-to-list 'yas/root-directory "~/.emacs.d/git/yasnippet-snippets")

;;syntax highlighting
(global-font-lock-mode 1)

;;line wrapping
(global-visual-line-mode t)

;; colored shell commands
(add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;persistency
; save the place in files
(require 'saveplace)
(setq-default save-place t)

;; show recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)

;; save minibuffer history
(require 'savehist)
(savehist-mode t)

;; Use the system clipboard
(setq x-select-enable-clipboard t)

;; newline and indent on return
(add-hook 'prog-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

(require 'mic-paren) ; loading
(paren-activate)     ; activating

;; web indent
(setq-default web-mode-markup-indent-offset tab-width)
(setq-default web-mode-css-indent-offset tab-width)
(setq-default web-mode-code-indent-offset tab-width)
(setq-default web-mode-sql-indent-offset tab-width)

;;emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


;;web-mode
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

(delete-selection-mode 1)


;; Custom keybinds
(global-set-key (kbd "C-M-j") 'goto-line)
(global-set-key (kbd "C-M-u") 'undo)

;; Undo tree
(global-undo-tree-mode t)
