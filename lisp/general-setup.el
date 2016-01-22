;;Initialization and configuration of variables

;; Set the starting position and width and height of Emacs Window
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 175))

;;silence beeping
(setq ring-bell-function #'ignore)

;;No new frames
(setq ns-pop-up-frames nil)

;;Killring completion
(defun konix/kill-ring-insert ()
  (interactive)
  (let ((to_insert (completing-read "Yank : "
                                    (delete-duplicates kill-ring :test #'equal))))
    (when (and to_insert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert to_insert)))
(global-set-key "\M-y" 'konix/kill-ring-insert)

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

;; Display continuous lines
(setq-default truncate-lines nil)
;; Do not use tabs for indentation
(setq-default indent-tabs-mode nil)
;;set tab width
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(menu-bar-mode t)

;; truncate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)
 
;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;;backups and auto-saves in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;syntax highlighting
(global-font-lock-mode 1)

;;line wrapping
(global-visual-line-mode t)

;; colored shell commands
(add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;persistency
;;save the place in files
(setq-default save-place t)

;; show recent files
(recentf-mode 1)
(setq recentf-max-menu-items 1000)

;; save minibuffer history
(savehist-mode t)

;; Use the system clipboard
(setq x-select-enable-clipboard t)

;; newline and indent on return
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;; web indent
(setq-default web-mode-markup-indent-offset tab-width)
(setq-default web-mode-css-indent-offset tab-width)
(setq-default web-mode-code-indent-offset tab-width)
(setq-default web-mode-sql-indent-offset tab-width)

;;Overwrite selections
(delete-selection-mode 1)

;; Custom keybinds
(global-set-key (kbd "C-j") 'goto-line)
(global-set-key (kbd "C-/") 'undo)

;;Highligh parentheses
(add-hook 'prog-mode-hook #'show-paren-mode)

;;Disable built in scroll
(setq auto-window-vscroll nil)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)
