;;Initialization and configuration of variables

;; Set the starting position and width and height of Emacs Window
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
;;Initialization and configuration of variables

;; Set the starting position and width and height of Emacs Window
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

(menu-bar-mode nil)

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
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
 )

;;Don't use pager in shell
(setenv "PAGER" "cat")

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

;;Disable toolbar and scrollbar
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
)

;; Custom keybindings
(global-set-key (kbd "C-j") 'goto-line)
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)
(global-set-key (kbd "C-o") 'universal-argument)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-M-<backspace>") 'kill-whole-line)

;;Highligh parentheses
;; (add-hook 'prog-mode-hook #'show-paren-mode)

;;Disable built in scroll
(setq auto-window-vscroll nil)

;;Tramp ssh for Unix sockets
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=%%C "
       "-o ControlMaster=auto -o ControlPersist=no"))

;;Mouse scroll speed
(setq mouse-wheel-scroll-amount '(5))

;;Hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;;Code folding
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display 
   (if selective-display nil (or column 1))))
(global-set-key (kbd "M-.") 'toggle-selective-display)
