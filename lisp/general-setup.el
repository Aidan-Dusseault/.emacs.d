;;Initialization and configuration of variables

;;Start server
(server-start)

;;Smart save
(defun smart-save ()
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (save-buffer))
  )

;;silence beeping
(setq ring-bell-function #'ignore)

;;Mac specific
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;;Windows specific
(when (eq system-type 'windows-nt)
  (setq w32-alt-is-meta nil)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-alt-to-system nil)
  (setq w32-lwindow-modifier 'meta) ; Left Windows key
  (setq w32-alt-modifier 'alt)
  (setq w32-register-hot-key [A-q])
  (setq w32-register-hot-key [A-w])
  (setq w32-register-hot-key [A-e])
  (setq w32-register-hot-key [A-a])
  (setq w32-register-hot-key [A-s])
  (setq w32-register-hot-key [A-z])
  (setq w32-register-hot-key [A-x])
  (setq w32-register-hot-key [A-c])
  (setq w32-register-hot-key [A-v])
  (setq w32-register-hot-key [A-f])
  (setq w32-register-hot-key [A-b])
  (setq w32-register-hot-key [A-g])
  (setq w32-register-hot-key [A-h])
  (setq w32-register-hot-key [A-u])
  (setq w32-register-hot-key [A-j])
  (setq w32-register-hot-key [A-n])
  (setq w32-register-hot-key [A-i])
  (setq w32-register-hot-key [A-k])
  (setq w32-register-hot-key [A-o])
  (setq w32-register-hot-key [A-k])
  (setq w32-register-hot-key [A-delete])
  (setq w32-register-hot-key [A-ret])
  )

;;No new frames
(setq ns-pop-up-frames nil)

;;Killring completion
(defun select-kill-ring-insert ()
  (interactive)
  (let ((to_insert (completing-read "Yank : "
                                    (delete-duplicates kill-ring :test #'equal))))
    (when (and to_insert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert to_insert)))

;; set default directory
(setq default-directory "~/")
 
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

;; Set the starting position and width and height of Emacs Window
(setq frame-resize-pixelwise t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;better mouse scrolling
(setq mouse-wheel-scroll-amount '(5 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

(menu-bar-mode nil)

;; truncate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

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
(setenv "EDITOR" "emacsclient -a ''")

;;Clear command in shell
(setq comint-input-sender 'n-shell-simple-send)
(defun n-shell-simple-send (proc command)
  "17Jan02 - sailor. Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command)
    )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )

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

;;Disable shift selection
(setq shift-select-mode nil)

;;Scroll multiple lines
(defun scroll-down-multi-line ()
  (interactive)
  (dotimes (number 10)
    (scroll-down-line))
  )

(defun scroll-up-multi-line ()
  (interactive)
  (dotimes (number 10)
    (scroll-up-line))
  )

(defun backward-delete-word ()
  (interactive)
  (backward-kill-word)
  (pop kill-ring)
  )

;; Custom keybindings
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'goto-line)
(global-set-key (kbd "M-p") 'scroll-down-multi-line)
(global-set-key (kbd "M-n") 'scroll-up-multi-line)
(global-set-key (kbd "M-i") 'scroll-down-multi-line)
(global-set-key (kbd "M-k") 'scroll-up-multi-line)
(global-set-key (kbd "A-<backspace>") 'backward-delete-word)
(global-set-key (kbd "A-S-<backspace>") 'kill-whole-line)
(define-key key-translation-map (kbd "A-<return>") (kbd "RET"))
(global-set-key (kbd "A-j") 'backward-char)
(global-set-key (kbd "A-J") 'backward-word)
(global-set-key (kbd "A-l") 'forward-char)
(global-set-key (kbd "A-L") 'forward-word)
(define-key key-translation-map (kbd "A-i") (kbd "C-p"))
(define-key key-translation-map (kbd "A-I") (kbd "M-p"))
(define-key key-translation-map (kbd "A-k") (kbd "C-n"))
(define-key key-translation-map (kbd "A-K") (kbd "M-n"))
(define-key key-translation-map (kbd "A-u") (kbd "C-a"))
(define-key key-translation-map (kbd "A-U") (kbd "M-<"))
(define-key key-translation-map (kbd "A-o") (kbd "C-e"))
(define-key key-translation-map (kbd "A-O") (kbd "M->"))
(define-key key-translation-map (kbd "A-n") (kbd "C-s"))
(define-key key-translation-map (kbd "A-N") (kbd "C-r"))
(define-key key-translation-map (kbd "A-m") (kbd "M-m"))
(global-set-key (kbd "A-;") 'goto-line)
(global-set-key (kbd "A-:") 'move-to-column)
(define-key key-translation-map (kbd "A-,") (kbd "M-;"))
(global-set-key (kbd "A-z") 'undo)
(global-set-key (kbd "A-Z") 'redo)
(global-set-key (kbd "A-/") 'undo-tree-visualize)
(global-set-key (kbd "A-x") 'kill-region)
(global-set-key (kbd "A-c") 'copy-region-as-kill)
(global-set-key (kbd "A-v") 'yank)
(global-set-key (kbd "A-V") 'select-kill-ring-insert)
(global-set-key (kbd "A-a") 'mark-whole-buffer)
(global-set-key (kbd "A-s") 'smart-save)
(global-set-key (kbd "A-S") 'write-file)
(define-key key-translation-map (kbd "A-q") (kbd "C-g"))
(define-key key-translation-map (kbd "A-w") (kbd "TAB"))
(global-set-key (kbd "A-p") 'set-mark-command)
(define-key key-translation-map (kbd "A-e") (kbd "M-x"))
(define-key key-translation-map (kbd "A-b") (kbd "C-x b"))
(global-set-key (kbd "A-B") 'kill-buffer)
(define-key key-translation-map (kbd "A-f") (kbd "C-x C-f"))
(define-key key-translation-map (kbd "A-0") (kbd "C-0"))
(define-key key-translation-map (kbd "A-1") (kbd "C-1"))
(define-key key-translation-map (kbd "A-2") (kbd "C-2"))
(define-key key-translation-map (kbd "A-3") (kbd "C-3"))
(define-key key-translation-map (kbd "A-4") (kbd "C-4"))
(define-key key-translation-map (kbd "A-5") (kbd "C-5"))
(define-key key-translation-map (kbd "A-6") (kbd "C-6"))
(define-key key-translation-map (kbd "A-7") (kbd "C-7"))
(define-key key-translation-map (kbd "A-8") (kbd "C-8"))
(define-key key-translation-map (kbd "A-9") (kbd "C-9"))

;;Disable built in scroll
(setq auto-window-vscroll nil)

;;Tramp ssh for Unix sockets
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=%%C "
       "-o ControlMaster=auto -o ControlPersist=no"))

;;Code folding
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display 
   (if selective-display nil (or column 1))))
(global-set-key (kbd "M-.") 'toggle-selective-display)

;;Make directories when using find-file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))
