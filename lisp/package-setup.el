(require 'package)
(package-initialize)
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
 'aggressive-indent
 'auto-complete
 'emmet-mode
 'flycheck
 'flx-ido
 'grizzl
 'ido
 'ido-ubiquitous
 'ido-vertical-mode
 'indent-guide
 'linum
 'lua-mode
 'mic-paren
 'popup-kill-ring
 'powerline
 'projectile
 'rainbow-delimiters
 'smart-mode-line
 'smartparens
 'smooth-scrolling
 'undo-tree
 'web-mode
 'yasnippet
 ) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)
