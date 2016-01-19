(require 'auto-complete-config)
 (add-to-list 'ac-dictionary-directories
     "~/.emacs.d/.cask/24.3.50.1/elpa/auto-complete-20130724.1750/dict")
 (ac-config-default)
 (setq ac-ignore-case nil)

(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
