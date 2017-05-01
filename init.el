;;; init.el --- Main entry for Emacs configuration
;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;;; Code:
;;; --- Load configuration files


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;; --- Outsourced configuration files
(load "~/.emacs.d/config/pacman.el")
(load "~/.emacs.d/config/packages.el")
(load "~/.emacs.d/config/core.el")
(load "~/.emacs.d/config/ligatures.el")
(load "~/.emacs.d/config/keymap.el")
(load "~/.emacs.d/config/appearance.el")

(provide 'init.el)
;;; init.el ends here

;;; --- Automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("25e33cc1ce8b45b9712467dab8a42787bd7bbcf8a1e665d4c1dcbba9be71767c" "bc40f613df8e0d8f31c5eb3380b61f587e1b5bc439212e03d4ea44b26b4f408a" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(elm-format-on-save t)
 '(package-selected-packages
   (quote
    (flycheck-haskell company-ghc company-cabal dumb-jump dump-jump counsel-projectile glsl-mode ujellly-theme markdown-mode+ restclient restclient-mode haskell-mode neotree all-the-icons flyspell-correct flyspell-popup company-php t: company elm-mode php-mode color-theme-sanityinc-tomorrow solarized-theme flycheck-pos-tip-mode flycheck sass-mode emmet-mode tern-auto-complete tern undo-tree rainbow-delimiters spaceline yasnippet yasnippets exec-path-from-shell better-defaults auto-complete auto-compelete counsel swiper org-bullets which-key use-package try))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
