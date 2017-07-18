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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" default)))
 '(elm-format-on-save t)
 '(haskell-tags-on-save t)
 '(irony-additional-clang-options
   (quote
    ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
 '(package-selected-packages
   (quote
    (js2-refactor hindent hindent-mode company-intero yaml-mode wrap-region which-key web-mode vue-mode undo-tree typoscript-mode twilight-theme twilight-bright-theme twig-mode try sublime-themes stylus-mode spaceline soothe-theme smart-tabs-mode sass-mode rjsx-mode rainbow-mode rainbow-delimiters php-mode org-bullets no-littering nlinum-hl neotree multiple-cursors markdown-mode+ magit json-mode intero indium html-check-frag gruvbox-theme gradle-mode google-c-style glsl-mode ggtags flyspell-popup flyspell-correct flycheck-pos-tip flycheck-haskell fill-column-indicator fic-mode exec-path-from-shell emmet-mode elpy elm-mode editorconfig dumb-jump doom-themes counsel-projectile company-tern company-rtags company-restclient company-quickhelp company-jedi company-irony-c-headers company-irony company-ghci company-ghc cmake-ide cider ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-forground :height 1.5)))))
