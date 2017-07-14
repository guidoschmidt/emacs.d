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
    ("24685b60b28b071596be6ba715f92ed5e51856fb87114cbdd67775301acf090d" "2d16a5d1921feb826a6a9b344837c1ab3910f9636022fa6dc1577948694b7d84" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" default)))
 '(elm-format-on-save t)
 '(irony-additional-clang-options
   (quote
    ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
 '(package-selected-packages
   (quote
    (doom-themes yaml-mode wrap-region which-key web-mode vue-mode undo-tree typoscript-mode twilight-theme twilight-bright-theme twig-mode try stylus-mode spaceline soothe-theme smart-tabs-mode sass-mode rjsx-mode rainbow-mode rainbow-delimiters php-mode org-bullets no-littering nlinum-hl neotree multiple-cursors markdown-mode+ magit json-mode indium html-check-frag gruvbox-theme gradle-mode google-c-style glsl-mode ggtags flyspell-popup flyspell-correct flycheck-pos-tip fill-column-indicator fic-mode exec-path-from-shell emmet-mode elpy elm-mode editorconfig dumb-jump dockerfile-mode counsel-projectile company-tern company-restclient company-quickhelp company-jedi company-irony-c-headers company-irony company-ghci cmake-ide cider all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-forground :height 1.5)))))
