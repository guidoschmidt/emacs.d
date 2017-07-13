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
    ("5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" default)))
 '(elm-format-on-save t)
 '(irony-additional-clang-options
   (quote
    ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
 '(package-selected-packages
   (quote
    (doom-themes rjsx-mode indium yaml-mode wrap-region which-key websocket vue-mode use-package undo-tree twilight-theme twilight-bright-theme twig-mode try stylus-mode spaceline smart-tabs-mode sass-mode rainbow-mode rainbow-delimiters org-bullets no-littering nlinum-hl neotree multiple-cursors markdown-mode+ magit gruvbox-theme gradle-mode google-c-style glsl-mode ggtags flyspell-popup flyspell-correct flycheck-pos-tip fill-column-indicator fic-mode exec-path-from-shell emmet-mode elpy elm-mode editorconfig dumb-jump dockerfile-mode counsel-projectile company-tern company-restclient company-quickhelp company-jedi company-irony-c-headers company-irony company-ghci cmake-ide cider all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-forground :height 1.5)))))
