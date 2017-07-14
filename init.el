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
 '(ansi-color-names-vector
   ["#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(custom-safe-themes
   (quote
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" default)))
 '(elm-format-on-save t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(irony-additional-clang-options
   (quote
    ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
 '(package-selected-packages
   (quote
    (company-gtags typoscript-mode php-mode json-mode web-mode html-check-frag htm-check-frag rjsx-mode indium yaml-mode wrap-region which-key websocket vue-mode use-package undo-tree twilight-theme twilight-bright-theme twig-mode try stylus-mode spaceline smart-tabs-mode sass-mode rainbow-mode rainbow-delimiters org-bullets no-littering nlinum-hl neotree multiple-cursors markdown-mode+ magit gruvbox-theme gradle-mode google-c-style glsl-mode ggtags flyspell-popup flyspell-correct flycheck-pos-tip fill-column-indicator fic-mode exec-path-from-shell emmet-mode elpy elm-mode editorconfig dumb-jump dockerfile-mode counsel-projectile company-tern company-restclient company-quickhelp company-jedi company-irony-c-headers company-irony company-ghci cmake-ide cider all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-forground :height 1.5)))))
