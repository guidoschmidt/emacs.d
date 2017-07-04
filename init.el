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
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "f23a961abba42fc5d75bf94c46b5688c52683c02b3a81313dd0738b4d48afd1d" "2d16a5d1921feb826a6a9b344837c1ab3910f9636022fa6dc1577948694b7d84" "24685b60b28b071596be6ba715f92ed5e51856fb87114cbdd67775301acf090d" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" default)))
 '(elm-format-on-save t)
 '(irony-additional-clang-options
   (quote
    ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
 '(package-selected-packages
   (quote
    (dockerfile-mode twilight-theme gruvbox-theme twig-mode company-restclient company-quickhelp company-restcliennt fill-column-indicator fci-mode company-gtags ggtags gradle-mode wrap-region twilight-brigtht-theme cmake-ide flycheck-irony google-c-style flycheck-google-cpplint nlinum-hl nlinum yaml-mode which-key vue-mode use-package undo-tree twilight-bright-theme try sublime-themes stylus-mode spaceline soothe-theme smart-tabs-mode skewer-mode sass-mode restclient rainbow-mode rainbow-delimiters processing-mode org-bullets neotree multiple-cursors markdown-mode+ magit hlint-refactor haskell-snippets haskell-mode glsl-mode flyspell-popup flyspell-correct flycheck-pos-tip fic-mode exec-path-from-shell emmet-mode elpy elm-mode editorconfig dumb-jump counsel-projectile company-tern company-php company-jedi company-c-headers cider all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-forground :height 1.5)))))
