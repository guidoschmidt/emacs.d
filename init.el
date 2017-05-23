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
 '(elm-format-on-save t)
 '(package-selected-packages
   (quote
    (smart-tabs-mode sublime-themes elpy skewer-mode vue-mode processing-mode elm-mode hlint-refactor haskell-snippets haskell-mode sass-mode stylus-mode glsl-mode yaml-mode markdown-mode+ fic-mode restclient flyspell-popup flyspell-correct multiple-cursors company-tern company-jedi company-c-headers company-php magit git-commit rainbow-mode emmet-mode ace-window exec-path-from-shell org-bullets tern undo-tree dumb-jump counsel-projectile projectile rainbow-delimiters neotree spaceline powerline yasnippet all-the-icons flycheck-pos-tip editorconfig which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
