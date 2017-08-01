;;; javascript-web.el --- Javascript & co. setup
;;; Commentary:
;; Setup web development tools

;;; Code:
;;; --- Javascript
(use-package indium
  :ensure t
  :defer t
  :config
  (eval-when-compile
    (add-hook 'js-mode-hook #'indium-interaction-mode)))

(use-package js2-refactor
  :ensure t
  :defer t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js-mode #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;;; --- JSON
(use-package json-mode
  :ensure t
  :defer t
  :config
  (setq js-indent-level 2))

;;; --- Twig templates
(use-package twig-mode
  :ensure t
  :defer t)

;;; --- Typoscript
(use-package typoscript-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.ts\\'" . typoscript-mode)))

;;; --- Vue.js
(use-package vue-mode
  :ensure t
  :defer t
  :config
  (use-package vue-html-mode :ensure t :defer t)
  (setq mmm-submode-decoration-level 0))

;;; --- JSX & React
(use-package rjsx-mode
  :ensure t
  :defer t
  :config
  (defun emmet/rjsx-mode-hook ()
    (setq-default emmet-expand-jsx-className? t)
    (emmet-mode))
  (add-hook 'rjsx-mode-hook 'emmet/rjsx-mode-hook)
  (setq-default rjsx-indent-level 2))

;;; --- Elm
(use-package elm-mode
  :ensure t
  :defer t
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (defun company/elm-mode-hook ()
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook 'company/elm-mode-hook)
  (custom-set-variables '(elm-format-on-save t)))

;;; --- Tern
(use-package tern
  :ensure t
  :defer t
  :init
  (add-to-list
   'load-path
   "~/.nvm/versions/node/v8.1.2/lib/node_modules/tern/")
  (autoload 'tern-mode "tern.el" nil t)
  :config
  (use-package company-tern
    :ensure t
    :defer t)
  (defun tern/js-mode-hook ()
    (add-to-list 'company-backends 'company-tern)
    (tern-mode t))
  (add-hook 'js-mode-hook 'tern/js-mode-hook))

(provide 'javascript-web.el)
;;; javascript.el ends here
