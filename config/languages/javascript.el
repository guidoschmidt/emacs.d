;;; javascript-web.el --- Javascript & co. setup
;;; Commentary:
;; Setup web development tools

;;; Code:
;;; --- Javascript
(use-package indium
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js-mode #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;;; --- JSON
(use-package json-mode
  :ensure t
  :config
  (setq js-indent-level 2))

;;; --- Twig templates
(use-package twig-mode
  :ensure t)

;;; --- Typoscript
(use-package typoscript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.ts\\'" . typoscript-mode)))

;;; --- Vue.js
(use-package vue-mode
  :ensure t
  :config
  (defun emmet/vue-mode-hook ()
    (emmet-mode))
  (add-hook 'vue-mode-hook 'emmet/vue-mode-hook)
  (setq mmm-submode-decoration-level 0)
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

;;; --- JSX & React
(use-package rjsx-mode
  :ensure t
  :config
  (use-package react-snippets :ensure t)
  (defun emmet/rjsx-mode-hook ()
    (setq-default emmet-expand-jsx-className? t)
    (emmet-mode))
  (add-hook 'rjsx-mode-hook 'emmet/rjsx-mode-hook)
  (setq-default rjsx-indent-level 2)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

;;; --- Elm
(use-package elm-mode
  :ensure t
  :config
  (use-package elm-yasnippets :ensure t)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (defun company/elm-mode-hook ()
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook 'company/elm-mode-hook)
  (custom-set-variables '(elm-format-on-save t)))

(use-package flycheck-elm
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

;;; --- Tern
(use-package tern
  :ensure t)

(use-package company-tern
  :ensure t
  :config
  (defun company/js-mode-hook ()
    (tern-mode t)
    (push 'company-tern company-backends))
  (add-hook 'js-mode-hook 'company/js-mode-hook)
  (add-hook 'js2-mode-hook 'company/js-mode-hook))

(use-package tern-auto-complete
  :ensure t)

(provide 'javascript-web.el)
;;; javascript.el ends here
