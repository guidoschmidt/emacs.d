;;; javascript-web.el --- Javascript & co. setup
;;; Commentary:
;; Setup web development tools

;;; Code:
;;; --- Javascript
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global.
src: http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package indium
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

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
  (defun company/vue-mode-hook ()
    (add-to-list 'company-backends 'company-tern)
    (add-to-list 'company-backends 'company-css))
  (add-hook 'vue-mode-hook 'company/vue-mode-hook)
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

;;; --- Flow
(use-package company-flow
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-flow)))

;;; --- Tern
(use-package tern
  :ensure t
  :ensure-system-package (tern . "npm i -g tern"))

(use-package company-tern
  :ensure t
  :config
  (defun company/js-mode-hook ()
    (tern-mode t)
    (push 'company-tern company-backends))
  (add-hook 'js-mode-hook 'company/js-mode-hook)
  (add-hook 'js2-mode-hook 'company/js-mode-hook)
  (add-hook 'vue-mode-hook 'company/js-mode-hook))

;;; --- Prettier.js
;; (use-package prettier-js
;;   :load-path "~/.emacs.d/github/prettier-emacs/"
;;   :ensure
;;   :config
;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
;;   (add-hook 'web-mode-hook 'prettier-js-mode)
;;   (add-hook 'vue-mode-hook 'prettier-js-mode)
;;   (defun enable-minor-mode (my-pair)
;;     (if (buffer-file-name)
;;         (if (string-match (car my-pair) buffer-file-name)
;;             (funcall (cdr my-pair)))))
;;   (add-hook 'rjsx-mode #'(lambda ()
;;                            (enable-minor-mode
;;                             '("\\.jsx?\\'" . prettier-js-mode)))))

;; (defun prettier-vue ()
;;   (interactive)
;;   (let ((original (point)))
;;     (goto-char 0)
;;     (let* ((script-start (re-search-forward "<script>" nil t))
;;            (start (+ script-start 1))
;;            (script-end (re-search-forward "</script>" nil t))
;;            (end (- script-end 10)))
;;       (prettier-js--prettify start end)
;;       (goto-char original))))

(use-package web-beautify :ensure t
  :ensure-system-package (tern . "npm i -g js-beautify"))

(provide 'javascript-web.el)
;;; javascript.el ends here
