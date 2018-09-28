;;; lang.javascript --- Javascript & co. setup

;;; Commentary:
;; Setup web development tools
;; TODO:
;; - Refactor different languages (like Elm) into their own lang config files

;;; Code:

;;; Javascript
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global.
src: http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (if (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)
      (setq-local flycheck-javascript-eslint-executable
                  "~/.nvm/versions/node/v10.11.0/bin/eslint"))))

(use-package indium
  :ensure t
  :commands (indium-run-chrome indium-run-node)
  :config
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  :hook (flycheck-mode . my/use-eslint-from-node-modules))

(use-package js2-refactor
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  :hook ((js2-mode . js2-refactor-mode)
         (js-mode . js2-refactor-mode)))

;;; Javascript docstrings
(add-to-list 'load-path "~/.emacs.d/config/languages/javascript/")
(load "nd-js")
(add-hook 'js-mode-hook
          (lambda () (local-set-key (kbd "C-c d") #'nd-js-doc)))


;;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))


;;; Twig templates
(use-package twig-mode
  :ensure t
  :mode "\\.twig\\'")


;;; Typoscript
(use-package typoscript-mode
  :ensure t
  :mode "\\.ts\\'")


;;; Vue.js
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (defun emmet/vue-mode-hook ()
    (emmet-mode))
  (add-hook 'vue-mode-hook 'emmet/vue-mode-hook)
  (defun company/vue-mode-hook ()
    (add-to-list 'company-backends '(company-tern :with company-yasnippet))
    (add-to-list 'company-backends '(company-css :with company-yasnippet)))
  (add-hook 'vue-mode-hook 'company/vue-mode-hook)
  (setq mmm-submode-decoration-level 0)
  (flycheck-add-mode 'javascript-eslint 'vue-mode))


;;; JSX & React
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (use-package react-snippets :ensure t)
  (defun emmet/rjsx-mode-hook ()
    (setq-default emmet-expand-jsx-className? t)
    (emmet-mode))
  (add-hook 'rjsx-mode-hook 'emmet/rjsx-mode-hook)
  (setq-default rjsx-indent-level 2)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))


;;; Elm
(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :commands elm-mode
  :config
  (use-package elm-yasnippets :ensure t)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (defun company/elm-mode-hook ()
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook 'company/elm-mode-hook)
  (custom-set-variables '(elm-format-on-save t)))

(use-package flycheck-elm
  :ensure t
  :commands flycheck-elm-setup
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

;;; Flow
(use-package company-flow
  :ensure t
  :commands company-flow
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-flow)))

;;; Tern
(use-package tern
  :ensure t
  :disabled
  :commands company-tern)

(use-package company-tern
  :ensure t
  :disabled
  :commands company-tern
  :config
  (defun company/js-mode-hook ()
    (tern-mode t)
    (add-to-list 'company-backends '(company-tern :with company-yasnippet))
    (add-to-list 'company-backends '(company-css :with company-yasnippet)))
  (add-hook 'js-mode-hook 'company/js-mode-hook)
  (add-hook 'js2-mode-hook 'company/js-mode-hook)
  (add-hook 'vue-mode-hook 'company/js-mode-hook)
  (add-hook 'rjsx-mode-hook 'company/js-mode-hook))

;;; Prettier.js
(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-command "~/.nvm/versions/node/v9.4.0/bin/vue-prettier")
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (defun enable-minor-mode (my-pair)
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  (add-hook 'rjsx-mode-hook #'(lambda ()
                                (enable-minor-mode
                                 '("\\.jsx?\\'" . prettier-js-mode))))
  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode
                                '("\\.jsx?\\'" . prettier-js-mode)))))

;;; Language server client for JavaScript
(defun my-company-transformer (candidates)
  "Custom company transformer for CANDIDATES."
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my-js-hook nil
  "Custom hook for using custom company transformer."
  (make-local-variable 'company-transformers)
  (push 'my-company-transformer company-transformers))

(use-package lsp-javascript-typescript
  :ensure t
  :config
  (add-hook 'js-mode-hook         #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'js3-mode-hook        #'lsp-javascript-typescript-enable)
  (add-hook 'rjsx-mode            #'lsp-javascript-typescript-enable)
  (add-hook 'js-mode-hook 'my-js-hook)
  (add-hook 'rjsx-mode-hook 'my-js-hook))

(use-package lsp-typescript
  :ensure t
  :disabled
  :config
  (add-hook 'js-mode-hook  #'lsp-typescript-enable)
  (add-hook 'js2-mode-hook #'lsp-typescript-enable)
  (add-hook 'rjsx-mode     #'lsp-typescript-enable))
  

(provide 'lang.javascript)
;;; lang.javascript ends here
