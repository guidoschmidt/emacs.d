;;; lang.javascript.el --- Javascript setup -*- lexical-binding: t; -*-

;;; Commentary:
;;; Relies on rjsx mode + tooling via prettier.js and eslint via flycheck

;;; Code:
(use-package json-mode
  :straight t
  :mode "\\.json\\'")

(defun emmet/jsts-mode-hook ()
  "Setup emmet in js/ts/jsx/tsx modes."
  (setq-default emmet-expand-jsx-className? t)
  (emmet-mode))

(use-package rjsx-mode
  :straight t
  :config
  (add-hook 'rjsx-mode-hook 'emmet/jsts-mode-hook)
  :init
  (setq-default rjsx-indent-level 2))

(defvar flycheck-javascript-eslint-executable)
(defun custom/use-eslint-from-node-modules ()
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
                  "~/.nvm/versions/node/v15.4.0/bin/eslint"))))

(use-package react-snippets
  :straight t)

(use-package prettier-js
  :straight t
  :config
  (when (hostname? "gs-cube")
    (setq prettier-js-command "~/.nvm/versions/node/v17.1.0/bin/prettier"))
  (when (hostname? "Vreni")
    (setq prettier-js-command "~/.nvm/versions/node/v17.0.1/bin/prettier"))
  (when (hostname? "Brandon.fritz.box")
    (setq prettier-js-command "~/.nvm/versions/node/v16.9.1/bin/prettier"))
  (when (hostname? "Brandon.local")
    (setq prettier-js-command "~/.nvm/versions/node/v16.9.1/bin/prettier"))
  (when (hostname? "Zenbook-GS")
    (setq prettier-js-command "~/AppData/Roaming/nvm/v14.15.3/prettier"))
  :hook
  (js2-mode        . prettier-js-mode)
  (rjsx-mode       . prettier-js-mode)
  (typescript-mode . prettier-js-mode)
  (html-mode       . prettier-js-mode))

(use-package typescript-mode
  :straight t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (setq company-tooltip-align-annotations t)
  :hook
  (('typescript-mode-hook . 'emmet/jsts-mode-hook)
   ('typescript-mode-hook . 'setup-tide-mode))
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)
         ("\\.js\\'" . typescript-mode)
         ("\\.mjs\\'" . typescript-mode)))

(use-package tide
  :straight t
  :after
  (typescript-mode company flycheck))

(use-package svelte-mode
  :straight t)

(provide 'lang.javascript)
;;; lang.javascript.el ends here
