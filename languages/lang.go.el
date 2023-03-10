;;; lang.go.el --- Golang setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Setup golang mode + eglot for gopls language server

;;; Code:
(use-package go-mode
  :straight t
  :config
  (add-hook 'go-mode-hook #'eglot-ensure))

(provide 'lang.go)
;;; lang.go.el ends here
