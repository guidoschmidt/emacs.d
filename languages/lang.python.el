;;; lang.python.el --- Python language tooling and config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration for Python usage in Emacs

;;; Code:
(use-package python-mode
  :straight t
  :mode "\\py\\'"
  :hook ((python-mode . lsp-deferred))
  :config
  (when (hostname? "Vreni")
    (setq-default python-shell-interpreter "~/.pyenv/versions/3.7.2/bin/python3"))
  (when (hostname? "Cube")
    (setq-default python-shell-interpreter "C:/Program Files/Python311/python.exe"))
  ;; LSP config for ruff
  ;; Use shopify-cli / theme-check-language-server for Shopify's liquid syntax.
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(python-mode . "python"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "ruff")
                      :activation-fn (lsp-activate-on "python")
                      :server-id 'ruff))))

(use-package pippel
  :straight t)

(provide 'lang.python)
;;; lang.python.el ends here
