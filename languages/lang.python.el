;;; lang.python.el --- Python language tooling and config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration for Python usage in Emacs

;;; Code:
(use-package python-mode
  :straight t
  :mode "\\py\\'"
  :config
  (when (hostname? "Vreni")
    (setq-default python-shell-interpreter "~/.pyenv/versions/3.7.2/bin/python3"))
  (when (hostname? "Cube")
    (setq-default python-shell-interpreter "C:/Program Files/Python310/python.exe")))

(use-package company-jedi
  :disabled
  :straight t
  :config
  (setq jedi:complete-on-dot t)
  :hook (python-mode . (lambda () (push 'company-jedi company-backends))))

(use-package pippel
  :straight t)

(use-package lsp-python-ms
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))

(provide 'lang.python)
;;; lang.python.el ends here
