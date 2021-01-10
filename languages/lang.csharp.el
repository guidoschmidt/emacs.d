;;; lang.csharp.el --- C# setup -*- lexical-binding: t; -*-

;;; Commentary:
;;; C# language configuration

;;; Code:

(use-package csharp-mode
  :straight t
  :mode "\\.cs\\'"
  :hook (omnisharp-mode . (lambda () (lsp))))

(use-package omnisharp
  :straight t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  :hook (csharp-mode . omnisharp-mode))

(provide 'lang.csharp)
;;; lang.csharp.el ends here
