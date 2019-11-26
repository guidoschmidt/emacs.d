;;; lang.csharp --- Setup C# language

;;; Commentary:

;;; Code:
(defun csharp-endline ()
  "Insert semicolon and move point/cursor to the next line."
  (interactive)
  (insert ";")
  (newline)
  (move-beginning-of-line 1))

(use-package csharp-mode
  :ensure t
  :mode ("\\.cs\\'" "\\.fx\\'")
  :config
  (add-hook 'omnisharp-mode-hook '(lambda () (lsp-mode 0)))
  :bind
  (("<C-return>" . csharp-endline)))

(use-package omnisharp
  :ensure t
  :disabled
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  :hook (csharp-mode . omnisharp-mode))

(provide 'lang.csharp)
;;; lang.csharp ends here
