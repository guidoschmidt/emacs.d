;;; haskell.el --- Haskell settings
;;; Commentary:

;;; Code:
(use-package haskell-mode
  :commands haskell-mode
  :config
  (use-package hindent :ensure t)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map [f8]
       'haskell-navigate-imports))
  (let ((my-cabal-path
         (expand-file-name "~/.cabal/bin")))
    (setenv "PATH"
            (concat
             my-cabal-path
             path-separator
             (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (custom-set-variables '(haskell-tags-on-save t))
  (add-hook 'haskell-interactive-mode-hook 'company-mode))

(use-package company-ghc
  :commands haskell-mode
  :config
  (defun company/haskell-mode-hook ()
    (push 'company-ghc company-backends))
  (add-hook 'haskell-mode-hook
            'company/haskell-mode-hook))

(use-package company-ghci
  :commands haskell-mode
  :config
  (defun company/interactive-haskell-mode-hook ()
    (push 'company-ghci company-backends))
  (add-hook 'haskell-interactive-mode-hook
            'company/interactive-haskell-mode-hook))

(use-package intero
  :commands haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  :bind
  (("C-c i" . intero-company)))

(use-package haskell-snippets
  :commands haskell-mode)

(use-package flycheck-haskell
  :commands haskell-mode
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(provide 'haskell.el)
;;; haskell.el ends here
