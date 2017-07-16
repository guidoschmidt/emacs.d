;;; haskell.el --- Haskell settings
;;; Commentary:

;;; Code:
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (use-package hindent :ensure t :defer t)
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

;; (use-package intero
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   (eval-after-load 'company
;;     (add-to-list 'company-backends 'company-intero)))

(use-package company-ghc
  :ensure t
  :defer t
  :config
  (defun company/haskell-mode-hook ()
    (add-to-list 'company-backends 'company-ghc))
  (add-hook 'haskell-mode-hook
            'company/haskell-mode-hook))

(use-package company-ghci
  :ensure t
  :defer t
  :config
  (defun company/haskell-mode-hook ()
    (add-to-list 'company-backends 'company-ghci))
  (add-hook 'haskell-interactive-mode-hook
            'company/haskell-mode-hook))

(use-package haskell-snippets
  :ensure t
  :defer t)

(provide 'haskell.el)
;;; haskell.el ends here
