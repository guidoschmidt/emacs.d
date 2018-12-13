;;; layer.lsp --- Language server protocol client
;;; Commentary:

;;; Code:
(use-package lsp-mode
  :ensure t
  :hook (lsp-after-open . lsp-enable-imenu))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package lsp-symbol-outline
  :straight
  (lsp-symbol-outline
   :type git
   :host github
   :repo "bizzyman/LSP-Symbol-Outline")
  :init
  (progn
    (use-package outline-magic
      :ensure t)
    (use-package cquery
      :ensure t)
    (require 'lsp-symbol-outline-C)))

(provide 'layer.lsp)
;;; layer.lsp ends here
