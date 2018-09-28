;;; layer.lsp.el ‒-- Language server protocol client
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
    :config
    (push 'company-lsp company-backends))

(provide 'layer.lsp)
;;; layer.lsp ends here
