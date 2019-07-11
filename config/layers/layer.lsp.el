;;; layer.lsp --- Language server protocol client
;;; Commentary:

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure t
  :hook (prog-mode . lsp)
  :config
  (lsp--persist-session (lsp-session)))

(use-package lsp-ui :commands lsp-ui-mode :ensure t)

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

;; @TODO
(use-package dap-mode
  :ensure t)

(provide 'layer.lsp)
;;; layer.lsp ends here
