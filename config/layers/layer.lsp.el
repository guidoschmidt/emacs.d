;;; layer.lsp --- Language server protocol client
;;; Commentary:

;;; Code:

(use-package lsp-mode
  :commands lsp
  :ensure t
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

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-gcc
                                             c/c++-cppcheck
                                             c/c++-clang))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(provide 'layer.lsp)
;;; layer.lsp ends here
