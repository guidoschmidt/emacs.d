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
    :config
    (push 'company-lsp company-backends))

(use-package lsp-symbol-outline
  :load-path "~/.emacs.d/github/lsp-symbol-outline"
  :init
  (progn
    (use-package outline-magic
      :ensure t)
    (use-package cquery
      :ensure t)
    (require 'lsp-symbol-outline-C)))

(provide 'layer.lsp)
;;; layer.lsp ends here
