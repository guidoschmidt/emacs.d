;;; layer.dap --- Debug Adapter Protocol
;;; Commentary:

;;; Code:
(use-package dap-mode
  :ensure t
  :config
  (require 'dap-lldb)
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra))))

(provide 'layer.dap)
;;; layer.dap ends here
