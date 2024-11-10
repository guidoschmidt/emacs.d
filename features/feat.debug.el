;;; feat.debug.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; 

;;; Code:
(use-package dap-mode
  :straight t
  :defer
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (add-hook 'dap-mode-hook
            (lambda () (set-fringe-style '(20 . 8))))
  (dap-auto-configure-mode t)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))

  (require 'dap-lldb)
  ;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("/opt/homebrew/opt/llvm/bin/lldb-dap"))
  ;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug."))))

(provide 'feat.debug)
;;; feat.debug.el ends here
