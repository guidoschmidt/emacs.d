;;; feat.debug.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; 

;;; Code:
(use-package dap-mode
  :straight (dap-mode
             :type git
             :host github
             :repo "sfavazza/dap-mode"
             :branch "fix_buf_w_breakpoints")
  :defer
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg)
              (call-interactively #'dap-hydra)
              (set-fringe-style '(0 . 0))))
  (defun dap-mode-fringe-hook ()
    "Sets fringes for dap-mode."
    (set-window-fringes nil 20 0))
  (add-hook 'dap-mode-hook 'dap-mode-fringe-hook)
  (dap-auto-configure-mode t)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))

  (dap-register-debug-template
   "LLDB::Run"
   (list :type "lldb-vscode"
         :cwd "."
         :request "launch"
         :name "LLDB::Run"))

  (require 'dap-lldb)
  ;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("/opt/homebrew/opt/llvm/bin/lldb-dap"))
  ;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug."))))

(provide 'feat.debug)
;;; feat.debug.el ends here
