;;; lang.cc.el --- C/C++ language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; C/C++ configuration and tooling

;;; Code:
(use-package clang-format
  :straight t)

(use-package modern-cpp-font-lock
  :straight t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package google-c-style
  :straight t
  :hook
  (c-mode-common-hook . google-set-c-style)
  (c++-mode-hook      . google-set-c-style)
  (c-mode-hook        . google-set-c-style))

;; CCLS for lsp - lsp implementation for C++
(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp)))
  :init
  (defvar ccls-sem-highlight-method 'font-lock)
  (setq lsp-prefer-flymake nil)
  :config
  (defvar projectile-project-root-files-top-down-recurring)
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring))))

(use-package dap-mode
  :straight t
  :config
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra))))

(provide 'lang.cc)
;;; lang.cc.el ends here
