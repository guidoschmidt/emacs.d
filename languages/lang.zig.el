;;; lang.zig.el --- Zig language config-*- lexical-binding: t; -*-

;;; Commentary:

;;; 

;;; Code:
(use-package zig-mode
  :straight t
  :defer zig-mode
  :hook ((zig-mode . lsp-deferred))
  :after lsp-mode
  :config
  (if (>= emacs-major-version 28)
      (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
    (progn
      (defun colorize-compilation-buffer ()
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point))))
      (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

  (setq lsp-zig-enable-build-on-save t)
  (setq lsp-zig-build-on-save-step "check")
  (setq lsp-zig-enable-autofix nil)
  (setq lsp-zig-enable-inlay-hints t)

  (defun zig-check ()
    "Compile using `zig build check`."
    (interactive)
    (zig--run-cmd "build" "check"))

  (when (hostname? "Cube")
    (setq lsp-zig-zls-executable "F:/git/zig/zls/zig-out/bin/zls.exe")))

(provide 'lang.zig)
;;; lang.zig.el ends here
