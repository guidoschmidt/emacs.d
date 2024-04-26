;;; lang.zig.el --- Zig language config-*- lexical-binding: t; -*-

;;; Commentary:

;;; 

;;; Code:
(use-package zig-mode
  :straight t
  :defer zig-mode
  :hook ((zig-mode . lsp-deferred))
  :custom (zig-format-on-save nil)
  :config
  (setq zig-indent-offset 4)
  (if (>= emacs-major-version 28)
      (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
    (progn
      (defun colorize-compilation-buffer ()
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point))))
      (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (when (hostname? "Cube")
    (setq lsp-zig-zls-executable "F:/git/zig/zls/zig-out/bin/zls.exe"))
  (when (hostname? "Vreni")
    (setq lsp-zig-zls-executable "zls"))
  (when (hostname? "Konrad")
    (setq lsp-zig-zls-executable "~/git/zig/zls/zig-out/bin/zls")))

(provide 'lang.zig)
;;; lang.zig.el ends here
