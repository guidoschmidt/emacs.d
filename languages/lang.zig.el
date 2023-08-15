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
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (when (hostname? "Cube")
    (setq lsp-zig-zls-executable "~/.zig/zls.exe"))
  (when (hostname? "Vreni")
    (setq lsp-zig-zls-executable "zls")))

(provide 'lang.zig)
;;; lang.zig.el ends here
