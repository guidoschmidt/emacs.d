;;; lang.zip.el --- Zig language config-*- lexical-binding: t; -*-

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
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "~/zls/zls")
    :major-modes '(zig-mode)
    :server-id 'zls)))

(provide 'lang.zig)
;;; lang.zip.el ends here
