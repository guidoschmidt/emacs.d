;;; lang.nix.el --- Nix language setup -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package nix-mode
  :straight t
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(provide 'lang.nix)
;;; lang.nix.el ends here
