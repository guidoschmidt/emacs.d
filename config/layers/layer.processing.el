;;; layer.processing --- Openframeworks convenience functions
;;; Commentary:

;;; Code:
(use-package processing-mode
  :ensure t
  :mode ("\\.pde\\'" . processing-mode)
  :config
  (setq processing-location "/usr/local/bin/processing-java")
  (setq processing-application-dir "/Applications/Processing.app")
  (setq processing-sketchbook-dir "~/Documents/Processing"))

(use-package processing-snippets
  :ensure t)

(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'processing-mode-hook 'lsp))

(provide 'layer.processing)
;;; layer.processing ends here
