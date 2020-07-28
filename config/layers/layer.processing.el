;;; layer.processing --- Openframeworks convenience functions
;;; Commentary:

;;; Code:
(use-package processing-mode
  :ensure t
  :mode ("\\.pde\\'" . processing-mode)
  :config
  (when (and (equalp (system-name) "Vreni")
             (memq window-system '(ns)))
      (setq processing-location "/usr/local/bin/processing-java")
      (setq processing-application-dir "/Applications/Processing.app")
      (setq processing-sketchbook-dir "~/Documents/Processing"))
  (when (and (equalp (system-name) "Cube")
             (memq window-system '(w32)))
      (setq processing-location "c:/Development/processing/3.5.4/processing-java.exe")
      (setq processing-application-dir "c:/Development/processing/3.5.4")
      (setq processing-sketchbook-dir "f:/git/processing")))
      

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
