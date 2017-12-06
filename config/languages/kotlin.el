;;; kotlin.el --- Setup markup languages
;;; Commentary:

;;; Code:
(use-package kotlin-mode
  :ensure
  :defer
  :mode "\\.kt\\'")

(use-package flycheck-kotlin
  :ensure
  :defer
  :mode "\\.kt\\'")

(provide 'kotlin.el)
;;; kotlin.el ends here
