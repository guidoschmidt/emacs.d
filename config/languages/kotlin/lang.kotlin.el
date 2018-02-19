;;; lang.kotlin --- Setup markup languages
;;; Commentary:

;;; Code:
(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(use-package flycheck-kotlin
  :ensure t
  :mode "\\.kt\\'"
  :after (flycheck kotlin-mode)
  :config
  (eval-after-load 'flycheck
    (flycheck-kotlin-setup)))

(provide 'lang.kotlin)
;;; lang.kotlin ends here
