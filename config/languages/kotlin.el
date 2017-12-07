;;; kotlin.el --- Setup markup languages
;;; Commentary:

;;; Code:
(use-package kotlin-mode
  :ensure
  :defer
  :mode "\\.kt\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode)))

(use-package flycheck-kotlin
  :ensure
  :defer
  :mode "\\.kt\\'"
  :after (flycheck)
  :config
  (eval-after-load 'flycheck
    (flycheck-kotlin-setup)))

(provide 'kotlin.el)
;;; kotlin.el ends here
