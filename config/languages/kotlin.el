;;; kotlin.el --- Setup markup languages
;;; Commentary:

;;; Code:
(use-package kotlin-mode
  :ensure
  :mode "\\.kt\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode)))

;; (use-package flycheck-kotlin
;;   :ensure
;;   :defer
;;   :commands (kotlin-mode)
;;   :mode "\\.kt\\'"
;;   :after (flycheck kotlin-mode)
;;   :config
;;   (eval-after-load 'flycheck
;;     (flycheck-kotlin-setup)))

(provide 'kotlin.el)
;;; kotlin.el ends here
