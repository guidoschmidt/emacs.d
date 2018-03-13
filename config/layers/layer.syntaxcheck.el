;;; layer.syntaxcheck --- Syntax checking via flycheck

;;; Commentary:

;;; Code:
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq-default flycheck-temp-prefix ".flycheck"))

(use-package flycheck-package
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-package-setup))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  (eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; Custom fringe indicator
(define-fringe-bitmap 'my-flycheck-fringe-indicator
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00011100
          #b00111110
          #b00111110
          #b00111110
          #b00011100
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000))

(let ((bitmap 'my-flycheck-fringe-indicator))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap bitmap
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap bitmap
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap bitmap
    :fringe-face 'flycheck-fringe-info))

(global-flycheck-mode 1)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

(provide 'layer.syntaxcheck)
;;; layer.syntaxcheck ends here
