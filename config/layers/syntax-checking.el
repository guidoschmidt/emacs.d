;;; packages.el --- Syntax checking via flycheck
;;; Commentary:

;;; Code:
;; --- Flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (setq-default flycheck-temp-prefix ".flycheck")
    (setq-default flycheck-disabled-checkers '(javascript-jshint))
    (setq-default flycheck-disabled-checkers '(json-jsonlint))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'vue-mode)))

(use-package flycheck-package
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-package-setup))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(global-flycheck-mode 1)

(provide 'syntax-checking.el)
;;; syntax-checking.el ends here
