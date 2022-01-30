;;; lang.clojure.el --- Clojure language tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Clojure tools and packages

;;; Code:
(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t
  :config
  (setq cider-repl-history-file ".cider-repl-history"
        nrepl-log-messages t))

(use-package flycheck-clojure
  :straight t
  :config
  (flycheck-clojure-setup)
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'lang.clojure)
;;; lang.clojure.el ends here
