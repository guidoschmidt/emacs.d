;;; clojure.el --- Setup clojure language
;;; Commentary:

;;; Code:
(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package clojure-snippets
  :ensure t)

(use-package cider
  :ensure t)

(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
