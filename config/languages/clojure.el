;;; clojure.el --- Setup clojure language
;;; Commentary:

;;; Code:
(use-package clojure-mode
  :ensure
  :mode "\\.clj\\'"
  :defer
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package clojure-snippets
  :ensure
  :defer
  :commands clojure-mode)

(use-package cider
  :ensure
  :defer
  :commands clojure-mode)

(use-package flycheck-clojure
  :ensure
  :defer
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
