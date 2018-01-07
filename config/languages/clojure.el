;;; clojure.el --- Setup clojure language
;;; Commentary:

;;; Code:
(use-package clojure-mode
  :commands clojure-mode
  :mode "\\.clj\\'"
  :defer
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package clojure-snippets
  :commands clojure-mode)

(use-package cider
  :commands clojure-mode)

(use-package flycheck-clojure
  :commands clojure-mode
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
