;;; clojure.el --- Setup clojure language
;;; Commentary:

;;; Code:
(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package clojure-snippets
  :ensure t
  :commands clojure-mode)

(use-package cider
  :ensure t
  :commands clojure-mode)

(use-package flycheck-clojure
  :ensure
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
