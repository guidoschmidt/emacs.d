;;; clojure.el --- Setup clojure language
;;; Commentary:

;;; Code:
(use-package clojure-mode
  :ensure
  :commands clojure-mode
  :mode "\\.clj\\'"
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package clojure-snippets
  :ensure
  :commands clojure-mode)

(use-package cider
  :ensure
  :commands clojure-mode
  :config
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package flycheck-clojure
  :ensure
  :commands clojure-mode
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
