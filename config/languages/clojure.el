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
  :ensure t
  :commands (cider-mode cider-jack-in)
  :diminish "CDR"
  :init
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-log-messages nil)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook
            (lambda () (setq next-error-function #'flycheck-next-error-function))))

(use-package flycheck-clojure
  :ensure
  :commands clojure-mode
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
