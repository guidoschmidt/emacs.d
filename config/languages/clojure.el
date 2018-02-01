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
  :config
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-log-messages nil)
  (setq cider-repl-tab-command #'indent-for-tab-command)
  (setq cider-repl-result-prefix ";; -> ")
  (add-hook 'cider-repl-mode-hook
            #'eldoc-mode)
  (add-hook 'cider-mode-hook
            #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook
            #'company-mode)
  (add-hook 'cider-mode-hook
            #'company-mode)
  (add-hook 'cider-repl-mode-hook
            #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook
            #'cider-company-enable-fuzzy-completion)
  (defun cider-repl-prompt-show-two (namespace)
    "Return a prompt string with the last 2 segments of NAMESPACE."
    (let ((names (reverse (subseq (reverse (split-string namespace "\\.")) 0 2))))
      (concat (car names) "." (cadr names) "> ")))
  (add-hook 'cider-mode-hook
            (lambda () (setq next-error-function #'flycheck-next-error-function))))

(use-package clj-refactor
  :ensure t
  :config
  (defun clj-refactor-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'clj-refactor-hook))

(use-package flycheck-clojure
  :ensure
  :commands clojure-mode
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(provide 'clojure.el)
;;; clojure.el ends here
