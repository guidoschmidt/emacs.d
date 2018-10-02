;;; lang.clojure --- Setup clojure language

;;; Commentary:

;;; Code:
(use-package clojure-mode
  :ensure t
  :commands clojure-mode
  :mode "\\.clj\\'"
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package clojure-snippets
  :ensure t
  :defer)

(use-package cider
  :ensure t
  :commands (cider-mode cider-jack-in)
  :diminish "Cider"
  :config
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-log-messages nil)
  (setq cider-repl-tab-command #'indent-for-tab-command)
  (setq cider-repl-result-prefix ";; -> ")
  (defun cider-repl-prompt-show-two (namespace)
    "Return a prompt string with the last 2 segments of NAMESPACE."
    (let ((names (reverse (subseq (reverse (split-string namespace "\\.")) 0 2))))
      (concat (car names) "." (cadr names) "> ")))
  :hook ((cider-repl-mode . eldoc-mode)
         (cider-mode      . eldoc-mode)
         (cider-repl-mode . company-mode)
         (cider-mode      . company-mode)
         (cider-repl-mode . (lambda ()
                              (eval-when-compile
                                (cider-company-enable-fuzzy-completion))))
         (cider-mode      . (lambda ()
                              (eval-when-compile
                                (cider-company-enable-fuzzy-completion))))
         (cider-mode      . (lambda ()
                              (setq next-error-function
                                    #'flycheck-next-error-function)))))

(use-package clj-refactor
  :ensure t
  :config
  (defun clj-refactor-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c f"))
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(lsp-define-stdio-client lsp-clojure
                         "clojure"
                         #'projectile-project-root
                         '("~/Downloads/clojure-lsp"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (lsp-clojure-enable)))

(provide 'lang.clojure)
;;; lang.clojure ends here
