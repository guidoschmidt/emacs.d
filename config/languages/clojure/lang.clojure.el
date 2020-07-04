;;; lang.clojure --- Setup clojure language

;;; Commentary:

;;; Code:
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
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  :hook ((cider-repl-mode . eldoc-mode)
         (cider-mode      . eldoc-mode)
         (cider-repl-mode . company-mode)
         (cider-mode      . company-mode)))

(use-package clj-refactor
  :ensure t
  :disabled
  :config
  (defun clj-refactor-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c f"))
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :config
  (require 'flycheck-clj-kondo)
  :hook (clojure-mode-hook . lispy-mode))

(use-package clojurescript-mode
  :mode "\\.cljs\\'"
  :config
  (require 'flycheck-clj-kondo)
  :hook (clojurescript-mode-hook . lispy-mode))

(provide 'lang.clojure)
;;; lang.clojure ends here
