;;; feat.snippets.el --- Snippet support -*- lexical-binding: t; -*-

;;; Commentary:
;;; Implement snippet system using yasnippet

;;; Code:
(use-package yasnippet
  :straight t
  :config
  (setq yas-trigger-in-field t)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'feat.snippets)
;;; feat.snippets.el ends here
