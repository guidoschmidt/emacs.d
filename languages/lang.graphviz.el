;;; lang.graphviz.el --- Graphviz support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Use graphviz dot files in Emacs

;;; Code:
(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode
             :type git
             :host github
             :repo "ppareit/graphviz-dot-mode")
  :config
  (setq graphviz-dot-intend-with 4)
  (use-package company-graphviz-dot
    :disabled))

(provide 'lang.graphviz)
;;; lang.graphviz.el ends here
