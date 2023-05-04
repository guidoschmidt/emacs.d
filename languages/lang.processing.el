;;; lang.processing.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; 

;;; Code:
(use-package processing-3-mode
  :straight (processing-3-mode :type git
                               :host github
                               :repo "motform/processing-3-mode")
  :mode  "\\.pde\\'")

(provide 'lang.processing)
;;; lang.processing.el ends here
