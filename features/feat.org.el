;;; feat.org.el --- Org mode setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Org mode configuration and setup

;;; Code:

;;; Hide the emphasis markup
;;; (e.g. /.../ for italics, *...* for bold, etc.)
(setq org-hide-emphasis-markers t)

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'feat.org)
;;; feat.org.el ends here
