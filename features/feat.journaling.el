;;; feat.journaling.el --- Org mode setup -*- lexical-binding: t; -*-

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

(use-package writeroom-mode
  :straight t
  :defer markdown-mode+
  :config
  (custom-set-default 'writeroom-fullscreen-effect 'maximized)
  (defun writeroom-set-june (arg)
    (setq buffer-face-mode-face '(:family "Atkinson Hyperlegible Mono" :height 180))
    (buffer-face-mode arg))
  (add-to-list 'writeroom-global-effects 'writeroom-set-june)
  (add-hook 'markdown-mode-hook (lambda () (writeroom-mode))))

(provide 'feat.journaling)
;;; feat.journaling.el ends here
