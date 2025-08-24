;;; feat.journaling.el --- Org mode setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Org mode configuration and setup

;;; Code:

;;; Hide the emphasis markup
;;; (e.g. /.../ for italics, *...* for bold, etc.)
(setq-default org-hide-emphasis-markers t)

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package writeroom-mode
  :straight t
  :defer markdown-mode
  :config
  (custom-set-default 'writeroom-fullscreen-effect 'maximized)
  (defun writeroom-set-font ()
    "Sets a fixed width (monospace) font in current buffer"
    (setq buffer-face-mode-face '(:family "Atkinson Hyperlegible Mono" :height 250))
    (buffer-face-mode)
    (auto-fill-mode 0))
  (add-to-list 'writeroom-mode-hook 'writeroom-set-font)
  (add-hook 'markdown-mode-hook (lambda () (writeroom-mode))))

(provide 'feat.journaling)
;;; feat.journaling.el ends here
