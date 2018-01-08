;;; org.el --- Setup and configure org-mode

;;; Commentary:

;;; Code:
'(org-clock-into-drawer "timetracking")

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("☰" "➟" "●" "✕"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; enable syntax highlighting by default
(setq org-src-fontify-natively t)

(load "~/.emacs.d/github/org-kanban/org-kanban.el")

(provide 'orgmode.el)
;;; orgmode.el ends here
