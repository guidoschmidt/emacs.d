;;; org.el --- Setup and configure org-mode

;;; Commentary:

;;; Code:
'(org-clock-into-drawer "timetracking")

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("☰" "➟" "●" "✕"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; -- Enable syntax highlighting by default
(setq org-src-fontify-natively t)

;; Kanban board
;;(load "~/.emacs.d/github/org-kanban/org-kanban.el")

;; Confluence
(use-package ox-confluence
  :load-path "~/.emacs.d/github/org-confluence"
  :commands org-confluence-export-as-confluence)

(provide 'orgmode.el)
;;; orgmode.el ends here
