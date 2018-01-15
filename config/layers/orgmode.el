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

;; --- Setup TODO states
(setq org-todo-keywords
      '((sequence "IN" "TODO" "BLOCKED" "WIP" "|" "DONE" "WONTDO")))
(setq org-todo-keyword-faces
      '(("IN"      . (:foreground "white"  :weight bold))
        ("TOOD"    . (:foreground "orange" :weight bold))
        ("BLOCKED" . (:foreground "red"    :weight bold))
        ("WIP"     . (:foreground "orange" :weight bold))
        ("DONE"    . (:foreground "green"  :weight bold))
        ("WONTDO"  . (:foreground "green"  :weight bold))))

(load "~/.emacs.d/github/org-kanban/org-kanban.el")

(provide 'orgmode.el)
;;; orgmode.el ends here
