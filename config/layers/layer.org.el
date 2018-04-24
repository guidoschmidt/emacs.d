;;; layer.org --- Setup and configure org-mode

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

(defhydra hydra-org (:color blue :hint nil)
  "
Org Mode

_j_: org-cycle-level ->
_k_: org-cycle <-
"
  ("j" org-cycle-level)
  ("k" org-cycle))

(evil-leader/set-key
  "z" 'hydra-org/body)

;; Kanban board
;;(load "~/.emacs.d/github/org-kanban/org-kanban.el")

;; Confluence
(use-package ox-confluence
  :load-path "~/.emacs.d/github/org-confluence"
  :commands org-confluence-export-as-confluence)

;; Show inline images
(setq org-image-actual-width nil)
(setq org-image-actual-width '(400))
(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)

(provide 'layer.org)
;;; layer.org ends here
