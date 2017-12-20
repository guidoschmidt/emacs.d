;;; org.el --- Setup and configure org-mode
;;; Commentary:

;;; Code:
'(org-clock-into-drawer "timetracking")

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("☰" "➟" "●" "✕"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'org.el)
;;; org.el ends here

