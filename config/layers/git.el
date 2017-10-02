;;; git.el --- Setup git & magit
;;; Commentary:

;;; Code:
(use-package git-commit
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package gist
  :ensure t)

(provide 'git.el)
;;; git.el ends here
