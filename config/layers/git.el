;;; git.el --- Setup git & magit
;;; Commentary:

;;; Code:
(use-package git-commit
  :ensure t)

(use-package magit
  :ensure t
  :config
  (use-package evil-magit
    :ensure)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package gist
  :ensure t)

(use-package git-gutter
  :ensure
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:update-interval 2)))

(provide 'git.el)
;;; git.el ends here
