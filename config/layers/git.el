;;; git.el --- Setup git & magit
;;; Commentary:

;;; Code:
(use-package shell
  :ensure)

(use-package magit
  :ensure
  :commands magit-status
  :config
  (use-package evil-magit
    :ensure)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package gist
  :ensure)

(use-package git-gutter
  :ensure
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:update-interval 2)))

(when (memq window-system '(w32))
  (use-package ssh-agency
    :ensure
    :config
    (ssh-agency-find-agent)
    (ssh-agency-add-keys '("~/.ssh/id_rsa.github.cube"))))

(provide 'git.el)
;;; git.el ends here
