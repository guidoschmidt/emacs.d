;;; layer.git.el --- Setup git & magit
;;; Commentary:

;;; Code:
(use-package shell
  :ensure t)

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (use-package evil-magit
    :ensure)
  (use-package magit-gh-pulls
    :ensure
    :commands magit-status
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
  (setq magit-diff-paint-whitespace t)
  (defun prevent-whitespace-mode-for-magit ()
    (not (derived-mode-p 'magit-mode)))
  (add-function :before-while whitespace-enable-predicate 'prevent-whitespace-mode-for-magit)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package gist
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:update-interval 2)))

(when (memq window-system '(w32))
  (use-package ssh-agency
    :ensure t
    :config
    (ssh-agency-find-agent)
    (ssh-agency-add-keys '("~/.ssh/id_rsa.github.cube"
                           "~/.ssh/id_rsa.gitlab.cube"))))

(provide 'layer.git)
;;; layer.git ends here