;;; layer.git.el --- Setup git & magit
;;; Commentary:

;;; Code:
(require 'cl)

(use-package magit-gh-pulls
  :ensure t
  :disabled
  :hook (magit-mode . #'tun-on-magit-gh-pulls))

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (setq magit-diff-paint-whitespace t)
  (setq magit-completing-read-function 'ivy-completing-read)

  (when (and (memq window-system '(w32))
             (equalp (system-name) "Cube"))
    (use-package ssh-agency
      :ensure t
      :config
      (setenv "SSH_ASKPASS" "git-gui--askpass")
      (ssh-agency-find-agent)
      (setq ssh-agency-keys '("~/.ssh/id_rsa.github.cube"
                              "~/.ssh/id_rsa.gitlab.cube"))))

  (when (and (memq window-system '(w32))
             (equalp (system-name) "Zenbook-GS"))
   (use-package ssh-agency
     :ensure t
     :config
     (setenv "SSH_ASKPASS" "git-gui--askpass")
     (ssh-agency-find-agent)
     (setq ssh-agency-keys '("//wsl$/Ubuntu/home/guido/.ssh/id_ed25519.github@fmp.studio")))))

(provide 'layer.git)
;;; layer.git ends here
