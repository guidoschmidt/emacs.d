;;; tabs --- beautiful Emacs tabs
;;; Commentary:

;;; Code:
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-height 42)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-mode 'under)
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(provide 'tabs)
;;; tabs ends here
