;;; tabs --- beautiful Emacs tabs
;;; Commentary:

;;; Code:
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-mode 'under)
  (centaur-tabs-mode t)
  (defhydra hydra-tabs (:color red :hint nil)
    "
Tabs

_>_: Next Tab
_<_: Previous Tab"
    (">" centaur-tabs-forward)
    ("<" centaur-tabs-backward))
  (evil-leader/set-key
    "t" 'hydra-tabs/body)
  :bind
  ("C-<" . centaur-tabs-backward)
  ("C->" . centaur-tabs-forward))

(provide 'tabs)
;;; tabs ends here