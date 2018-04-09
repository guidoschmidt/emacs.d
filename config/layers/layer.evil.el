;;; layer.evil --- Setup evil-mode

;;; Commentary:

;;; Code:
(require 'ibuffer)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode t)
  (setq evil-emacs-state-modes
        (delq 'ibuffer-mode evil-emacs-state-modes))
  :bind
  ((:map ibuffer-mode-map
         ("v" . nil)
         ("V" . nil)
         ("r" . ibuffer-do-revert))))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (eval-when-compile (evil-leader/set-leader "<SPC>"))
  (evil-leader/set-key
    "a"       'align-regexp
    "o"       'sort-lines
    "b"       'ivy-switch-buffer
    "i"       'ibuffer
    "n"       'ivy-switch-buffer-other-window
    "k"       'ido-kill-buffer
    "s"       'magit-status
    "TAB"     'indent-region
    "?"       'ispell-word
    "p"       'counsel-projectile-switch-project
    "f"       'counsel-projectile-find-file
    "t"       'mc/mark-sgml-tag-pair
    "g"       'counsel-ag
    "RET"     'eval-defun
    "v"       'undo-tree-visualize
    "c"       'ggtags-create-tags
    "d"       'ggtags-find-tag-dwim
    "<up>"    'beginning-of-defun
    "w"       'save-buffer
    "<down>"  'end-of-defun
    "j"       'counsel-imenu))

(use-package lispy
  :ensure t
  :hook ((clojure-mode       . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (common-lisp-mode   . lispy-mode)
         (emacs-lisp-mode    . lispy-mode))
  :config
  (evil-leader/set-key
    "l"  'hydra-lispy-x/body))

(use-package lispyville
  :ensure t
  :config
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package evil-mc
  :ensure t
  :config
  (setq evil-mc-one-cursor-show-mode-line-text t)
  (global-evil-mc-mode 1)
  :config
  (defhydra hydra-evil-mc (:color black)
    "
Cursors

_j_: next
_k_: previous
_n_: skip + next
_p_: skip + previous
_m_: make cursor
_a_: make all cursors
"
    ("j" evil-mc-make-cursor-move-next-line "next")
    ("k" evil-mc-make-cursor-move-prev-line "previous")
    ("n" evil-mc-skip-and-goto-next-match "skip + next")
    ("p" evil-mc-skip-and-goto-prev-match "skip + previous")
    ("m" evil-mc-make-cursor-here "cursor"))
  (evil-leader/set-key
    "m" 'hydra-evil-mc/body))

(use-package evil-multiedit
  :ensure t
  :config
  (evil-multiedit-default-keybinds))

(use-package expand-region
  :ensure t
  :config
  (eval-after-load
      "evil"
    '(setq expand-region-contract-fast-key "z"))
  (evil-leader/set-key "e" 'er/expand-region))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-anzu
  :ensure t)

(provide 'layer.evil)
;;; layer.evil ends here
