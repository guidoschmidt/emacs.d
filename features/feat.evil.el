;;; feat.evil.el --- Evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; VIM emulation for Emacs = Evil

;;; Code:
(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)

(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "a"        'align-regexp
    "o"        'sort-lines
    "b"        'ivy-switch-buffer
    "c"        'clang-format-region
    "x"        'frog-jump-buffer
    "i"        'ibuffer
    "u"        'hydra-lsp-ui/body
    "n"        'ivy-switch-buffer-other-window
    "k"        'ido-kill-buffer
    "s"        'magit-status
    "p"        'counsel-projectile-switch-project
    "f"        'counsel-projectile-find-file
    "g"        'counsel-ag
    "w"        'save-buffer
    "j"        'swiper-avy
    "y"        'yas-insert-snippet
    "q"        'kill-emacs
    "r"        'reload-current-buffer
    "?"        'flyspell-correct-at-point
    "!"        'flyspell-add-word-to-dict
    "m"        'hydra-evil-mc/body
    "0"        'switch-window
    "<C-up>"   'move-line-up
    "<C-down>" 'move-line-down

    "TAB"      'indent-region
    "RET"      'eval-buffer

    "<up>"     'beginning-of-defun
    "<down>"   'end-of-defun

    "("        'lispy-parens
    "{"        'lispy-braces
    "["        'lispy-brackets
    "\""       'lispy-quotes
    "*"        'lispy-asterisk
    "/"        'lispy-slash
    "          '" 'lispy-singlequote
    "~"        'lispy-tilde
    "<"        'lispy-angle-brackets
    "`"        'lispy-backticks))

(use-package evil
  :straight t
  :after evil-leader
  :config
  (evil-set-undo-system 'undo-fu)
  (evil-mode t))

(use-package undo-fu
  :straight t)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq-default evil-collection-setup-minibuffer t)
  (setq-default evil-collection-company-use-tng nil)
  (evil-collection-init))

;; provides gl and gL align operators
(use-package evil-lion
  :straight t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-lispy
  :straight t
  :after evil
  :config
  (defalias 'lispy-asterisk
    (lispy-pair "*" "*" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' using *")
  (defalias 'lispy-singlequote
    (lispy-pair "'" "'" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' using '")
  (defalias 'lispy-slash
    (lispy-pair "/" "/" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' using /")
  (defalias 'lispy-tilde
    (lispy-pair "~" "~" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' using ~.")
  (defalias 'lispy-angle-brackets
    (lispy-pair "<" ">" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' using <>")
  (defalias 'lispy-backticks
    (lispy-pair "`" "`" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' using `"))

(use-package evil-mc
  :straight t
  :after evil
  :config
  (setq evil-mc-one-cursor-show-mode-line-text t)
  (defhydra hydra-evil-mc (:color black)
    "
Cursors
_j_: next                 _i_: iedit-mode
_k_: previous
_n_: skip + next
_p_: skip + previous
_m_: make cursor
"
    ("j" evil-mc-make-cursor-move-next-line "next")
    ("k" evil-mc-make-cursor-move-prev-line "previous")
    ("n" evil-mc-skip-and-goto-next-match "skip + next")
    ("p" evil-mc-skip-and-goto-prev-match "skip + previous")
    ("m" evil-mc-make-cursor-here "cursor")
    ("i" iedit-mode))
  (global-evil-mc-mode t))

(provide 'feat.evil)
;;; feat.evil.el ends here
