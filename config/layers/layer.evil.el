;; layer.evil --- Setup evil-mode

;;; Commentary:

;;; Code:
(require 'ibuffer)

(setq-default evil-want-C-u-scroll  t)
(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)

(use-package undo-tree
  :disabled
  :straight (undo-tree
             :type git
             :host github
             :repo "emacsmirror/undo-tree"))

(use-package key-chord
  :ensure t
  :after evil
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1))

(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

(defun insert-dash ()
  "Insert dash at current cursor position."
  (interactive)
  (insert-char (string-to-char "—")))

(use-package evil-leader
  :straight (evil-leader
             :type git
             :host github
             :repo "cofi/evil-leader")
  :ensure t
  :after evil
  :init (global-evil-leader-mode)
  :config
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
    "?"       'flyspell-correct-at-point
    "!"       'flyspell-add-word-to-dict
    "/"       'synosaurus-choose-and-replace
    "p"       'counsel-projectile-switch-project
    "f"       'counsel-projectile-find-file
    "t"       'mc/mark-sgml-tag-pair
    "g"       'counsel-ag
    "RET"     'eval-defun
    "c"       'ggtags-create-tags
    "d"       'ggtags-find-tag-dwim
    "<up>"    'beginning-of-defun
    "w"       'save-buffer
    "<down>"  'end-of-defun
    "q"       'objed-occur
    "j"       'swiper-avy
    "-"       'insert-dash
    "o"       'objed-activate
    "0"       'switch-window
    "("       'lispy-parens
    "{"       'lispy-braces
    "["       'lispy-brackets
    "\""      'lispy-quotes
    "*"       'lispy-asterisk
    "/"       'lispy-slash
    "'"       'lispy-singlequote
    "~"       'lispy-tilde
    "<"       'lispy-angle-brackets
    "x"       'frog-jump-buffer
    "y"       'ffip))

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (setq evil-emacs-state-modes
        (delq 'ibuffer-mode evil-emacs-state-modes))
  :bind
  ((:map ibuffer-mode-map
         ("v" . nil)
         ("V" . nil)
         ("r" . ibuffer-do-revert))))

(use-package evil-lispy
  :ensure t
  :after evil
  :config
  (evil-leader/set-key
    "l"  'hydra-lispy-x/body)
  (defalias 'lispy-asterisk
      (lispy-pair "*" "*" 'lispy-parens-preceding-syntax-alist)
      "`lispy-pair' with **.")
  (defalias 'lispy-singlequote
      (lispy-pair "'" "'" 'lispy-parens-preceding-syntax-alist)
      "`lispy-pair' with ''.")
  (defalias 'lispy-slash
      (lispy-pair "/" "/" 'lispy-parens-preceding-syntax-alist)
      "`lispy-pair' with //.")
  (defalias 'lispy-tilde
      (lispy-pair "~" "~" 'lispy-parens-preceding-syntax-alist)
      "`lispy-pair' with ~.")
  (defalias 'lispy-angle-brackets
      (lispy-pair "<" ">" 'lispy-parens-preceding-syntax-alist)
    "`lispy-pair' with <>."))

(use-package lispyville
  :ensure t
  :disabled
  :after evil
  :config
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (setq evil-mc-one-cursor-show-mode-line-text t)
  (global-evil-mc-mode 1)
  :config
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
  (evil-leader/set-key
    "m" 'hydra-evil-mc/body))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package expand-region
  :ensure t
  :disabled
  :config
  (eval-after-load
      "evil"
    '(setq expand-region-contract-fast-key "z"))
  (evil-leader/set-key "e" 'er/expand-region))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-anzu
  :ensure t
  :disabled
  :after evil)

(use-package evil-goggles
  :ensure t
  :disabled
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-org
  :ensure t
  :disabled
  :after evil
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :hook org-mode)

(use-package evil-indent-plus
  :ensure t
  :disabled
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-snipe
  :ensure t
  :after evil
  :disabled
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-commentary
  :ensure t
  :after evil
  :disabled
  :config
  (evil-commentary-mode))

(use-package evil-paredit
  :ensure t
  :after evil)

(use-package evil-cleverparens
  :ensure t
  :after evil
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)
         (clojure-mode . evil-cleverparens-mode)))

(use-package objed
  :straight (objed
             :type git
             :host github
             :repo "clemera/objed")
  :disabled
  :config
  (setq objed-cursor-color "#25D3E2")
  (setq objed-modeline-hint-p nil)
  (objed-mode))

(use-package evil-magit
  :ensure t
  :after evil
  :config
  (setq evil-magit-use-y-for-yank t))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(provide 'layer.evil)
;;; layer.evil ends here
