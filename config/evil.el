;;; evil.el --- Setup evil-mode like a pro
;;; Commentary:

;;; Code:
;;; --- Basic evil mode setup
(use-package evil
  :ensure
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode t)
  (setq evil-emacs-state-modes
        (delq 'ibuffer-mode evil-emacs-state-modes))
  (define-key ibuffer-mode-map (kbd "v") nil)
  (define-key ibuffer-mode-map (kbd "V") nil)
  (define-key ibuffer-mode-map (kbd "r") 'ibuffer-do-revert))

(use-package evil-leader
  :ensure
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
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
    "RET"     'eval-defun))

(use-package evil-cleverparens
  :ensure
  :config
  (add-hook 'emacs-lisp-mode #'evil-cleverparens-mode)
  (add-hook 'clojure-mode #'evil-cleverparens-mode))

;;; --- Paredit for evil mode
(use-package evil-paredit
  :ensure
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
  (add-hook 'clojure-mode 'evil-paredit-mode))

(use-package evil-mc
  :ensure
  :config
  (use-package multiple-cursors
    :ensure)
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (global-evil-mc-mode 1)
  :bind
  (("C-<" . evil-mc-make-cursor-move-next-line)
   ("C->" . evil-mc-make-cursor-move-prev-line)))

(use-package expand-region
  :ensure
  :config
  (eval-after-load
      "evil"
    '(setq expand-region-contract-fast-key "z"))
  (evil-leader/set-key "e" 'er/expand-region))

(use-package evil-surround
  :ensure
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
  :commands evil-mode
  :ensure)

(provide 'evil.el)
;;; evil.el ends here
