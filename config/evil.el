;;; evil.el --- Setup evil-mode like a pro
;;; Commentary:

;;; Code:
;;; --- Basic evil mode setup
(use-package evil
  :ensure
  :config
  (evil-mode t)
  (setq evil-emacs-state-modes
        (delq 'ibuffer-mode evil-emacs-state-modes)))

;;; --- Evil leader for shortcuts
(use-package evil-leader
  :ensure
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "a" 'align-regexp
    "b" 'ivy-switch-buffer
    "i" 'ibuffer
    "n" 'ivy-switch-buffer-other-window
    "k" 'ido-kill-buffer
    "s" 'magit-status
    "TAB" 'indent-region
    "?" 'ispell-word
    "p" 'counsel-projectile-switch-project
    "f" 'counsel-projectile-find-file
    "t" 'mc/mark-sgml-tag-pair))

;;; --- Evil cleverparens
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

;;; --- Multiple cursors + evil multiple cursors
(use-package multiple-cursors
  :ensure)

(use-package evil-mc
  :ensure
  :config
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (global-evil-mc-mode 1)
  :bind
  (("C-<" . evil-mc-make-cursor-move-next-line)
   ("C->" . evil-mc-make-cursor-move-prev-line)))

;;; Expand region in combination with evil-leader
(use-package expand-region
  :ensure
  :config
  (eval-after-load
      "evil"
    '(setq expand-region-contract-fast-key "z"))
  (evil-leader/set-key "e" 'er/expand-region))

;;; Evil-surround
(use-package evil-surround
  :ensure
  :config
  (global-evil-surround-mode 1))

(provide 'evil.el)
;;; evil.el ends here
