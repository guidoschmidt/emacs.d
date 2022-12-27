;;; feat.editor.el --- Editor improvements -*- lexical-binding: t; -*-

;;; Commentary:
;;; Replacements and improvements for Emacs behaviour

;;; Code:
;;; Clean emacs configuration with no littering
(use-package no-littering
  :straight t
  :init
  (require 'recentf)
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; Diminish
(use-package diminish
  :straight t)

;;; counsel
(use-package counsel
  :straight t
  :bind
  (("M-x" . counsel-M-x)
   ("C-c g" . counsel-ag)
   ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :straight t
  :after projectile)

;;; ivy - generic completion frontend
(use-package ivy
  :straight t
  :diminish ivy-mode
  :config
  (setq enable-recursive-minibuffers nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-height 35)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-recenter ()
    "Advice swiper to recenter on exit."
    (recenter))
  (advice-add 'swiper :after #'swiper-recenter)
  (ivy-mode t))

(use-package wgrep-ag
  :straight t)

(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode t))

(use-package lsp-ivy
  :straight t)

;;; swiper - isearch replacement
(use-package swiper
  :straight t
  :bind
  (("C-s" . swiper)))

;;; avy - jump to characters and expressions fast
(use-package avy
  :straight t
  :custom-face
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  (custom-set-faces
   '(avy-lead-face
     ((t (:inherit avy-lead-face
                   :background "#eeeeee"
                   :foreground "#424242"))))
   '(avy-lead-face-0
     ((t (:inherit avy-lead-face-0
                   :background "#feca32"
                   :foreground "#424242")))))
  :bind
  (("C-c a" . avy-goto-char)
   ("C-c o" . avy-goto-char-timer)
   ("C-c e" . swiper-avy)))

;;; Project management
(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (projectile-mode t))

;;; Smart M-x enhancement, e.g. sorting items in M-x minibuffer by usage
(use-package smex
  :straight t)

;;; Switch window
(use-package switch-window
  :straight t)

;;; Execute path from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;;; Undo visualization
(use-package undo-tree
  :disabled
  :straight t
  :diminish undo-tree-mode)

;;; Smart hungry delete
(use-package smart-hungry-delete
  :straight t
  :bind
  (("C-<backspace>" . smart-hungry-delete-backward-char)
   ("C-d" . smart-hungry-delete-forward-char))
  :config
  (smart-hungry-delete-add-default-hooks))

;; Highlight parantheses
(use-package highlight-parentheses
  :straight t
  :hook ((prog-mode . highlight-parentheses-mode)
         (prog-mode . show-paren-mode)))

;; Treemacs
(use-package treemacs
  :straight t
  :config
  (setq treemacs-position 'left)
  (setq treemacs-width 42)
  (setq treemacs-indentation 2)
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-resize-icons 13)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  :hook
  (treemacs-mode . (lambda ()
                     (display-line-numbers-mode -1)
                     (custom-set-faces
                      '(treemacs-root-face
                        ((t (:inherit treemacs-root-face
                             :family "Iosevka Term SS08 Thin"
                             :height 115))))))))

(use-package treemacs-evil
  :after treemacs evil
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :straight t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

;; dashboard
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.emacs.d/logo.png")
  (setq dashboard-items '((recents  . 10)
                          (projects . 3)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "Have a good coding session"))

;; view large files
(use-package vlf
  :straight t
  :config
  (require 'vlf-setup))

(provide 'feat.editor)
;;; feat.editor.el ends here
