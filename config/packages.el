;;; packages.el --- Manage MELPA packages
;;; Commentary:

;;; Code:
;;; --- Try packages without installing them
;;; Keep .emacs.d clean
(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package try
  :ensure t)

;;; --- Setup which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; --- EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; --- Yasnippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;;; --- Powerline & Spaceline
(use-package powerline
  :ensure t)
(use-package spaceline
  :ensure t
  :after powerline
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'utf-8)
    (setq powerline-height 20)
    (spaceline-spacemacs-theme)))

;;; --- Neo-tree with icons
(use-package all-the-icons
  :ensure t)
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;; --- Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; --- Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

;;; --- Dump-Jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)
  :ensure t)

;;; --- Undo-tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;;; --- Tern
(use-package tern
  :ensure t
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

;;; --- Setup org-bullets
'(org-clock-into-drawer "timetracking")

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; --- Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;;; --- Setup ace-window
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-forground :height 1.5)))))))

;;; --- Emmet
(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook 'sass-mode-hook  'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'stylus-mode-hook 'emmet-mode)
    (add-hook 'jsx-mode-hook 'emmet-mode)
    (add-hook 'markdown-mode-hook 'emmet-mode)))

;;; --- Rainbow mode
(use-package rainbow-mode
  :ensure t)

;;; --- Git
(use-package git-commit
  :ensure t)
(use-package magit
  :ensure)

;;; --- Ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

;;; --- Swiper - better isearch
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-ag))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;;; --- Avy
(use-package avy
  :ensure t
  :bind (("C-c a" . avy-goto-char)
         ("C-c o" . avy-goto-char-timer)))

;;; --- Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)))

;;; --- RESTclient
(use-package restclient
  :ensure t)

;;; --- Fill collumn indicator
(use-package fill-column-indicator
  :ensure t
  :defer t
  :config
  (setq fci-rule-width 2)
  (setq-default fci-rule-column 80)
  (setq fci-rule-color "lightgray")
  (setq whitespace-style '(face trailing))
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;;; --- FIC Mode
(use-package fic-mode
  :ensure t
  :config
  (require 'fic-mode)
  (add-hook 'c++-mode-hook 'fic-mode)
  (add-hook 'sass-mode-hook 'fic-mode)
  (add-hook 'python-mode-hook 'fic-mode)
  (add-hook 'jsx-mode-hook 'fic-mode))

;;; --- Wrap region
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-add-wrapper "`" "`")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper """ """)
  (wrap-region-add-wrapper "'" "'")
  (wrap-region-mode t))

;;; --- Language specific
;;; --- Haskell
(use-package haskell-mode
  :ensure t
  :defer t)

;;; --- Android
(use-package gradle-mode
  :ensure t)

;;; --- Clojure
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

;;; --- Markdown
(use-package markdown-mode+
  :ensure t)

;;; --- Yaml
(use-package yaml-mode
  :ensure t)

;;; --- GLSL
(use-package glsl-mode
  :ensure t
  :config
  (add-hook 'glsl-mode-hook
            (lambda()
              (defvar c-basic-offset 1)
              (setq tab-width 2))))

;;; --- Twig templates
(use-package twig-mode
  :ensure t
  :defer t)

;;; --- Vue.js
(use-package vue-mode
  :ensure t
  :defer t
  :config
  (use-package vue-html-mode :ensure t :defer t)
  (setq mmm-submode-decoration-level 0))

;;; --- Python
(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "ipython")
  (defun custom-python-mode-hook ()
    (setq python-indent-offset 4)
    (setq tab-width 4)
    (setq indent-tabs-mode nil))
  (add-hook 'python-mode-hook 'custom-python-mode-hook)
  (elpy-enable))

;;; --- Load additional layers
(load "~/.emacs.d/config/layers/auto-completion.el")
(load "~/.emacs.d/config/layers/c-modes.el")
(load "~/.emacs.d/config/layers/syntax-checking.el")
(load "~/.emacs.d/config/layers/spell-checking.el")

(provide 'packages.el)
;;; packages.el ends here
