;;; packages.el --- Manage MELPA packages
;;; Commentary:

;;; Code:
;;; --- Try packages without installing them
;;; Keep .emacs.d clean
(use-package no-littering
  :ensure t
  :defer t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package try
  :ensure t
  :defer t)

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

;;; --- EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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

;;; --- Auto highlight words
(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t))

;;; --- Setup org-bullets
'(org-clock-into-drawer "timetracking")

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
    (add-hook  'web-mode-hook      'emmet-mode)
    (add-hook  'mustache-mode-hook 'emmet-mode)
    (add-hook  'html-mode-hook     'emmet-mode)
    (add-hook  'markdown-mode-hook 'emmet-mode)))

;;; --- Rainbow mode
(use-package rainbow-mode
  :ensure t
  :defer t)

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
  :ensure t
  :defer t)

(use-package swiper
  :ensure t
  :defer t
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
  :defer t
  :bind (("C-c a" . avy-goto-char)
         ("C-c o" . avy-goto-char-timer)))

;;; --- Multiple cursors
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)))

;;; --- Fill collumn indicator
(use-package fill-column-indicator
  :ensure t
  :defer t
  :config
  (setq fci-rule-width 2)
  (setq-default fci-rule-column 80)
  (setq-default fci-rule-color "lightgray")
  (setq-default whitespace-style '(face trailing))
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;;; --- FIC Mode
(use-package fic-mode
  :ensure t
  :defer t
  :config
  (require 'fic-mode)
  (add-hook 'c++-mode-hook 'fic-mode)
  (add-hook 'sass-mode-hook 'fic-mode)
  (add-hook 'python-mode-hook 'fic-mode)
  (add-hook 'rjsx-mode-hook 'fic-mode)
  (add-hook 'js-mode-hook 'fic-mode)
  (add-hook 'js2-mode-hook 'fic-mode)
  (add-hook 'jsx-mode-hook 'fic-mode))

;;; --- Wrap region
(use-package wrap-region
  :ensure t
  :defer t
  :config
  (wrap-region-add-wrapper "`" "`")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper """ """)
  (wrap-region-add-wrapper "'" "'")
  (wrap-region-mode t))

;;; -- Refactor
(use-package emr
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  :bind
  (("<M-RET>" . emr-show-refactor-menu)))

;;; --- Language specific
;;; --- HTML
(use-package web-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.mustache\\'" . web-mode))
  (defun custom-web-mode-hook ()
    (setq-default web-mode-markup-indent-offset 2)
    (setq-default web-mode-css-indent-offset 2)
    (setq-default web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'custom-web-mode-hook))

(use-package html-check-frag
  :ensure t
  :defer t
  :config
  (add-hook 'html-mode-hook (lambda () (html-check-frag-mode 1))))

;;; --- Android
(use-package gradle-mode
  :ensure t
  :defer t)

;;; --- Markdown
(use-package markdown-mode+
  :ensure t
  :defer t)

;;; --- Yaml
(use-package yaml-mode
  :ensure t
  :defer t)

;;; --- Load additional layers
(load "~/.emacs.d/config/layers/autocomplete.el")
(load "~/.emacs.d/config/layers/git.el")
(load "~/.emacs.d/config/layers/shell.el")
(load "~/.emacs.d/config/layers/spell-checking.el")
(load "~/.emacs.d/config/layers/syntax-checking.el")

;;; --- Languages setup
(load "~/.emacs.d/config/languages/cc.el")
(load "~/.emacs.d/config/languages/clojure.el")
(load "~/.emacs.d/config/languages/css.el")
(load "~/.emacs.d/config/languages/glsl.el")
(load "~/.emacs.d/config/languages/haskell.el")
(load "~/.emacs.d/config/languages/javascript.el")
(load "~/.emacs.d/config/languages/php.el")
(load "~/.emacs.d/config/languages/python.el")

(provide 'packages.el)
;;; packages.el ends here
