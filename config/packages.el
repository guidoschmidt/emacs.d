;;; packages.el --- Manage MELPA packages
;;; Commentary:

;; TODO:
;; Move evil configuration into it's own file

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
  :config (which-key-mode)
  :diminish (which-key-mode . "w"))

;;; --- keyfreq
(use-package keyfreq
  :ensure
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; --- Smex
(use-package smex
  :ensure t)

;;; --- Smartparens
(use-package smartparens
  :ensure
  :config
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

;;; --- Setup evil mode
(use-package evil-leader
  :ensure
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'ivy-switch-buffer
    "n" 'ivy-switch-buffer-other-window
    "k" 'ido-kill-buffer

    "s" 'magit-status

    "p" 'counsel-projectile-switch-project
    "f" 'counsel-projectile-find-file))

(use-package evil
  :ensure
  :config
  (evil-mode t)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)))

(use-package evil-cleverparens
  :ensure
  :config
  (add-hook 'emacs-lisp-mode #'evil-cleverparens-mode)
  (add-hook 'clojure-mode #'evil-cleverparens-mode))

(use-package evil-paredit
  :ensure
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
  (add-hook 'clojure-mode 'evil-paredit-mode))

;;; --- EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;;; --- Yasnippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (eval-after-load 'yasnippet
    (yas-load-directory "~/.emacs.d/snippets"))
  :bind
  (("C-c e" . yas-expand)))

;;; --- Hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

;;; FIC-Mode
(use-package fic-mode
  :ensure
  :config
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  (add-something-to-mode-hooks '(c++
                                 emacs-lisp
                                 vue
                                 jsx
                                 rjsx
                                 web
                                 sass)
                               (lambda () (fic-mode 1))))

;;; --- Powerline & Spaceline
(use-package powerline
  :ensure t)

(use-package fancy-battery
  :ensure t)

(use-package spaceline
  :ensure t
  :after powerline
  :init
  (progn
    (require 'spaceline-config)
    ;; -- Powerline seperator styles:
    ;; alternate, arrow, arrow-fade, bar, box, brace,
    ;; butt, chamfer, contour, curve, rounded, roundstub,
    ;; wave, zigzag, utf-8, nil
    (setq powerline-default-separator nil)
    ;; Disable spaceline segments
    (spaceline-toggle-workspace-number-off)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-org-clock-off)
    ;; Enable spaceline segments
    (spaceline-toggle-projectile-root-on)
    (spaceline-toggle-battery-on)
    (spaceline-toggle-selection-info-on)
    ;; Select theme
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
  (global-set-key [remap other-window] 'ace-window)
  :config
  (setq aw-background nil)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-forground
                   :foreground "white"
                   :background "black"))))))

;;; --- Rainbow mode
(use-package rainbow-mode
  :ensure t)

;;; --- Ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-display-style 'fancy))

;;; --- Swiper - better isearch
(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-ag))
  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

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

;;; --- Fill collumn indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 2)
  (setq-default fci-rule-column 80)
  (setq-default fci-rule-color "lightgray")
  (setq-default whitespace-style '(face trailing))
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;;; --- Wrap region
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-add-wrapper "`" "`")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper """ """)
  (wrap-region-add-wrapper "'" "'")
  (wrap-region-mode t))

;;; -- Refactor
(use-package emr
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  :bind
  (("<M-RET>" . emr-show-refactor-menu)))

;;; -- Aggressive indent
(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

;;; --- Language specific
;;; --- Android
(use-package gradle-mode
  :ensure)

;;; --- Load additional layers
;; Auto-completion via company
(load "~/.emacs.d/config/layers/autocomplete.company.el")
;; Auto-completion via auto-complete
;; (load "~/.emacs.d/config/layers/autocomplete.auto-complete.el")
(load "~/.emacs.d/config/layers/git.el")
(load "~/.emacs.d/config/layers/shell.el")
(load "~/.emacs.d/config/layers/spell-checking.el")
(load "~/.emacs.d/config/layers/syntax-checking.el")
(load "~/.emacs.d/config/notifications.el")

;;; --- Languages setup
(load "~/.emacs.d/config/languages/arduino.el")
(load "~/.emacs.d/config/languages/cc.el")
(load "~/.emacs.d/config/languages/clojure.el")
(load "~/.emacs.d/config/languages/css.el")
(load "~/.emacs.d/config/languages/elisp.el")
(load "~/.emacs.d/config/languages/glsl.el")
(load "~/.emacs.d/config/languages/haskell.el")
(load "~/.emacs.d/config/languages/javascript.el")
(load "~/.emacs.d/config/languages/markup.el")
(load "~/.emacs.d/config/languages/php.el")
(load "~/.emacs.d/config/languages/python.el")
(load "~/.emacs.d/config/languages/rest.el")
(load "~/.emacs.d/config/languages/swift.el")

(provide 'packages.el)
;;; packages.el ends here
