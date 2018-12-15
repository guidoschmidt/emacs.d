;;; editor.packages --- Setup packages -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'recentf)

;; no-littering - Keep your .emacs.d clean
(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; try - Try packages without installing them
(use-package try
  :ensure t
  :commands try)

;; which-key - Display available key bindings in a popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.333
        which-key-idle-delay 0
        which-key-popup-type 'side-window
        which-key-side-window-location 'left))

;; keyfreq - gather statistics of key and command frequency
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; dashboard - Startup dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title ""))

;; Anzu - show matching selections on search
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

;; Smex - smart M-x enhancement
(use-package smex
  :ensure t)

;; EditorConfig - adapt Emacs to .editorconfig files
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Yasnippets - code snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Hydra - popup with options after pressing a leader key
(use-package hydra
  :ensure t)

(defhydra hydra-text-scale ()
  "Text Scaling"
  ("+" text-scale-increase "increase")
  ("<" text-scale-decrease "decrease")
  ("0" text-scale-adjust "adjust"))

(evil-leader/set-key "+" 'hydra-text-scale/body)

;; highlight-indent-guides - Highlight indentation
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; rainbow-delimiters - color highlight parenthesis according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((clojurescript-mode . rainbow-delimiters-mode)
   (clojure-mode . rainbow-delimiters-mode)
   (prog-mode . rainbow-delimiters-mode)))

;; rainbow-mode - highlight HTML color codes with the corresponding color
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; projectile - provide a searchable interface for projects
(use-package diminish
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile-switch-project
  :after projectile)

;; dump-jump - "jump to definition"
(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go-other-window
             dumb-jump-go
             dumb-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;; undo-tree - advanced undo actions
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

;; auto-highlight-symbol - highlight matching expressions on selection
(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t))

;; exec-path-from-shell - executer commands from you'r systems shell
(defvar explicit-shell-file-name)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (setq explicit-shell-file-name "/bin/zsh")
    (setq shell-file-name "zsh")
    (exec-path-from-shell-initialize)))

;; switch-window - drop ace-window in favor of this
(use-package switch-window
  :ensure t)

;; ace-link - effectively jump between links
(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

;; ivy - generic completion frontend for emacs
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-change-readonly-file t))

(use-package wgrep-ag
  :ensure t)

(use-package ivy
  :ensure t
  :commands (ivy-mode ivy-switch-buffer)
  :diminish ivy-mode
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-recenter ()
    "Advice swiper to recenter on exit."
    (recenter))
  (advice-add 'swiper :after #'swiper-recenter)
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b"   . ivy-switch-buffer)
   :map ivy-switch-buffer-map
   ("v" . nil)
   ("V" . nil)))

;; counsel - ivy-enhanced versiosn of Emacs-commands
(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-c g"   . counsel-ag)
   ("C-x C-f" . counsel-find-file)))

;; swiper - isearch replacement for Emacs
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)))

;; avy - jump to characters and expressions
(use-package avy
  :ensure t
  :commands (avy-goto-char avy-goto-char-timer)
  :custom-face
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  (custom-set-faces
   '(avy-lead-face
     ((t (:inherit avy-lead-face
                   :background "#FF33B2"
                   :foreground "white"))))
   '(avy-lead-face-0
     ((t (:inherit avy-lead-face-0
                   :background "#33FFB2"
                   :foreground "black")))))
  :bind
  (("C-c a" . avy-goto-char)
   ("C-c o" . avy-goto-char-timer)
   ("C-c e" . swiper-avy)))

;; fill-collumn-indicator - show a line at 80 characters
(use-package fill-column-indicator
  :ensure t
  :hook (prog-mode . fci-mode)
  :config
  (setq fci-rule-width 1)
  (setq-default fci-rule-column 80)
  (setq-default fci-rule-color "#b3bbf3")
  (setq-default whitespace-style '(face trailing)))

;; Wrap region - wrap a region with
(use-package wrap-region
  :ensure t
  :commands wrap-region-mode
  :disabled
  :config
  (wrap-region-add-wrapper "`" "`")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper """ """)
  (wrap-region-add-wrapper "'" "'")
  (wrap-region-mode t))

;; emr - refactor menu
(use-package emr
  :ensure t
  :disabled
  :commands emr-show-refactor-menu
  :hook (prog-mode . emr-initialize)
  :config
  ;; Fix emr popups when using fci
  ;; (fill column indicator) mode
  (defun toggle-fci (show-emr &rest args)
    (fci-mode -1)
    (apply show-emr args))
  (advice-add 'emr-show-refactor-menu :around #'toggle-fci)
  (advice-add 'emr-show-refactor-menu :after #'(lambda () (fci-mode)))
  :bind
  (("<M-RET>" . emr-show-refactor-menu)))

;; aggressive-indent - aggressively ensure indentation
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook (emacs-lisp . aggressive-indent-mode))

;; visual-regexp
(use-package visual-regexp
  :ensure t
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

;; parnifer - improved lisp editing
(use-package parinfer
  :ensure t
  :after lispy
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults
             pretty-parens
             evil
             lispy
             smart-tab
             smart-yank))
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; fzf - fuzzy file finder
(use-package fzf
  :ensure t
  ;; :ensure-system-package (fzf . "brew install fzf")
  :config
  (evil-leader/set-key "y" 'fzf))

;; deadgrep
(use-package deadgrep
  :ensure t
  ;; :ensure-system-package (ripgrep . "brew install ripgrep")
  :config
  (evil-leader/set-key "ü" 'deadgrep))

;; Hideshow - code folding
(defvar hs-special-modes-alist
  (mapcar 'purecopy
          '((c-mode "{" "}" "/[*/]" nil nil)
            (c++-mode "{" "}" "/[*/]" nil nil)
            (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
            (java-mode "{" "}" "/[*/]" nil nil)
            (js-mode "{" "}" "/[*/]" nil)
            (javascript-mode "{" "}" "/[*/]" nil))))

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(defhydra hydra-hideshow (:color "#F2D30B" :hint nil)
  "Hideshow

_k_: hide block
_j_: show block
_t_: toggle block"
  ("j" hs-hide-block)
  ("k" hs-show-block)
  ("t" hs-toggle-hiding))

(evil-leader/set-key
  "h" 'hydra-hideshow/body)

;; Smart hungry delete
(use-package smart-hungry-delete
  :ensure t
  :bind (("C-<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

;; Helpful
(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(provide 'editor.packages)
;;; editor.packages ends here
