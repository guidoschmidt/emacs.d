;;; editor.packages --- Setup packages -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; no-littering - Keep your .emacs.d clean
(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
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
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05))

;; smooth-scrolling - smooth scrolling and minimap
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

;; keyfreq - gather statistics of key and command frequency
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ranger - improved file browser
(use-package ranger
  :ensure t
  :diminish ranger-mode
  :commands ranger)

;; dashboard - Startup dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; Dired-Hacks - imporve dired-mode
(use-package dired-hacks-utils
  :ensure t
  :commands dired-mode
  :config
  (defconst my-dired-media-files-extensions
    '("mp3" "mp4" "avi" "mpg" "flv" "ogg")
    "Media files.")
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
  (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
  (dired-rainbow-define-chmod executable-unix "Green" "-[rw-]+x.*"))

;; Anzu - show matching selections on search
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

;; emojify - convert utf-8 smileys to images
(use-package emojify
  :ensure t
  :disabled
  :hook (after-init . global-emojify-mode))

;; Prodigy - manage external services from within Emacs
(use-package prodigy
  :disabled
  :ensure t)

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
  (yas-global-mode 1) 
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet))

;; Wakatime - track your coding time
(use-package wakatime-mode
 :if (boundp 'apikey-wakatime)
 :ensure t
 :commands global-wakatime-mode
 :config
 (setq wakatime-api-key apikey-wakatime)
 (global-wakatime-mode))

;; Hydra - popup with options after pressing a leader key
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-text-scale ()
    "
Text Scaling

"
    ("+" text-scale-increase "increase")
    ("<" text-scale-decrease "decrease")
    ("0" text-scale-adjust "adjust"))
  (evil-leader/set-key
    "+" 'hydra-text-scale/body))

;; Focus - visual highlight scopes
(use-package focus
  :ensure t
  :disabled
  :commands focus-mode)

;; highlight-indent-guides - Highlight indentation
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; neotree - provide a side panel with file tree view
(use-package neotree
  :disabled
  :ensure t
  :commands neotree-toggle
  :config
  ;; (custom-set-faces
  ;;  '(neo-dir-link-face ((t (:weight bold :height 100))))
  ;;  '(neo-file-link-face ((t (:weight normal :height 110))))
  ;;  '(neo-banner-face ((t :height 100))))
  (setq neo-theme (if (display-graphic-p) 'icons)))

;; rainbow-delimiters - color highlight parenthesis according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

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
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'grizzl))
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
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (setq explicit-shell-file-name "/bin/zsh")
    (setq shell-file-name "zsh")
    (exec-path-from-shell-initialize)))

;; ace-window - effectively jump between frames and windows
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

;; ivy - generic completion frontend for emacs
(use-package ivy
  :ensure t
  :commands (ivy-mode ivy-switch-buffer)
  :diminish ivy-mode
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-recenter (&rest args)
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
  (("C-s" . swiper)
   :map read-expression-map
   ("C-r" . counsel-expression-history)))

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

;; hungry-delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; counsel-spotify - control spotify from within Emacs
(use-package counsel-spotify
  :ensure t
  :config
  (setq counsel-spotify-client-id "b5b801a1c93741f09888dc6ac1bf11d0"
        counsel-spotify-client-secret "b03b24495a824f03ba70ae0e8da2a5a6"))

;; fzf - fuzzy file finder
(use-package fzf
  :ensure t
  :config
  (evil-leader/set-key "y" 'fzf))

(provide 'editor.packages)
;;; editor.packages ends here
