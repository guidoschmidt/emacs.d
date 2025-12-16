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
  ;; :disabled
  :straight t
  :bind
  (("M-x" . counsel-M-x)
   ("C-c g" . counsel-ag)
   ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  ;; :disabled
  :straight t
  :after projectile)

;;; ivy - generic completion frontend
(use-package ivy
  ;; :disabled
  :straight t
  :diminish ivy-mode
  :config
  (setq enable-recursive-minibuffers nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-height 35)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-magic-slash-non-match-action nil)
  (defun swiper-recenter ()
    "Advice swiper to recenter on exit."
    (recenter))
  (advice-add 'swiper :after #'swiper-recenter)
  (ivy-mode t))

(use-package ivy-posframe
  :disabled
  :straight t
  :config
  ;; (setq ivy-posframe-height-alist '((t . 40)))
  ;; (setq ivy-posframe-height 20)
  (setq ivy-posframe-height 40)
  (setq ivy-posframe-min-width 100)
  (setq ivy-posframe-width 100)
  (setq ivy-posframe-parameters
        '((left-fringe  . 12)
          (right-fringe . 12)))
  (setq ivy-posframe-border-width 40)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (defun ivy-posframe-theme-hook (theme)
    (set-face-attribute 'ivy-posframe-border nil :background (face-background 'default)))
  (add-hook 'circadian-after-load-theme-hook 'ivy-posframe-theme-hook)
  (ivy-posframe-mode 1))

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
  ;; :disabled
  :bind
  (("C-s" . swiper)))

;;; avy - jump to characters and expressions fast
(use-package avy
  :straight t
  :custom-face
  :config
  (setq avy-background t)
  (setq avy-all-windows 'all-frames)
  (custom-set-faces
   '(avy-lead-face
     ((t (:inherit avy-lead-face
                   :background "#42424ace2"
                   :foreground "#eeeeee"))))
   '(avy-lead-face-0
     ((t (:inherit avy-lead-face-0
                   :background "#feca32"
                   :foreground "#424242")))))
  :bind
  (("C-c e" . avy-goto-char)
   ("C-c o" . avy-goto-char-timer)))

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
  (when (not (windows?))
    (exec-path-from-shell-initialize)))

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
  (setq treemacs-user-mode-line-format 'none)
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
                             :family "Atkinson Hyperlegible Mono"
                             :height 110))))))))

(use-package treemacs-evil
  :after treemacs evil
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package treemacs-icons-dired
  :disabled
  :after treemacs dired
  :straight t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

;; dired
;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
;; dashboard
(use-package dashboard
  :straight t
  :init
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.emacs.d/logo.txt")
  (setq dashboard-items '((recents  . 20)
                          (projects . 6)
                          (registers . 5)))
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "Have a good coding session")
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

;; view large files
(use-package vlf
  :straight t
  :config
  (require 'vlf-setup))

;; string inflection
(use-package string-inflection
  :straight t
  :config
  (require 'string-inflection))

;; log files
(use-package logview
  :straight t)

;; editorconfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; helpful
(use-package helpful
  :straight (helpful
             :type git
             :host github
             :repo "Wilfred/helpful"))

;; Inline calc mode
(use-package literate-calc-mode
  :straight t)

;; Insert paths into minibuffer prompts in Emacs
(use-package consult-dir
  :disabled
  :straight t)

;; Provides search and navigation commands based on completing-read (Emacs default)
(use-package consult
  :disabled
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("C-s"     . consult-line)
   ("C-x C-f" . find-file))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5))

;; Performant and minimalistic vertical completion UI with default completion system
(use-package vertico
  :disabled
  :straight t
  :custom
  (vertico-count 40)
  (vertico-resize t)
  (vertico-cycle t)
  (setq vertico-scroll-margin 20)
  :init
  (vertico-mode))

(use-package vertico-posframe
  :disabled
  :straight t
  :after vertico
  :config
  (setq vertico-posframe-border-width 20)
  (defun vertico-posframe-theme-hook (theme)
    (set-face-attribute 'vertico-posframe-border nil :background (face-background 'default))
    (set-face-attribute 'vertico-posframe-border-2 nil :background (face-background 'default))
    (set-face-attribute 'vertico-posframe-border-3 nil :background (face-background 'default))
    (set-face-attribute 'vertico-posframe-border-4 nil :background (face-background 'default))
    (set-face-attribute 'vertico-posframe-border-fallback nil :background (face-background 'default)))
  (add-hook 'circadian-after-load-theme-hook 'vertico-posframe-theme-hook)
  (add-hook 'consult-theme 'vertico-posframe-theme-hook)
  :init
  (when (not (windows?))
   (vertico-posframe-mode 1)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :disabled
  :init
  (savehist-mode))

;; Adding extra metadata for completions in the margins
(use-package marginalia
  :disabled
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (when (not (windows?))
    (marginalia-mode)))

;; Completion style for matching regexps in any order
(use-package orderless
  :disabled
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :disabled
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :disabled
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :disabled
  :straight t
  :init
  (global-corfu-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))



(provide 'feat.editor)
;;; feat.editor.el ends here
