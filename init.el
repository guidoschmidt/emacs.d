;;; init.el --- Main entry for Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Emacs core configuration
;; Start server for daemon usage
(server-start)

;; Remove startup messages
(setq inhibit-startup-message t)

;; Setup UTF8
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8)

;; Interactive do mode
(ido-mode t)

;; Electric pairs
(electric-pair-mode t)

;; Column indicator at 80 characters
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; yes-or-no shortcut for dialogues
(defalias 'yes-or-no-p 'y-or-n-p)

;; Define a file used for storing customization information.
(setq custom-file "~/.emacs.d/local/custom.el")

;; Time display
(display-time-mode t)

;; Apropos sortage by relevancy
(setq apropos-sort-by-scores t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Disable toolbar, menu-bar and scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set window fringe
(fringe-mode 20)

;; Enable winner mode for window history
(winner-mode t)

;; Disable visual bell
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Allow pixelwise frame sizing
(setq frame-resize-pixelwise t)

;; Disable window decorations
(when (memq system-type '(windows-nt))
  (set-frame-parameter nil 'undecorated t))

;; Move backup files
(setq backup-directory-alist
      '((".*" "~/.emacs.d/backup/")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq make-backup-files nil)

;; When delete-selection-mode is active, typed or pasetd text
;; will delete a selcted text region
(delete-selection-mode t)

;; Move auto-save files
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))

;; Keymap basics
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq select-enable-clipboard t)


;; straight.el bootstrapping
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


;; Clean emacs configuration with no littering
(use-package no-littering
  :straight t
  :init
  (require 'recentf)
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; Diminish
(use-package diminish
  :straight t)

;; evil-mode
(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "a"   'align-regexp
    "o"   'sort-lines
    "b"   'ivy-switch-buffer
    "x"   'frog-jump-buffer
    "i"   'ibuffer
    "n"   'ivy-switch-buffer-other-window
    "k"   'ido-kill-buffer
    "s"   'magit-status
    "p"   'counsel-projectile-switch-project
    "f"   'counsel-projectile-find-file
    "g"   'counsel-ag
    "w"   'save-buffer
    "j"   'swiper-avy
    "y"   'yas-insert-snippet
    "q"   'kill-emacs
    "r"   'reload-current-buffer
    "?"   'flyspell-correct-at-point
    "!"   'flyspell-add-word-to-dict
    "0"   'switch-window
    "TAB" 'indent-region
    "RET" 'eval-buffer))

(use-package evil
  :straight t
  :after evil-leader
  :config
  (evil-mode t))

(setq evil-want-C-u-scroll t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-magit
  :straight t
  :after evil
  :config
  (setq evil-magit-use-y-for-yank t))

;; provides gl and gL align operators
(use-package evil-lion
  :straight t
  :after evil
  :config
  (evil-lion-mode))

;; counsel
(use-package counsel
  :straight t
  :bind
  (("M-x" . counsel-M-x)
   ("C-c g" . counsel-ag)
   ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :straight t
  :after projectile)

;; ivy - generic completion frontend
(use-package ivy
  :straight t
  :diminish ivy-mode
  :config
  (setq enable-recursive-minibuffers nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-height 50)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-recenter ()
    "Advice swiper to recenter on exit."
    (recenter))
  (advice-add 'swiper :after #'swiper-recenter)
  (ivy-mode t))

(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode t))

(use-package all-the-icons-ivy
  :straight t)

(use-package all-the-icons-ivy-rich
  :straight t
  :config
  (all-the-icons-ivy-rich-mode t))

;; swiper - isearch replacement
(use-package swiper
  :straight t
  :bind
  (("C-s" . swiper)))

;; avy - jump to characters and expressions fast
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

;; frog-jumping
(use-package frog-jump-buffer
  :straight t)

;; Project management
(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (projectile-mode t))

;; Smart M-x enhancement, e.g. sorting items in M-x minibuffer by usage
(use-package smex
  :straight t)

;; company autocompletion
(use-package company
	     :straight t
	     :config
	     (global-company-mode t))

;; Themes and circadian for automatic time switching 
(use-package doom-themes :straight t)
(use-package soothe-theme :straight t)

(use-package circadian
  :straight t
  :init
  (setq calendar-latitude 49.0)
  (setq calendar-longitude 8.5)
  (setq circadian-themes '((:sunrise . doom-snazzy)
			   (:sunset  . doom-challenger-deep)))
  (add-hook 'emacs-startup-hook #'circadian-setup))


;; Font settings
(use-package alfontzo
  :straight (alfontzo :type git
		      :host github
		      :repo "guidoschmidt/alfontzo")
  :config
  (alfontzo-init))

;; Markdown
(use-package markdown-mode+
  :straight t
  :mode (("\\.md" . markdown-mode)
	 ("\\.mdx" . markdown-mode)
	 ("\\.makdown" . markdown-mode)))

;; Magit
(use-package magit
  :straight t
  :config
  (setq magit-diff-paint-whitespace t)
  (setq magit-completion-read-function 'ivy-completion-read))

;; Switch window
(use-package switch-window
  :straight t)

;; Indentation defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; Execute path from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Snippet system
(use-package yasnippet
  :straight t
  :config
  (setq yas-trigger-in-field t)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; Undo visualization
(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

;; Automatically highlight symbols matching the current selection
(use-package auto-highlight-symbol
  :straight t
  :hook (prog-mode . auto-highlight-symbol-mode))

;; Hydra
(use-package hydra
  :straight t)

;; Rainbow delimiters, color highlight parenthesis
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Color coded hex/rgb/hls values
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Smart hungry delete
(use-package smart-hungry-delete
  :straight t
  :bind
  (("C-<backspace>" . smart-hungry-delete-backward-char)
   ("C-d" . smart-hungry-delete-forward-char))
  :config
  (smart-hungry-delete-add-default-hooks))

;; Load custom functions
(add-to-list 'load-path "~/.emacs.d/core/")

(require 'core.functions)

;; Reset garbage collection. Not doing so will cause garbage
;; collection freezes during long-term interactive use. Conversely, a
;; gc-cons-threshold that is too small will cause stuttering. We use 16mb as our
;; default.
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 33554432 ; 32mb
          gc-cons-percentage 0.1)))

(provide 'init.el)
;;; init.el ends here
