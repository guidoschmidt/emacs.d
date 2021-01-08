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

;; Allow pixelwise frame sizing
(setq frame-resize-pixelwise t)

;; Disable window decorations
(when (memq system-type '(windows-nt))
  (set-frame-parameter nil 'undecorated t))


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


;; evil-mode
(use-package evil
	     :straight t
	     :config
	     (evil-mode t))
(use-package evil-leader
	     :straight t
	     :config
	     (global-evil-leader-mode t))


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
  :commands magit-status
  :config
  (setq magit-diff-paint-whitespace t)
  (setq magit-completion-read-function 'ivy-completion-read))

(use-package evil-magit
  :straight t
  :after evil
  :config
  (setq evil-magit-use-y-for-yank t))


(provide 'init.el)
;;; init.el ends here
