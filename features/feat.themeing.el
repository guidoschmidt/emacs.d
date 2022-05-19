;;; feat.themeing.el --- Themeing and tools related to themes -*- lexical-binding: t; -*-

;;; Commentary:
;;; Themes and theme related tooling

;;; Code:
(use-package doom-themes :straight t)
(use-package soothe-theme :straight t)
(use-package eink-theme :straight t)
(use-package ample-theme :straight t)
(use-package gruvbox-theme :straight t)
(use-package flatui-theme :straight t)
(use-package kaolin-themes :straight t)
(use-package atom-dark-theme :straight t)
(use-package solo-jazz-theme :straight t)
(use-package curry-on-theme
  :straight (cury-on-theme :type git
                           :host github
                           :repo "mvarela/Curry-On-theme"))

(use-package circadian
  :straight t
  :init
  (setq calendar-latitude 49.0)
  (setq calendar-longitude 8.5)
  (setq circadian-themes '((:sunrise . doom-material)
                           (:sunset  . doom-challenger-deep)))
  (add-hook 'emacs-startup-hook #'circadian-setup))

;; Font settings
(use-package alfontzo
  :straight (alfontzo :type git
		                  :host github
		                  :repo "guidoschmidt/alfontzo")
  :config
  (alfontzo-init))

;; Rainbow delimiters, color highlight parenthesis
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Color coded hex/rgb/hls values
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Use beautiful icons
(use-package all-the-icons-ivy
  :straight t)

;; Use beautiful icons also in ivy
(use-package all-the-icons-ivy-rich
  :straight t
  :config
  (all-the-icons-ivy-rich-mode t))

;; highliht indentation
(use-package highlight-indent-guides
  :straight t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  :hook (prog-mode . highlight-indent-guides-mode))

;; Set line spacing
(setq-default line-spacing 2)

(provide 'feat.themeing)
;;; feat.themeing.el ends here
