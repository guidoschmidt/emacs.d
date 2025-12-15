;; feat.themeing.el --- Themeing and tools related to themes -*- lexical-binding: t; -*-

;;; Commentary:
;;; Themes and theme related tooling

;;; Code:
(use-package doom-themes :straight t)
(use-package soothe-theme :straight t)
(use-package eink-theme :straight t)
(use-package ample-theme :straight t)
(use-package tao-theme :straight t)
(use-package ample-zen-theme
  :straight (ample-zen-theme :type git
                             :host github
                             :repo "mjwall/ample-zen"))
(use-package gruvbox-theme :straight t)
(use-package base16-theme :straight (base16-theme
                                     :host github
                                     :repo "guidoschmidt/base16-emacs"))
(use-package flatui-theme :straight t)
(use-package kaolin-themes :straight t)
(use-package atom-dark-theme :straight t)
(use-package solo-jazz-theme :straight t)
(use-package nyx-theme :straight t)
(use-package danneskjold-theme :straight t)
(use-package stimmung-themes :straight t)
(use-package klere-theme :straight t)
(use-package humanoid-themes :straight t)
(use-package kanagawa-themes :straight t)

(use-package emacs-color-themes
  :straight (emacs-color-themes :type git
                                :host github
                                :repo "owainlewis/emacs-color-themes")
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/straight/repos/emacs-color-themes/themes"))
(use-package espresso-theme
  :straight (espresso-theme :type git
                            :host github
                            :repo "dgutov/espresso-theme"))
(use-package curry-on-theme
  :straight (cury-on-theme :type git
                           :host github
                           :repo "mvarela/Curry-On-theme"))

(use-package circadian
  :straight (circadian
             :type git
             :host github
             :repo "guidoschmidt/circadian.el")
  :init
  (setq circadian-verbose t)
  (setq calendar-latitude 49.398750)
  (setq calendar-longitude 8.672434)
  (setq circadian-themes '((:sunrise . base16-kanagawa)
                           (:sunset  . doom-ir-black)
                           ("23:59"  . doom-ir-black)))
  (add-hook 'emacs-startup-hook #'circadian-setup)
  (circadian-setup))

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
  :disabled
  :straight t)

;; Use beautiful icons also in ivy
(use-package all-the-icons-ivy-rich
  :disabled
  :straight t
  :config
  (all-the-icons-ivy-rich-mode t))

;; Colour tools
(use-package ct
  :straight (ct
             :host github
             :repo "neeasade/ct.el"
             :branch "master"))

;; highliht indentation
(use-package highlight-indent-guides
  :straight t
  :config
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face
                       (ct-edit-oklab-l-dec (face-background 'default) 1))
  (set-face-background 'highlight-indent-guides-even-face
                       (ct-edit-oklab-l-dec (face-background 'default) 2))
  (set-face-foreground 'highlight-indent-guides-character-face "#444")
  :hook (prog-mode . highlight-indent-guides-mode))

;; Set line spacing
(setq-default line-spacing 2)

;; highlight buffers
(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

;; spacious padding
(use-package spacious-padding
  :straight (spacious-padding :type git
                              :host github
                              :repo "protesilaos/spacious-padding")
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 20
          :header-line-width 0
          :mode-line-width 0
          :tab-width 2
          :right-divider-width 0
          :scroll-bar-width 0
          :fringe-width 0))
  (spacious-padding-mode 1))

(provide 'feat.themeing)
;;; feat.themeing.el ends here
