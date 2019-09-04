;;; themes --- Configure themes
;;; Commentary:

;;  Nighttime themes:
;; - junio (pack: sublime-themes)
;; - spolsky (pack: sublime-themes)
;; - soohe-theme
;; - spacegray (pack: spacegray-theme)
;;
;;  Daytime themes:
;; - flatui-theme
;; - material-light (pack: material-theme)
;; - twilight-bright (pack: twilight-bright-theme)
;; - espresso-theme
;; - ample-theme

;;; Code:
;; --- Theme load paths
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/"
             "~/.emacs.d/straight/repos/zaiste-emacs-theme/")

;;; --- Re-set linum after loading a new theme.
(use-package ample-theme                    :ensure :defer)
(use-package apropospriate-theme            :ensure :defer)
(use-package boron-theme                    :ensure :defer)
(use-package birds-of-paradise-plus-theme   :ensure :defer)
(use-package color-theme-sanityinc-tomorrow :ensure :defer)
(use-package darkokai-theme                 :ensure :defer)
(use-package espresso-theme                 :ensure :defer)
(use-package flatui-theme                   :ensure :defer)
(use-package flatland-theme                 :ensure :defer)
(use-package gruvbox-theme                  :ensure :defer)
(use-package hemera-theme                   :ensure :defer)
(use-package jazz-theme                     :ensure :defer)
(use-package kaolin-themes                  :ensure :defer)
(use-package material-theme                 :ensure :defer)
(use-package nord-theme                     :ensure :defer)
(use-package nyx-theme                      :ensure :defer)
(use-package soothe-theme                   :ensure :defer)
(use-package subatomic-theme                :ensure :defer)
(use-package tao-theme                      :ensure :defer)
(use-package color-theme-solarized          :ensure :defer)
(use-package abyss-theme                    :ensure :defer)
(use-package challenger-deep-theme          :ensure :defer)
(use-package srcery-theme                   :ensure :defer)
(use-package zerodark-theme                 :ensure :defer)
(use-package spacemacs-theme                :ensure :defer)
(use-package kooten-theme                   :ensure :defer)
(use-package panda-theme                    :ensure :defer)
(use-package brutalist-theme                :ensure :defer)
(use-package atom-dark-theme                :ensure :defer)
(use-package atom-one-dark-theme            :ensure :defer)
(use-package doom-themes                    :ensure :defer)
(use-package zaiste-theme
  :straight (zaiste-theme
             :type git
             :host github
             :repo "zaiste/zaiste-emacs-theme"))

;; --- Circadian
(use-package circadian
  ;; :load-path "~/git/develop/emacs/circadian.el"
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . zaiste)
                           (:sunset  . doom-Iosvkem)))
  (circadian-setup)
  (add-hook 'after-init-hook 'circadian-setup))

(add-hook 'circadian-after-load-theme-hook
          #'(lambda (theme)
              ;; (setq linum-format 'linum-format-func)
              ;; Cursor
              (set-default 'cursor-type 'bo)
              (set-cursor-color "#F55D13")
              ;; Remove box style from modeline
              (set-face-attribute 'mode-line nil :box nil)
              ;; Set evil-modes cursor colors
              '(evil-emacs-state-cursor (quote ("#F55D13" hbar)) t)
              '(evil-insert-state-cursor (quote ("#03F5E5" bar)) t)
              '(evil-normal-state-cursor (quote ("#F52503" box)) t)
              '(evil-visual-state-cursor (quote ("#F3F3F2" box)) t)))

(provide 'themes)
;;; themes ends here
