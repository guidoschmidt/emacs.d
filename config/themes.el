;;; themes.el --- Configure themes
;;; Commentary:

;; ** Nighttime themes:
;; - junio (pack: sublime-themes)
;; - spolsky (pack: sublime-themes)
;; - soohe-theme
;; - spacegray (pack: spacegray-theme)
;;
;; ** Daytime themes:
;; - flatui-theme
;; - material-light (pack: material-theme)
;; - twilight-bright (pack: twilight-bright-theme)
;; - espresso-theme
;; - ample-theme

;;; Code:

;; --- Theme load paths
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;; --- Re-set linum after loading a new theme.
(use-package ample-theme :ensure :defer)
(use-package apropospriate-theme :ensure :defer)
(use-package darkokai-theme :ensure :defer)
(use-package espresso-theme :ensure :defer)
(use-package kaolin-themes :ensure :defer)
(use-package nord-theme :ensure :defer)
(use-package tao-theme :ensure :defer)
(use-package soothe-theme :ensure :defer)
(use-package nyx-theme :ensure :defer)
(use-package hemera-theme :ensure :defer)
(use-package nord-theme :ensure :defer)
(use-package gruvbox-theme :ensure :defer)
(use-package darkokai-theme :ensure :defer)
(use-package material-theme :ensure :defer)
(use-package jazz-theme :ensure :defer)
(use-package subatomic-theme :ensure :defer)
(use-package flatui-theme :ensure :defer)

;; --- Circadian
(use-package circadian
  ;; :load-path "~/git/develop/emacs/circadian.el/"
  :ensure
  :init
  :config
  (setq calendar-latitude 49.329896)
  (setq calendar-longitude 8.570925)
  (setq circadian-themes '((:sunrise . flatui)
                           (:sunset . gruvbox-dark-hard)))
  (circadian-setup))

(add-hook 'circadian-after-load-theme-hook
          #'(lambda (theme)
              ;; Line numbers appearance
              (setq linum-format 'linum-format-func)
              ;; Cursor
              (set-default 'cursor-type 'box)
              (set-cursor-color "#F52503")))

(provide 'themes)
;;; themes.el ends here
