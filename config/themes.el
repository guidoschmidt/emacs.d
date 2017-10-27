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
(defun load-theme-restore-line-numbering(theme &rest args)
  "Set appearance again after loading any theme."
  ;; Line numbers appearance
  (setq linum-format 'linum-format-func)
  ;; Cursor
  (set-default 'cursor-type 'box)
  (set-cursor-color "#FA0C72"))
(advice-add 'load-theme :after #'load-theme-restore-line-numbering)

(use-package apropospriate-theme :ensure :defer)
(use-package darkokai-theme :ensure :defer)
(use-package tao-theme :ensure :defer)
(use-package nord-theme :ensure :defer)
;; (use-package soothe-theme :ensure :defer)
;; (use-package nyx-theme :ensure :defer)
;; (use-package hemera-theme :ensure :defer)
;; (use-package nord-theme :ensure :defer)
;; (use-package gruvbox-theme :ensure :defer)
;; (use-package apropospriate-theme :ensure :defer)
;; (use-package darkokai-theme :ensure :defer)

;; --- Circadian
(use-package circadian
  ;; :load-path "~/Development/emacs/circadian.el/"
  :ensure
  :init
  :config
  (setq calendar-latitude 49.329896)
  (setq calendar-longitude 8.570925)
  (setq circadian-themes '((:sunrise . apropospriate-light)
                           (:sunset . darkokai)))
  (circadian-setup))

(provide 'themes)
;;; themes.el ends here
