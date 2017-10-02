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

;; --- Circadian
(use-package circadian
  :ensure
  :init
  (use-package soothe-theme :ensure :defer)
  (use-package nyx-theme :ensure :defer)
  (use-package hemera-theme :ensure :defer)
  (use-package gruvbox-theme :ensure :defer)
  :config
  (setq circadian-themes '(("8:00" . hemera)
                           ("10:00" . gruvbox-light-medium)
                           ("19:00" . nyx)
                           ("21:00" . soothe)
                           ("21:10" . gruvbox-dark-hard)))
  (circadian-setup))

(provide 'themes)
;;; themes.el ends here
