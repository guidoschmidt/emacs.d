;;; themes.el --- Configure themes
;;; Commentary:

;; ** Nighttime themes:
;; - junio (pack: sublime-themes)
;; - spolsky (pack: sublime-themes)
;; - soothe-theme
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

;; --- Advice functions for theme loading
(defun load-theme-disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme-disable-old-theme)

;; Re-set linum after loading a new theme.
(defun load-theme-restore-line-numbering(theme &rest args)
  "Set appearance again after loading any theme."
  ;; Line numbers appearance
  (setq linum-format 'linum-format-func)
  ;; Cursor
  (set-default 'cursor-type 'box)
  (set-cursor-color "#FA0C72"))
(advice-add 'load-theme :after #'load-theme-restore-line-numbering)

;; --- Packaged themes from MELPA
(use-package nyx-theme
  :ensure t)

(use-package hemera-theme
  :ensure t)

(use-package spacegray-theme
  :ensure t)

(use-package soothe-theme
  :ensure t)

;; --- Circadian
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((" 8:00" . hemera)
                           ("14:00" . nyx)
                           ("20:00" . soothe)))
  (circadian-setup))


(provide 'themes)
;;; themes.el ends here
