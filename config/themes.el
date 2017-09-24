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
  :load-path "~/Development/emacs/circadian.el"
  :ensure
  :init
  (use-package nyx-theme :ensure)
  (use-package hemera-theme :ensure)
  (use-package soothe-theme :ensure)
  :config
  (setq circadian-themes '(("8:00" . hemera)
                           ("20:00" . nyx)
                           ("20:02" . soothe)
                           ("20:04" . hemera)
                           ("20:20" . leuven)
                           ("20:21" . hemera)
                           ("20:34" . soothe)
                           ("20:36" . nyx)))
  (circadian-setup))

(provide 'themes)
;;; themes.el ends here
