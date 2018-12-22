;;; modeline --- Customize modeline -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'solar)

(use-package sky-color-clock
  :straight (sky-color-clock
             :type git
             :host github
             :repo "zk-phi/sky-color-clock")
  :config
  (eval-when-compile
    (when calendar-latitude
      (sky-color-clock-initialize (round calendar-latitude)))))

(defface evil-normal-state-face
  `((t (:foreground "#F7B2EE"
        :background "#A273FF"
        :weight ultra-bold)))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface evil-insert-state-face
  `((t (:foreground "#1A7162" :background "#B2EFE5" :weight ultra-bold)))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface evil-visual-state-face
  `((t (:foreground "#E9391D" :background "#FFA96F" :weight ultra-bold)))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface evil-leader-state-face
  `((t (:foreground "#FFFFFF" :background "#000000" :weight ultra-bold)))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defun evil-state-char ()
  "Show starting character of evil state and propertize with respective face."
  (let ((state (symbol-value 'evil-state)))
    (cond ((equal 'normal state) (propertize " N "
                                             'face 'evil-normal-state-face))
          ((equal 'insert state) (propertize " I "
                                             'face 'evil-insert-state-face))
          ((equal 'visual state) (propertize " V "
                                             'face 'evil-visual-state-face)))))

(use-package doom-modeline
  :ensure t
  :defer t
  :config
  (defvar skycolor-clock)
  (doom-modeline-def-segment skycolor-clock
    (concat " " (sky-color-clock) " "))

  (defvar custom-evil-state)
  (doom-modeline-def-segment custom-evil-state
    (evil-state-char))
  (doom-modeline-def-modeline
   'gs
   ;; Left mode line segments
   '(bar
     workspace-number
     window-number
     custom-evil-state
     "  "
     matches
     buffer-info
     selection-info)
   ;; Right mode line segments
   '(major-mode
     vcs
     flycheck
     skycolor-clock))
  (doom-modeline-set-modeline 'gs t)
  (setq doom-modeline-height 30)
  :hook (after-init . doom-modeline-init))

(provide 'modeline)
;;; modeline ends here
