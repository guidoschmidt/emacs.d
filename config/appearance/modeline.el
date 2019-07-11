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
  :config
  ;; Custom doom-modeline segments
  (defvar skycolor-clock)
  (doom-modeline-def-segment skycolor-clock
    (sky-color-clock))
  (defvar custom-evil-state)
  (doom-modeline-def-segment custom-evil-state
    (evil-state-char))
  ;; Configure doom-modeline variables
  (setq find-file-visit-truename t)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-height 22)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-lsp t)
  ;; Define custom doom-modeline configuration
  (doom-modeline-def-modeline 'gs
    '(bar custom-evil-state vcs buffer-encoding buffer-info)
    '(lsp major-mode skycolor-clock))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'gs 'default)) 
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
  :hook
  (after-init . doom-modeline-mode))

(provide 'modeline)
;;; modeline ends here
