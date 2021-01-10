;;; feat.modeline.el --- Modeline suing doom modile -*- lexical-binding: t; -*-

;;; Commentary:
;;; Setting up doom modile

;;; Code:
(defface evil-normal-state-face
  `((t (:foreground "#F3F2F1"
        :background "#333333"
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
  :straight t
  :config
  (defvar custom-evil-state)
  (doom-modeline-def-segment
   custom-evil-state
   (evil-state-char))
  (setq find-file-visit-truename t)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-height 20)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-lsp t)
  ;; Define custom doom-modeline configuration
  (doom-modeline-def-modeline 'gs
    '(bar custom-evil-state vcs buffer-encoding buffer-info)
    '(lsp major-mode))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'gs 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
  :hook
  (after-init . doom-modeline-mode))

(provide 'feat.modeline)
;;; feat.modeline.el ends here
