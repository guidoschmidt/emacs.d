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
  :disabled
  :straight t
  :after sky-color-clock
  :config
  (defvar sky-clock)
  (doom-modeline-def-segment
    sky-clock
    (sky-color-clock))
  (defvar custom-evil-state)
  (doom-modeline-def-segment
   custom-evil-state
   (evil-state-char))
  (setq find-file-visit-truename nil)
  (setq doom-modeline-bar-width 0)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-lsp t)
  ;; Define custom doom-modeline configuration
  (doom-modeline-def-modeline 'gs
    '(custom-evil-state major-mode buffer-info)
    '(lsp vcs sky-clock))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'gs 'default)
    (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))
  :hook
  (after-init . doom-modeline-mode))

(use-package sky-color-clock
  :straight (sky-color-clock
             :type git
             :host github
             :repo "zk-phi/sky-color-clock")
  :config
  (eval-when-compile
    (when calendar-latitude
      (sky-color-clock-initialize (round calendar-latitude))))
  (setq sky-color-clock-enable-emoji-icon nil))

(use-package mood-line
  :straight (mood-line
             :type git
             :host github
             :repo "jessiehildebrandt/mood-line")
  :config
  (defun mood-line-segment-evil-state ()
    "Displays a customized evil state indicator."
    custom-evil-state)
  (setq mode-line-format
        '((:eval
           (mood-line--format
            ;; Left
            (format-mode-line
             '(""
               (:eval (evil-state-char))
               (:eval (sky-color-clock))
               " "
               (:eval (mood-line-segment-modified))
               (:eval (mood-line-segment-buffer-name))
               (:eval (mood-line-segment-anzu))
               (:eval (mood-line-segment-multiple-cursors))
               (:eval (mood-line-segment-position))))
            ;; Right
            (format-mode-line
             '((:eval (mood-line-segment-eol))
               (:eval (mood-line-segment-encoding))
               (:eval (mood-line-segment-vc))
               (:eval (mood-line-segment-major-mode)) 
               (:eval (mood-line-segment-flycheck))
               (:eval (mood-line-segment-flymake))
               (:eval (mood-line-segment-process))
               " "))))))

  (mood-line-mode))

(provide 'feat.modeline)
;;; feat.modeline.el ends here
