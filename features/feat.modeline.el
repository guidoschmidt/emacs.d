;;; feat.modeline.el --- Modeline suing doom modile -*- lexical-binding: t; -*-

;;; Commentary:
;;; Setting up modeline

;;; Code:
(defvar-local evil-box-height 10)
(defvar-local evil-n--background "#2213f7")
(defvar-local evil-n--foreground "#9983ef")
(defvar-local evil-i--background "#0bb7ae")
(defvar-local evil-i--foreground "#92efef")
(defvar-local evil-v--background "#d63319")
(defvar-local evil-v--foreground "#ffcaad")

(defface evil-normal-state-face
  `((t (:foreground ,evil-n--foreground
                    :background ,evil-n--background
                    :weight bold
                    :box (:line-width ,evil-box-height :color ,evil-n--background))))
  "Face for warnings in the modeline - Used by `*flycheck'."
  :group 'modeline)

(defface evil-insert-state-face
  `((t (:foreground ,evil-i--foreground
         :background ,evil-i--background
         :weight bold
         :box (:line-width ,evil-box-height :color ,evil-i--background))))
  "Face for warnings in the modeline - used by `*flycheck'."
  :group 'modeline)

(defface evil-visual-state-face
  `((t (:foreground ,evil-v--foreground
        :background ,evil-v--background
        :weight bold
        :box (:line-width ,evil-box-height :color ,evil-v--background))))
  "Face for warnings in the modeline - used by `*flycheck'."
  :group 'modeline)

(defun evil-state-char ()
  "Show starting character of evil state and propertize with respective face."
  (let ((state (symbol-value 'evil-state)))
    (cond ((equal 'normal state) (propertize " ■ "
                                             'face 'evil-normal-state-face))
          ((equal 'insert state) (propertize " ▶ "
                                             'face 'evil-insert-state-face))
          ((equal 'visual state) (propertize " ● "
                                             'face 'evil-visual-state-face)))))

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
  (defun customize-mood-line ()
    (mood-line-mode)
    (setq-default
     mode-line-format
          '((:eval
             (mood-line--format
              ;; Left
              (format-mode-line
               '(""
                 (:eval (evil-state-char))
                 "  "
                 (:eval (mood-line-segment-modified))
                 (:eval (mood-line-segment-buffer-name))
                 (:eval (mood-line-segment-anzu))
                 ;; (:eval (mood-line-segment-multiple-cursors))
                 ;; (:eval (mood-line-segment-position))
                 ))
              ;; Right
              (format-mode-line
               '((:eval (mood-line-segment-eol))
                 (:eval (mood-line-segment-encoding))
                 (:eval (mood-line-segment-vc))
                 (:eval (mood-line-segment-major-mode))
                 ;;(:eval (mood-line-segment-flycheck))
                 ;;(:eval (mood-line-segment-flymake))
                 (:eval (mood-line-segment-process))
                 "   "
                 (:eval (sky-color-clock)))))))))
  (customize-mood-line))

(provide 'feat.modeline)
;;; feat.modeline.el ends here
