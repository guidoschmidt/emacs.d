;;; modeline --- Customize modeline -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'solar)
(use-package sky-color-clock
  :load-path "~/.emacs.d/github/sky-color-clock"
  :config
  (eval-when-compile
    (when calendar-latitude
     (sky-color-clock-initialize (round calendar-latitude)))))

(use-package powerline
  :ensure t
  :disabled
  :config
  (setq powerline-default-separator 'bar)
  (setq powerline-gui-use-vcs-glyph t)
  (setq powerline-height 38)
  (powerline-reset))

(use-package spaceline
  :ensure t
  :disabled
  :after powerline
  :config
  (require 'spaceline-config)
  ;; Define custom segments
  (spaceline-define-segment sky-color-clock-segment
    (concat "" (sky-color-clock))
    :tight t)
  ;; Edit evil state colcos
  (set-face-attribute
   'spaceline-evil-normal nil :background "#F6EDDF" :foreground "#232323")
  (set-face-attribute
   'spaceline-evil-insert nil :background "#B2EFE5" :foreground "##1A7162")
  (set-face-attribute
   'spaceline-evil-visual nil :background "#E9391D" :foreground "#FFA96F")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; Turn segments OFF
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-anzu-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  ;; Turn segments ON
  (spaceline-toggle-buffer-size-on)
  (spaceline-toggle-buffer-modified-on)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-hud-on)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-point-position-off)
  (spaceline-toggle-projectile-root-on)
  ;; Setup modeline theme
  (spaceline-emacs-theme
   'sky-color-clock-segment
   'etc))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(provide 'modeline)
;;; modeline ends here
