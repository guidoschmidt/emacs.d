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
  :config
  (setq powerline-default-separator nil)
  (setq powerline-gui-use-vcs-glyph t)
  (setq powerline-height 42)
  (powerline-reset))

(use-package spaceline
  :ensure t
  :after powerline
  :config
  (require 'spaceline-config)
 
  ;; Define custom segments
  (spaceline-define-segment sky-color-clock-segment
    (concat "" (sky-color-clock))
    :tight t)

  (setq powerline-height 42)
  (powerline-reset)

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

;; (use-package all-the-icons
;;   :ensure t
;;   :init
;;   (when (memq window-system '(w32))
;;     (add-to-list 'load-path "~/.emacs.d/github/")
;;     (require 'font-lock+)))

;; (use-package spaceline-all-the-icons
;;   :ensure t
;;   :after (spaceline all-the-icons)
;;   :requires spaceline
;;   :config
;;   (require 'spaceline-config)
;;   ;; Customize spaceline
;;   (set-face-attribute
;;    'spaceline-evil-normal nil :background "#F6EDDF" :foreground "#232323")
;;   (set-face-attribute
;;    'spaceline-evil-insert nil :background "#B2EFE5" :foreground "##1A7162")
;;   (set-face-attribute
;;    'spaceline-evil-visual nil :background "#E9391D" :foreground "#FFA96F")
;;   (setq spaceline-all-the-icons-separator-type 'none)
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   ;; Define custom segments
;;   (spaceline-define-segment sky-color-clock-segment
;;     (concat "" (sky-color-clock))
;;     :tight t)
;;   ;; Turn segemnts off
;;   (spaceline-toggle-all-the-icons-buffer-path-off)
;;   (spaceline-toggle-all-the-icons-buffer-position-off)
;;   (spaceline-toggle-all-the-icons-buffer-size-off)
;;   (spaceline-toggle-all-the-icons-modified-off)
;;   (spaceline-toggle-all-the-icons-region-info-off)
;;   (spaceline-toggle-all-the-icons-time-off)
;;   ;; Turn segments on
;;   (spaceline-toggle-all-the-icons-buffer-id-on)
;;   (spaceline-toggle-all-the-icons-buffer-size-on)
;;   (spaceline-toggle-all-the-icons-flycheck-status-on)
;;   (spaceline-toggle-all-the-icons-git-status-on)
;;   (spaceline-toggle-all-the-icons-hud-on)
;;   (spaceline-toggle-all-the-icons-minor-modes-on)
;;   (spaceline-toggle-all-the-icons-mode-icon-on)
;;   (spaceline-toggle-all-the-icons-narrowed-on)
;;   (spaceline-toggle-all-the-icons-package-updates-on)
;;   (spaceline-toggle-all-the-icons-position-on)
;;   (spaceline-toggle-all-the-icons-projectile-on)
;;   ;; Enable the theme
;;   (spaceline-emacs-theme
;;    'sky-color-clock-segment
;;    'etc))

(provide 'modeline)
;;; modeline ends here
