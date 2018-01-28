;;; modeline.el --- Customize modeline
;;; Commentary:

;;; Code:
;; --- Powerline & Spaceline
(use-package sky-color-clock
  :load-path "~/.emacs.d/github/sky-color-clock"
  :config
  (when calendar-latitude
    (sky-color-clock-initialize (round calendar-latitude))))

(use-package powerline
  :ensure
  :config
  (setq powerline-default-separator nil)
  (setq powerline-height 42)
  (setq powerline-gui-use-vcs-glyph t))

(use-package fancy-battery
  :ensure)

(use-package spaceline
  :ensure
  :after powerline
  ;; :init
  ;; (progn
  ;;   (require 'spaceline-config)
  ;;     ;;; -- Powerline seperator styles:
  ;;     alternate, arrow, arrow-fade, bar, box, brace,
  ;;     butt, chamfer, contour, curve, rounded, roundstub,
  ;;     wave, zigzag, utf-8, nil
  ;;     ;;; -- Disable spaceline segments
  ;;     (spaceline-toggle-workspace-number-off)
  ;;     (spaceline-toggle-minor-modes-off)
  ;;     (spaceline-toggle-buffer-encoding-abbrev-off)
  ;;     (spaceline-toggle-buffer-size-off)
  ;;     (spaceline-toggle-org-clock-off)
  ;;     (spaceline-toggle-projectile-root-off)
  ;;     (spaceline-toggle-battery-off)
  ;;     (spaceline-toggle-selection-info-off)
  ;;     (spaceline-toggle-evil-state-off)
  ;;     (spaceline-toggle-buffer-id-off)
  ;;     (spaceline-toggle-major-mode-off)
  ;;     (spaceline-toggle-minor-modes-off)
  ;;     (spaceline-toggle-flycheck-error-off)
  ;;     (spaceline-toggle-flycheck-info-off)
  ;;     (spaceline-toggle-flycheck-warning-off)
  ;;     (spaceline-toggle-version-control-off)
  ;;     (spaceline-toggle-line-column-off)
  ;;     (spaceline-toggle-global-off)
  ;;     (spaceline-toggle-hud-off)
  ;;     (spaceline-toggle-buffer-position-off)
  ;;     (spaceline-toggle-buffer-modified-off))
  ;; :config
  ;; (spaceline-emacs-theme)
)

(use-package all-the-icons
  :load-path "~/.emacs.d/github/all-the-icons"
  :init
  (when (memq window-system '(w32))
    (add-to-list 'load-path "~/.emacs.d/github/")
    (require 'font-lock+)))

(use-package spaceline-all-the-icons
  :load-path "~/.emacs.d/github/spaceline-all-the-icons"
  :after (spaceline all-the-icons)
  :config
  ;; -- Customize spaceline
  (set-face-attribute 
   'spaceline-evil-normal nil :background "#fafefd" :foreground "#232323")
  (set-face-attribute
   'spaceline-evil-insert nil :background "#3bffde" :foreground "#232323")
  (set-face-attribute
   'spaceline-evil-visual nil :background "#e012a0" :foreground "#fafefd")
  (setq spaceline-all-the-icons-separator-type 'none)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; -- Define custom segments
  (spaceline-define-segment sky-color-clock-segment
    (concat "" (sky-color-clock))
    :tight t)
  ;; -- Turn segemnts off
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-buffer-position-off)
  (spaceline-toggle-all-the-icons-buffer-size-off)
  (spaceline-toggle-all-the-icons-hud-off)
  (spaceline-toggle-all-the-icons-modified-off)
  (spaceline-toggle-all-the-icons-narrowed-off)
  (spaceline-toggle-all-the-icons-projectile-off)
  (spaceline-toggle-all-the-icons-region-info-off)
  (spaceline-toggle-all-the-icons-time-off)
  ;; -- Turn segments on
  (spaceline-toggle-all-the-icons-buffer-id-on)
  (spaceline-toggle-all-the-icons-flycheck-status-on)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-position-on)
  (spaceline-toggle-all-the-icons-package-updates-on)
  (spaceline-toggle-all-the-icons-minor-modes-on)
  (spaceline-all-the-icons-theme
   'sky-color-clock-segment
   'etc))

(provide 'modeline.el)
;;; modeline.el ends here
