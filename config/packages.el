;;; packages.el --- Setup packages
;;; Commentary:

;; TODO:
;; - dired hacks: https://github.com/Fuco1/dired-hacks
;; - Setup common-lisp (slime-company)
;; - https://github.com/DarwinAwardWinner/ido-completing-read-plus
;; - https://github.com/technomancy/find-file-in-project
;; - https://github.com/emacscollective/auto-compile
;; - https://github.com/syohex/emacs-anzu

;;; Code:
;;; --- Keep .emacs.d clean
(use-package no-littering
  :ensure
  :config
  (require 'recentf)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; --- Try packages without installing them
(use-package try
  :ensure
  :commands try)

;;; --- Setup which-key
(use-package which-key
  :commands which-key
  :config (which-key-mode)
  :diminish (which-key-mode . "w"))

;;; --- keyfreq
(use-package keyfreq
  :ensure
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; --- Smex
(use-package smex
  :ensure)

;;; --- EditorConfig
(use-package editorconfig
  :ensure
  :config
  (editorconfig-mode 1))

;;; --- Yasnippets
(use-package yasnippet
  :ensure
  :commands (yas-global-mode yas-minor-mode)
  :config
  (yas-global-mode 1)
  (eval-after-load 'yasnippet
    (yas-load-directory "~/.emacs.d/snippets")))

;;; --- Wakatime
(use-package wakatime-mode
  :ensure
  :commands global-wakatime-mode
  :config
  (setq wakatime-api-key "32135691-bb0b-462e-94c2-b364aa352a6c")
  (global-wakatime-mode))

;;; --- Hydra
(use-package hydra
  :ensure
  :config
  (defhydra hydra-text-scale (global-map "<f10>")
    "Text Zoom"
    ("+" text-scale-increase "increase")
    ("<" text-scale-decrease "decrease")
    ("0" text-scale-adjust "adjust")))

;;; --- Focus mode
(use-package focus
  :commands focus-mode)

;;; --- Highlight indentation
(use-package highlight-indent-guides
  :ensure
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;; --- Powerline & Spaceline
(use-package sky-color-clock
  :load-path "~/.emacs.d/github/sky-color-clock"
  :config
  (when calendar-latitude
   (sky-color-clock-initialize (round calendar-latitude))))

(use-package powerline
  :ensure)

(use-package fancy-battery
  :ensure)

(use-package spaceline
  :ensure
  :after powerline
  :init
  (progn
    (require 'spaceline-config)
    ;; -- Powerline seperator styles:
    ;; alternate, arrow, arrow-fade, bar, box, brace,
    ;; butt, chamfer, contour, curve, rounded, roundstub,
    ;; wave, zigzag, utf-8, nil
    (setq powerline-default-separator nil)
    ;;(setq powerline-height 42)
    (setq powerline-gui-use-vcs-glyph t)
    ;; -- Disable spaceline segments
    ;; (spaceline-toggle-workspace-number-off)
    ;; (spaceline-toggle-minor-modes-off)
    ;; (spaceline-toggle-buffer-encoding-abbrev-off)
    ;; (spaceline-toggle-buffer-size-off)
    ;; (spaceline-toggle-org-clock-off)
    ;; (spaceline-toggle-projectile-root-off)
    ;; (spaceline-toggle-battery-off)
    ;; (spaceline-toggle-selection-info-off)
    ;; (spaceline-toggle-evil-state-off)
    ;; (spaceline-toggle-buffer-id-off)
    ;; (spaceline-toggle-major-mode-off)
    ;; (spaceline-toggle-minor-modes-off)
    ;; (spaceline-toggle-flycheck-error-off)
    ;; (spaceline-toggle-flycheck-info-off)
    ;; (spaceline-toggle-flycheck-warning-off)
    ;; (spaceline-toggle-version-control-off)
    ;; (spaceline-toggle-line-column-off)
    ;; (spaceline-toggle-global-off)
    ;; (spaceline-toggle-hud-off)
    ;; (spaceline-toggle-buffer-position-off)
    ;; (spaceline-toggle-buffer-modified-off)
    )
  :config
  (spaceline-emacs-theme))

(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (setq spaceline-all-the-icons-separator-type 'slant)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; -- Define custom segments
  (spaceline-define-segment sky-color-clock-segment
    (concat "" (sky-color-clock))
    :tight t)
  (spaceline-define-segment evil-state-segment
    "The current evil state.  Requires `evil-mode' to be enabled."
    (when (bound-and-true-p evil-local-mode)
      (s-trim (evil-state-property evil-state :tag t))))
  ;; -- Turn segemnts off
  (spaceline-toggle-all-the-icons-buffer-id-off)
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
  (spaceline-toggle-all-the-icons-flycheck-status-on)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-position-on)
  (spaceline-toggle-all-the-icons-package-updates-on)
  (spaceline-toggle-all-the-icons-minor-modes-on)
  (spaceline-all-the-icons-theme
   'sky-color-clock-segment
   'evil-state-segment
   'etc))

;;; --- Neo-tree with icons
(use-package neotree
  :ensure
  :commands neotree-toggle
  :config
  (use-package all-the-icons
    :ensure)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;; --- Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; --- Rainbow mode
(use-package rainbow-mode
  :ensure
  :commands rainbow-mode)

;;; --- Projectile
(use-package projectile
  :ensure
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure
  :after projectile
  :config
  (counsel-projectile-mode))

;;; --- Dump-Jump
(use-package dumb-jump
  :commands (dumb-jump-go-other-window
             dumb-jump-go
             dumb-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;;; --- Undo-tree
(use-package undo-tree
  :ensure
  :commands global-undo-tree-mode
  :init (global-undo-tree-mode))

;;; --- Auto highlight words
(use-package auto-highlight-symbol
  :ensure
  :config (global-auto-highlight-symbol-mode t))

;;; --- Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure
  :config
  (when (memq window-system '(mac ns x))
    (setq explicit-shell-file-name "/bin/zsh")
    (setq shell-file-name "zsh")
    (exec-path-from-shell-initialize)))

(when (memq window-system '(w32))
  (print "WIND"))


;;; --- Setup ace-window
(use-package ace-window
  :ensure
  :init (global-set-key [remap other-window] 'ace-window)
  :config
  (setq aw-background nil)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-forground
                   :foreground "white"
                   :background "black"))))))

;;; --- Ivy
(use-package ivy
  :ensure
  :commands (ivy-mode ivy-switch-buffer)
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-display-style 'fancy)
  ;; Advise swiper to recenter on exit
  (defun bjm-swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (advice-add 'swiper :after #'bjm-swiper-recenter)
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-switch-buffer-map
         ("v" . nil)
         ("V" . nil)
         ("r" . ibuffer-do-revert)))

;;; --- Swiper - better isearch
(use-package counsel
  :ensure)

(use-package swiper
  :ensure
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-ag))
  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;;; --- Avy
(use-package avy
  :ensure
  :commands (avy-goto-char avy-goto-char-timer)
  :bind (("C-c a" . avy-goto-char)
         ("C-c o" . avy-goto-char-timer)))

;;; --- Fill collumn indicator
(use-package fill-column-indicator
  :ensure
  :config
  (setq fci-rule-width 2)
  (setq-default fci-rule-column 80)
  (setq-default fci-rule-color "#252525")
  (setq-default whitespace-style '(face trailing))
  (add-hook 'prog-mode-hook 'fci-mode))

;;; --- Wrap region
(use-package wrap-region
  :ensure
  :commands wrap-region-mode
  :config
  (wrap-region-add-wrapper "`" "`")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper """ """)
  (wrap-region-add-wrapper "'" "'")
  (wrap-region-mode t))

;;; -- Refactor
(use-package emr
  :ensure
  :commands emr-show-refactor-menu
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  :bind
  (("<M-RET>" . emr-show-refactor-menu)))

;;; -- Aggressive indent
(use-package aggressive-indent
  :ensure
  :commands aggressive-indent-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

;;; --- Language specific
;;; --- Android
(use-package gradle-mode
  :commands gradle-mode)

;;; --- iTerm2 Support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;;; --- fish shell
(use-package fish-mode
  :ensure
  :commands fish-mode
  :config
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'fish_indent-before-save))))

;;; --- Load additional layers
(load "~/.emacs.d/config/layers/autocomplete.company.el")
(load "~/.emacs.d/config/layers/git.el")
(load "~/.emacs.d/config/layers/orgmode.el")
(load "~/.emacs.d/config/layers/shell.el")
(load "~/.emacs.d/config/layers/spell-checking.el")
(load "~/.emacs.d/config/layers/syntax-checking.el")
(load "~/.emacs.d/config/notifications.el")

;;; --- Languages setup
(load "~/.emacs.d/config/languages/arduino.el")
(load "~/.emacs.d/config/languages/cc.el")
(load "~/.emacs.d/config/languages/clojure.el")
(load "~/.emacs.d/config/languages/clisp.el")
(load "~/.emacs.d/config/languages/css.el")
(load "~/.emacs.d/config/languages/elisp.el")
(load "~/.emacs.d/config/languages/glsl.el")
(load "~/.emacs.d/config/languages/haskell.el")
(load "~/.emacs.d/config/languages/javascript.el")
(load "~/.emacs.d/config/languages/kotlin.el")
(load "~/.emacs.d/config/languages/markup.el")
(load "~/.emacs.d/config/languages/php.el")
(load "~/.emacs.d/config/languages/python.el")
(load "~/.emacs.d/config/languages/rest.el")
(load "~/.emacs.d/config/languages/swift.el")

(provide 'packages.el)
;;; packages.el ends here
