;;; editor.appearance --- Configure ligatures for fonts -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;; Remove startup message
(setq inhibit-startup-message t)

;; Pixelwise resizing of frames
(setq frame-resize-pixelwise t)

(setq default-frame-alist
      '((ns-transparent-titlebar . t)
        (ns-appearance . 'light)))

;; Disable window decoration in Windows

;; Disable menubar
(set-frame-parameter nil 'undecorated t)
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Minimize the fringe of windows
(fringe-mode 1)

;; Whitespace
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Highlight current line
(global-hl-line-mode t)

;; Font configuration
(use-package alfontzo
  :straight
  (alfontzo
   :type git
   :host github
   :repo "guidoschmidt/alfontzo")
  :config
  (alfontzo-init))

;; Line numbers
(global-display-line-numbers-mode t)

;; Indentation
(use-package smart-tabs-mode
  :ensure t
  :config
  (progn
    (smart-tabs-insinuate 'c 'javascript)))

;; pretty-mode
(use-package pretty-mode
  :ensure t
  :disabled
  :config
  (global-pretty-mode t)
  (pretty-deactivate-groups
   '(:equality
     :ordering
     :ordering-double
     :ordering-triple
     :arrows
     :arrows-twoheaded
     :punctuation
     :logic
     :sets))
  (pretty-deactivate-patterns
   '(:lambda)))

;; beacon - cursor light
(use-package beacon
  :ensure t
  :init
  (setq beacon-color "#FFFFFF")
  (setq beacon-blink-delay 0.01)
  (setq beacon-blink-duration 1.75)
  :config
  (beacon-mode t))

(provide 'editor.appearance)
;;; editor.appearance ends here
