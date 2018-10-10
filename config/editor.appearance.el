;;; editor.appearance --- Configure ligatures for fonts -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;; Set title bar text color
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . light))

;;; Remove startup message
(setq inhibit-startup-message t)

;; Pixelwise resizing of frames
(setq frame-resize-pixelwise t)

(setq default-frame-alist
      '((ns-transparent-titlebar . t)
        (ns-appearance . 'nil)))

;; Disable menubar
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
(global-hl-line-mode 1)

;; Font configuration
(use-package alfontzo
  :ensure t
  :straight (alfontzo
             :type git
             :host github
             :repo "guidoschmidt/alfontzo")
  :config
  (alfontzo-init))

;; Line numbers
(global-display-line-numbers-mode 1)

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
  (setq beacon-color "#FEBB9F")
  (setq beacon-blink-delay 0.01)
  (setq beacon-blink-duration 1.75)
  :config
  (beacon-mode 1))

(provide 'editor.appearance)
;;; editor.appearance ends here
