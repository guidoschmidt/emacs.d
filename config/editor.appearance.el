;;; editor.appearance --- Configure ligatures for fonts -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;; Remove startup message
(setq inhibit-startup-message t)

;; Pixelwise resizing of frames
(setq frame-resize-pixelwise t)

;; Disable menubar
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Minimize the fringe of windows
(fringe-mode 1)

;; Whitespace
(global-whitespace-mode t)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Highlight current line
(global-hl-line-mode 1)

;; Font configuration
(use-package alfontzo
  :load-path "~/git/develop/emacs/alfontzo"
  :config
  (alfontzo-init))

;; Line numbers
;; TODO: replace nlinum with native line numbers:
;; https://lists.gnu.org/archive/html/emacs-devel/2017-06/msg00338.html
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (setq nlinum-format "%4d"))

(use-package nlinum-hl
  :ensure t
  :after nlinum
  :config
  (setq nlinum-highlight-current-line t))

;; Indentation
(use-package smart-tabs-mode
  :ensure t
  :config
  (progn
    (smart-tabs-insinuate 'c 'javascript)))

;; pretty-mode
(use-package pretty-mode
  :ensure t
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
  :init
  (setq beacon-color "#F8E19D")
  :config
  (beacon-mode 1))

(provide 'editor.appearance)
;;; editor.appearance ends here
