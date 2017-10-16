;;; appearance.el --- Configure look & feel of Emacs

;;; Commentary:

;;; Code:

;;; --- Frames & windows
;;; Remove startup message
(setq inhibit-startup-message t)

;; Disable menubar
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;;; --- Typeface
;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Iosevka-Term" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Iosevka-11"))
    (add-to-list 'default-frame-alist '(font . "Iosevka-11"))))
 ((string-equal system-type "darwin")  ; Mac OS X
  (when (member "Monoid" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Monoid-15"))
    (add-to-list 'default-frame-alist '(font . "Monoid-15"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Iosevka-Term" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Iosevka-20"))
    (add-to-list 'default-frame-alist '(font . "Iosevka-20")))))

;;; --- Whitespace
(global-whitespace-mode t)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;; --- Highlight current line
(global-hl-line-mode 1)

;;; --- Line numbers
;;; TODO: replace nlinum with native line numbers:
;;; https://lists.gnu.org/archive/html/emacs-devel/2017-06/msg00338.html
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (setq nlinum-format "%3d"))

(use-package nlinum-hl
  :ensure t
  :after nlinum
  :config
  (setq nlinum-highlight-current-line t))


;;; --- Indentation
(use-package smart-tabs-mode
  :ensure t
  :config
  (progn
    (smart-tabs-insinuate 'c 'javascript)))

(provide 'appearance)
;;; appearance.el ends here
