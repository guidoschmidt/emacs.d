;;; appearance.el --- Configure look & feel of Emacs

;;; Commentary:

;;; Code:

;;; --- Frames & windows
;;; Remove startup message
(setq inhibit-startup-message t)

;; Disable menubar
(when (not window-system)
  (menu-bar-mode -1))

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;;; --- Themes
;; ** Nighttime themes
;; junio (sublime-themes), soothe
;;
;; ** Daytime themes
;; espresso, flatui, material-light
(use-package soothe-theme
  :ensure t)
(use-package sublime-themes
  :ensure t)
(use-package flatui-theme
  :ensure t)
(use-package material-theme
  :ensure t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'junio t)

;;; --- Typeface
;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Hasklig" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Hasklig-12"))
    (add-to-list 'default-frame-alist '(font . "Hasklig-12"))))
 ((string-equal system-type "darwin"); Mac OS X
  (when (member "Hasklig" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Hasklig-16"))
    (add-to-list 'default-frame-alist '(font . "Hasklig-16"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Hasklig" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Hasklig-16"))
    (add-to-list 'default-frame-alist '(font . "Hasklig-16")))))

;;; --- Whitespace
(global-whitespace-mode -1)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;; --- Line numbers
(global-linum-mode 1)
(setq Linum-format "%4d")

;;; --- Indentation
(use-package smart-tabs-mode
  :ensure t)



(provide 'apperance.el)
;;; apperance.el ends here
