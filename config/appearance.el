;;; appearance.el --- Configure look & feel of Emacs

;;; Commentary:

;;; Code:

;;; --- Frames & windows
;;; Remove startup message
(setq inhibit-startup-message t)

;; Disable menubar
(when (not window-system)
  (menu-bar-mode 1))

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Cursor
(set-default 'cursor-type 'box)
(set-cursor-color "#FF0C42")

;;; --- Themes
;; ** Nighttime themes
;; - junio (sublime-themes)
;; - soothe-theme
;;
;; ** Daytime themes
;; - espresso (custom)
;; - flatui-theme
;; - material-light (material-theme)
(if (window-system)
    (if (or
	 (>= (nth 2 (decode-time (date-to-time (current-time-string)))) 19)
	 (<= (nth 2 (decode-time (date-to-time (current-time-string))))  7))
	;; Nigth theme
	(use-package soothe-theme
	  :ensure t
	  :config
	  (load-theme 'soothe t))
      ;; Day theme
      (use-package flatui-theme
	:ensure t
	:config
	(load-theme 'flatui t)))
  ;; Fallback
  (load-theme 'wombat t))

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
(setq linum-format "%4d")

;;; --- Indentation
(use-package smart-tabs-mode
  :ensure t
  :config
  (progn
    (smart-tabs-insinuate 'c 'javascript)))

(provide 'apperance)
;;; apperance.el ends here
