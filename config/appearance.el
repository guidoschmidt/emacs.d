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

;; Cursor
(set-default 'cursor-type 'box)
(set-cursor-color "#FF0C42")

;;; --- Themes
;; ** Nighttime themes
;; - junio (pack: sublime-themes)
;; - spolsky (pack: sublime-themes)
;; - soothe-theme
;;
;; ** Daytime themes
;; - espresso (custom)
;; - flatui-theme
;; - material-light (pack: material-theme)
;; - twilight-bright (pack: twilight-bright-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
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
      (use-package gruvbox-theme
        :ensure t
        :config
        (load-theme 'gruvbox-dark-soft t)))
  ;; ### Fallback
  (load-theme 'leuven t))

;;; --- Typeface
;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Monoid" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Monoid-11"))
    (add-to-list 'default-frame-alist '(font . "Monoid-11"))))
 ((string-equal system-type "darwin"); Mac OS X
  (when (member "Monoid" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Monoid-14"))
    (add-to-list 'default-frame-alist '(font . "Monoid-14"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Monoid" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Monoid-14"))
    (add-to-list 'default-frame-alist '(font . "Monoid-14")))))

;;; --- Whitespace
(global-whitespace-mode -1)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;; --- Line numbers
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (setq nlinum-format "%3d"))

(use-package nlinum-relative
    :config
    (add-hook 'prog-mode-hook 'nlinum-relative-mode))

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

(provide 'apperance)
;;; apperance.el ends here
