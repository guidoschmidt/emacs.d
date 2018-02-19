;;; editor.appearance --- Configure look & feel of Emacs

;;; Commentary:

;;; Code:
;;; Remove startup message
(setq inhibit-startup-message t)

;; Disable menubar
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;;; Typeface
;; set default font in initial window and for any new window
(defconst font-typeface "PragmataPro")

(defconst os-windows "windows-nt")
(defconst os-mac "darwin")
(defconst os-linux "linux")
(defcustom os-font-map '()
  "List of fonts used at different operating systems."
  :type 'alist
  :group 'fontset)
(add-to-list 'os-font-map `(,os-windows . 12))
(add-to-list 'os-font-map `(,os-mac . 18))
(add-to-list 'os-font-map `(,os-linux . 20))

(defun set-font (font size)
  "Set Emacs font via `set-frame-font' with a FONT name and SIZE."
  (interactive)
  (let ((font-string (concat font "-" (int-to-string size))))
    (add-to-list 'initial-frame-alist `(font . ,font-string))
    (add-to-list 'default-frame-alist `(font . ,font-string))
    (if (member font (font-family-list))
        (progn
          (set-frame-font font-string nil t)))
    (message (concat font " not available"))))

(defun set-font-for-os ()
  "Set the font for a matched operating system."
  (defun set-font-by-match (entry)
    (let ((os (car entry))
          (size (cdr entry)))
      (when (string-equal system-type os)
        (message os)
        (set-font font-typeface size))))
  (mapc #'set-font-by-match os-font-map))

(set-font-for-os)

;; TODO:
;; (defun scale-with (size scale)
;;   (round (* size scale)))
;;
;; (defconst host-roxy "Roxy.local")
;; (defconst host-emma "Emma.local")
;; (defcustom host-type-scales '((host-roxy . 0.875)
;;                               (host-emma . 1.125))
;;   "List of typographic scales used at different hosts."
;;   :type 'alist
;;   :group 'fontset)

;; Whitespace
(global-whitespace-mode t)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Highlight current line
(global-hl-line-mode 1)

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

;; Load additional appearance related package
;; configurations from ./appearance
(require 'ligatures)
(require 'modeline)
(require 'themes)

(provide 'editor.appearance)
;;; editor.appearance ends here
