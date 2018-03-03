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
          (set-frame-font font-string nil t))
      (message (concat font " not available")))
    (message (concat "Emacs typeface: " font " @ size " (number-to-string size)))))

(defun scale-with (size scalar)
  "Scale a typeface SIZE with a SCALAR."
  (round (* size scalar)))

(defconst host-roxy "Roxy.local")
(defconst host-emma "Emma.local")
(defcustom host-type-scales '()
  "List of typographic scales used at different hosts."
  :type 'alist
  :group 'fontset)
(add-to-list 'host-type-scales `(,host-roxy . 0.8125))
(add-to-list 'host-type-scales `(,host-emma . 1.125))

(require 'cl-lib)
(defun scale-from-host ()
  "Determine the typographic scale from the host."
  (cl-first
   (cl-remove-if-not (lambda (config)
                       (let ((c-hostname (car config)))
                         (string-equal c-hostname (system-name))))
                     host-type-scales)))

(defun font-for-os ()
  "Set the font for a matched operating system."
  (cl-first
   (cl-remove-if-not (lambda (entry)
                       (let ((os (car entry))
                             (size (cdr entry)))
                         (string-equal system-type os)))
                     os-font-map)))


(defun typescale (scale)
  "Adjust the typescale of the current system."
  (interactive)
  ;; TODO: add interactive prompt for parameters
  ;; TODO: add assoc list update
  (let ((host-scalar (cdr (scale-from-host)))
        (os-typescale (cdr (font-for-os)))
        (host (car (scale-from-host))))
    (let ((scalar (/ scale (float os-typescale))))
      (add-to-list 'host-type-scales `(,host . ,scalar))
      (set-font font-typeface (scale-with os-typescale scalar)))))

(typescale 15)

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
