;;; core.configuration.el --- Emacs core settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; First party Emacs configurations and settings

;;; Code:

;;; -------------------------------------------------------------------------------------------
;;; VARIABLE SETTINGS
;;; -------------------------------------------------------------------------------------------
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; byte compilation
(setq byte-compile-warnings '(cl-functions))

;; Remove startup messages
(setq inhibit-startup-message t)

;; Set max line width
(setq-default fill-column 80)

;; Define a file used for storing customization information.
(setq custom-file "~/.emacs.d/local/custom.el")

;; Apropos sortage by relevancy
(setq-default apropos-sort-by-scores t)

;; Disable visual bell
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Allow pixelwise frame sizing
(setq frame-resize-pixelwise t)

;; Set the default directory for C-x C-f
(setq default-directory "~/")

;; Disable window decorations
(when (windows?)
  (set-frame-parameter nil 'undecorated t))

;; Move backup files
(setq backup-directory-alist
      '((".*" "~/.emacs.d/backup/")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq make-backup-files nil)

;; Move auto-save files
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))

;; Keymap basics
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq select-enable-clipboard t)

;; Indentation defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; yes-or-no shortcut for dialogues
(defalias 'yes-or-no-p 'y-or-n-p)


;;; -------------------------------------------------------------------------------------------
;;; SERVER + DAEMON
;;; -------------------------------------------------------------------------------------------
;; Emacs core configuration
;; Start server for daemon usage
(server-start)


;;; -------------------------------------------------------------------------------------------
;;; MODES
;;; -------------------------------------------------------------------------------------------
;; Interactive do mode
(ido-mode t)

;; Electric pairs
(electric-pair-mode t)

;; Time display
(display-time-mode t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Disable toolbar, menu-bar and scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set window fringe
(fringe-mode 20)

;; Enable winner mode for window history
(winner-mode t)

;; When delete-selection-mode is active, typed or pasetd text
;; will delete a selcted text region
(delete-selection-mode t)

;; Disable .# files
(setq-default create-lockfiles nil)

;;; -------------------------------------------------------------------------------------------
;;; HOOKS
;;; -------------------------------------------------------------------------------------------
;; Column indicator at 80 characters
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;; -------------------------------------------------------------------------------------------
;;; i18n Settings
;;; -------------------------------------------------------------------------------------------
;; Setup UTF8
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8)


(provide 'core.configuration)
;;; core.configuration.el ends here
