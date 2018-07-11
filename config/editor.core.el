;;; editor.core --- Setup core editor behaviour -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; Start the server
(server-start)

(setq auto-window-vscroll nil)

;; Network security
(setq network-security-level 'high)

;; Setup calendar geo location
(require 'solar)
(setq calendar-latitude 49.329896)
(setq calendar-longitude 8.570925)

;; Setup language environment
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;; Apropos sortage by relevancy
(require 'apropos)
(setq apropos-sort-by-scores t)

;; Enable interactive do mode
(require 'ido)
(ido-mode t)

;; Move backup files
(setq backup-directory-alist
      `((".*" "~/.emacs.d/backup/" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Move auto-save files
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))

;; Electric pairs
(electric-pair-mode 1)

;; AutoFill mode
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use yes-or-no shortcut
(defalias 'yes-or-no-p 'y-or-n-p)

;; Delete selections
(delete-selection-mode t)

;; Show time
(display-time-mode 1)

;; Use ibuffers instead of default buffer overview
(defalias 'list-buffers 'ibuffer)

;; Enable winner mode for window setup history
(winner-mode 1)

;; Change scratch buffer message to empty string
(when (window-system)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message ""))

;; Enable show-paren-mode
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disabel backup files
(setq make-backup-files nil)

;; Disable visual bell
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; Custom function for moving lines up/down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive)
  (defvar ml-start)
  (defvar ml-end)
  (let ((col (current-column)))
    (beginning-of-line) (setq ml-start (point))
    (end-of-line) (forward-char) (setq ml-end (point))
    (let ((line-text (delete-and-extract-region ml-start ml-end)))
      (forward-line n)
      (insert line-text)
      (forward-line -1)
      (forward-char col))))

(defun move-line-up ()
  "Move the current line one line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the current line one line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; Custom function for reloading a currently opened file
(defun reload-current-file ()
  "Reload the file loaded in current buffer from the disk."
  (interactive)
  (cond (buffer-file-name
         (progn (find-alternate-file buffer-file-name)
                (message "File reloaded.")))
        (t (message "Not editing a file."))))

;; Custom function to duplicate a whole line
(defun duplicate-line()
  "Duplicate a line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (forward-line 1)
  (yank))

;; Custom function to insert a checkmark
(defun insert-checkmark()
  (interactive)
  (insert-char (string-to-char "✓")))

;; Custom function to create a new empty buffer in a separate frame
(defun new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; Custom function to create a new empty buffer
(defun new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((bn "untitled")
        (num 1))
    (while
        (get-buffer (concat bn (number-to-string num)))
      (setq num (1+ num)))
    (switch-to-buffer
     (concat bn (number-to-string num)))))

;; iedit customization's
(advice-add 'iedit-mode
            :after (lambda (&optional ignore)
                     (when iedit-mode
                       (minibuffer-message "iedit session started. Press C-; to end."))))

(defun ap/iedit-mode (orig-fn)
  "Call `iedit-mode' with function-local scope by default, or global scope if called with a universal prefix."
  (interactive)
  (pcase current-prefix-arg
    ('nil (funcall orig-fn '(0)))
    ('(4) (funcall orig-fn))
    (_ (user-error "`ap/iedit-mode' called with prefix: %s" prefix))))

;; Override default `iedit-mode' function with advice.
(advice-add #'iedit-mode :around #'ap/iedit-mode)

(provide 'editor.core)
;;; editor.core ends here
