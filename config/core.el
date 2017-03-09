;;; core.el --- Setup core editor behaviour
;;; Commentary:


;;; Code:
;; Enable interactive do mode
(require 'ido)
(ido-mode t)

;; Electric pairs
(electric-pair-mode 1)

;; Use yes-or-no shortcut
(defalias 'yes-or-no-p 'y-or-n-p)

;; Delete selections
(delete-selection-mode t)

;; Use ibuffers instead of default buffer overview
(defalias 'list-buffers 'ibuffer)

;; Enable winner mode for window setup history
(winner-mode 1)

;; Change scratch buffer message to empty string
(when (window-system)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message ""))

;; Enable show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disabel backup files
(setq make-backup-files nil)

;; Disable visual bell
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Custom function for moving lines up/down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
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
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(provide 'core.el)
;;; core.el ends here
