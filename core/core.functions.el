;;; core.functions.el --- Custom functions for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Basic functions to extend Emacs behavior.

;;; Code:
(require 'iedit)

(defun duplicate-line ()
  "Duplicate a line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (forward-line 1)
  (yank))

(defun move-line-up ()
  "Move the active line upwards."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the active line downwards."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun reload-current-buffer ()
  "Reload the file loaded in the current buffer from disk."
  (interactive)
  (cond (buffer-file-name
         (progn (find-alternate-file buffer-file-name)
                (message "File reloaded.")))
        (t (message "Buffer has no file attached to reload."))))

(defun iedit-dwim (argument)
  "Start iedit on ARGUMENT but use \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if argument
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))


(provide 'core.functions)
;;; core.functions.el ends here
