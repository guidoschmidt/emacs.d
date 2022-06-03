;;; core.functions.el --- Custom functions for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Basic functions to extend Emacs behavior.

;;; Code:
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

(defun new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buffer))

(defun insert-lambda-arrow ()
  "Insert '=>'."
  (interactive)
  (insert-char ?\ )
  (insert-char ?\()
  (insert-char ?\))
  (insert-char ?\ )
  (insert-char ?\=)
  (insert-char ?\>)
  (insert-char ?\ ))

(provide 'core.functions)
;;; core.functions.el ends here
