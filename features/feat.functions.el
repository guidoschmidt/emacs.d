;;; feat.functions.el --- Functions for third party packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Functions to improve or extend third party packages.

;;; Code:
(require 'iedit)

(defun iedit-dwim (arg)
  "Start iedit on ARG but use \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
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

(provide 'feat.functions)
;;; feat.functions.el ends here
