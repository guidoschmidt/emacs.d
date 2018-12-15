;;; lang.csharp --- Setup C# language

;;; Commentary:

;;; Code:
(defun csharp-endline ()
  "Insert semicolon and move point/cursor to the next line."
  (interactive)
  (insert ";")
  (newline)
  (move-beginning-of-line 1))

(use-package csharp-mode
  :ensure t
  :mode ("\\.cs\\'" "\\.fx\\'")
  :bind
  (("<C-return>" . csharp-endline)))

(provide 'lang.csharp)
;;; lang.csharp ends here
