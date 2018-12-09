;;; lang.csharp --- Setup C# language

;;; Commentary:

;;; Code:
(use-package csharp-mode
  :ensure t
  :mode ("\\.cs\\'" "\\.fx\\'"))

(defun csharp-endline ()
  "Insert semicolon and move point/cursor to the next line."
  (interactive)
  (insert ";")
  (newline)
  (move-beginning-of-line 1))

(define-key csharp-mode-map (kbd "<C-return>") 'csharp-endline)

(provide 'lang.csharp)
;;; lang.csharp ends here
