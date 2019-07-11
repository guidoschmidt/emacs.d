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

;; (use-package omnisharp
;;   :ensure t
;;   :config
;;   (eval-after-load
;;       'company
;;     '(add-to-list 'company-backends 'company-omnisharp))
;;   (define-key omnisharp-mode-map (kbd "<C-tab>") 'omnisharp-auto-complete)
;;   (define-key omnisharp-mode-map "." 'omnisharp-add-dot-and-auto-complete)
;;   :hook ((csharp . omnisharp)
;;          (csharp . flyckcheck)))

(use-package omnisharp
  :ensure t
  ;; :straight (omnisharp
  ;;            :type git
  ;;            :host github
  ;;            :repo "bbbscarter/omnisharp-emacs")
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  :hook (csharp-mode . omnisharp-mode))

(provide 'lang.csharp)
;;; lang.csharp ends here
