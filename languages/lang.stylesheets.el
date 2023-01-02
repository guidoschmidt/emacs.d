;;; lang.stylesheets.el --- Stylesheets -*- lexical-binding: t; -*-

;;; Commentary:
;;; Stylesheet language configuration for CSS + SASS.

;;; Code:
(use-package sass-mode
  :straight t
  :commands (sass-mode scss-mode)
  :config
  (setq-default flycheck-stylelintrc "~/.stylelintrc.json")
  (defun custom/sass-mode-hook ()
    "Hook to customize SASS mode."
    (setq rainbow-html-colors t)
    (rainbow-mode)
    (setq emmet-use-sass-syntax t))
  (defun sass-scss-company-mode-hook ()
    (setq-local company-backends
                '((company-files :with company-dabbrev company-yasnippet))))
  (add-hook 'scss-mode-hook #'sass-scss-company-mode-hook)
  :hook '(sass-mode . custom/sass-mode-hook))

(provide 'lang.stylesheets)
;;; lang.stylesheets.el ends here
