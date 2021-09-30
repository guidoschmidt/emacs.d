;;; lang.stylesheets.el --- Stylesheets -*- lexical-binding: t; -*-

;;; Commentary:
;;; Stylesheet language configuration for CSS + SASS.

;;; Code:
(use-package emmet-mode
  :straight t
  :hook
  ((web-mode     . emmet-mode)
   (html-mode    . emmet-mode)
   (makdown-mode . emmet-mode)
   (sass-mode    . emmet-mode)
   (scss-mode    . emmet-mode)
   (rjsx-mode    . emmet-mode)))

(use-package sass-mode
  :straight t
  :commands sass-mode
  :config
  (defun custom/sass-mode-hook ()
    "Hook to customize SASS mode."
    (setq rainbow-html-colors t)
    (rainbow-mode)
    (setq emmet-use-sass-syntax t))
  :hook (sass-mode . custom/sass-mode-hook))

(provide 'lang.stylesheets)
;;; lang.stylesheets.el ends here
