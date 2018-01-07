;;; css.el --- CSS settings
;;; Commentary:

;;; Code:
;;; --- CSS
(defun custom/css-mode-hook ()
  "Hook to customize css mode."
  (setq-default rainbow-html-colors t)
  (rainbow-mode)
  (emmet-mode))
(add-hook 'css-mode-hook 'custom/css-mode-hook)

;;; --- SASS & SCSS
(use-package sass-mode
  :commands sass-mode
  :config
  (defun custom/sass-mode-hook ()
    (setq-default rainbow-html-colors t)
    "Hook to customize sass mode."
    (rainbow-mode)
    (emmet-mode)
    (setq emmet-use-sass-syntax t))
  (add-hook 'sass-mode-hook 'custom/sass-mode-hook))

;;; --- Stylus
(use-package stylus-mode
  :commands stylus-mode
  :config
  (defun custom/stylus-mode-hook ()
    "Hook to customize stylus mode."
    (setq-default rainbow-html-colors t)
    (rainbow-mode)
    (emmet-mode)
    (setq emmet-use-css-transform t)
    (setq stylus-indent-offset 2)
    (setq tab-width 2)
    (setq indent-tabs-mode nil))
  (add-hook 'stylus-mode-hook 'custom/stylus-mode-hook))

(provide 'css.el)
;;; css.el ends here
