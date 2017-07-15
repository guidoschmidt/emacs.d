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
  :ensure t
  :defer t
  :config
  (defun custom/sass-mode-hook ()
    (setq-default rainbow-html-colors t)
    "Hook to customize sass mode."
    (rainbow-mode)
    (emmet-mode)
    (setq-default emmet-use-sass-syntax t))
  (add-hook 'sass-mode-hook 'custom/sass-mode-hook))

;;; --- Stylus
(use-package stylus-mode
  :ensure t
  :defer t
  :config
  (defun custom/stylus-mode-hook ()
    "Hook to customize stylus mode."
    (setq-default rainbow-html-colors t)
    (rainbow-mode)
    (emmet-mode)
    (setq-default emmet-use-css-transform t))
  (add-hook 'stylus-mode-hook 'custom/stylus-mode-hook))

(provide 'css.el)
;;; css.el ends here
