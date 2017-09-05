;;; markup.el --- Setup markup languages
;;; Commentary:

;;; Code:
;;; --- Emmet
(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook  'web-mode-hook      'emmet-mode)
    (add-hook  'mustache-mode-hook 'emmet-mode)
    (add-hook  'html-mode-hook     'emmet-mode)
    (add-hook  'markdown-mode-hook 'emmet-mode)))

;;; --- HTML
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.mustache\\'" . web-mode))
  (defun custom-web-mode-hook ()
    (setq emmet-expand-jsx-className? f)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'custom-web-mode-hook))

(use-package html-check-frag
  :ensure t
  :config
  (add-hook 'html-mode-hook
            (lambda () (html-check-frag-mode 1))))

;;; --- Markdown
(use-package markdown-mode+
  :ensure t)

;;; --- Yaml
(use-package yaml-mode
  :ensure t)

(provide 'markup.el)
;;; markup.el ends here
