;;; lang.markup --- Setup markup languages

;;; Commentary:

;;; Code:

;;; Emmet
(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook  'web-mode-hook      'emmet-mode)
    (add-hook  'mustache-mode-hook 'emmet-mode)
    (add-hook  'html-mode-hook     'emmet-mode)
    (add-hook  'markdown-mode-hook 'emmet-mode)))

;;; HTML
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (defun custom-web-mode-hook ()
    (setq-default web-mode-markup-indent-offset 2)
    (setq-default web-mode-css-indent-offset 2)
    (setq-default web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'custom-web-mode-hook)
  (add-to-list 'auto-mode-alist
               '("\\.mustache\\'" . web-mode)
               '("\\.hbs\\'" . web-mode)))

(use-package html-check-frag
  :ensure t
  :mode "\\.html\\'"
  :config
  :hook (html-mode . (lambda () (html-check-frag-mode 1))))

;;; Markdown
(use-package markdown-mode+
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;; YAML
(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure t)

(provide 'lang.markup)
;;; lang.markup ends here
