;;; lang.markup.el --- Markup languages support -*- lexical-binding: t; -*-

;;; Commentary:
;;; Setting up languages and tooling for markup languages like HTML, YAML, etc.

;;; Code:
;; YAML support
(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

;; Markdown
(use-package markdown-mode+
  :straight t
  :mode (("\\.md\\'"      . markdown-mode)
	       ("\\.mdx\\'"     . markdown-mode)
	       ("\\.makdown\\'" . markdown-mode)))

;; Emmet abbreviation system
(use-package emmet-mode
  :straight t
  :hook
  ((web-mode     . emmet-mode)
   (html-mode    . emmet-mode)
   (makdown-mode . emmet-mode)
   (sass-mode    . emmet-mode)
   (rjsx-mode    . emmet-mode)))

;; Encode emojis in markdown
(use-package emojify
  :straight t
  :hook (markdown-mode . emojify-mode))

(provide 'lang.markup)
;;; lang.markup.el ends here
