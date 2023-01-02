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
  :bind ( ("<C-return>" . follow-dendron-link)
          ("<C-tab>"    . switch-to-previous-buffer))
  :mode (("\\.md\\'"      . markdown-mode)
	       ("\\.mdx\\'"     . markdown-mode)
	       ("\\.makdown\\'" . markdown-mode)))

(defun follow-dendron-link ()
  "Follow a link that is placed between [[]] parantheses."
  (interactive)
  (let ((curline (thing-at-point 'line t)))
    (let ((start (string-match "\\[" curline))
          (end   (string-match "\\]" curline)))
      (find-file-existing (concat "./" (substring curline (+ start 2) end) ".md")))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Emmet abbreviation system
(use-package emmet-mode
  :straight t
  :config
  (setq-default emmet-expand-jsx-className? t)
  :hook
  ((web-mode     . emmet-mode)
   (html-mode    . emmet-mode)
   (makdown-mode . emmet-mode)
   (sass-mode    . emmet-mode)
   (scss-mode    . emmet-mode)
   (rjsx-mode    . emmet-mode)))

;; Encode emojis in markdown
(use-package emojify
  :straight t
  :hook (markdown-mode . emojify-mode))

(provide 'lang.markup)
;;; lang.markup.el ends here
