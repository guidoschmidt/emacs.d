;;; feat.git.el --- Git extension via Magit -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure magit for nice git integration

;;; Code:
(use-package magit
  :straight t
  :config
  (setq magit-diff-paint-whitespace t)
  (setq magit-completion-read-function 'ivy-completion-read))

(provide 'feat.git)
;;; feat.git.el ends here
