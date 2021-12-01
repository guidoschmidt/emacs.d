;;; feat.git.el --- Git extension via Magit -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure magit for nice git integration

;;; Code:
(use-package magit
  :straight t
  :config
  (setq magit-diff-paint-whitespace t)
  (setq magit-completion-read-function 'ivy-completion-read))

(use-package blamer
  :straight (blamer
             :type git
             :host github
             :repo "Artawower/blamer.el")
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))

(provide 'feat.git)
;;; feat.git.el ends here
