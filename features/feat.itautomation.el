;;; feat.itautomation.el --- Docker setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Docker related tooling

;;; Code:
(use-package dockerfile-mode
  :straight (dockerfile-mode :type git
                             :host github
                             :repo "spotify/dockerfile-mode"))

(use-package ansible
  :straight t)

(provide 'feat.itautomation)
;;; feat.itautomation.el ends here
