;;; glsl.el --- Setup GLSL shader language
;;; Commentary:

;;; Code:
(use-package glsl-mode
  :ensure t
  :config
  (add-hook 'glsl-mode-hook
            (lambda()
              (defvar c-basic-offset 1)
              (setq tab-width 2))))

(provide 'glsl.el)
;;; glsl.el ends here
