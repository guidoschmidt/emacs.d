;;; glsl.el --- Setup GLSL shader language
;;; Commentary:

;;; Code:
(use-package glsl-mode
  :ensure t
  :config
  (add-hook 'glsl-mode-hook
            (lambda()
              (irony--mode-exit)
              (defvar c-basic-offset 1)
              (setq tab-width 2))))

(use-package company-glsl
  :load-path "~/.emacs.d/github/company-glsl"
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(use-package flycheck-glsl
  :load-path "~/.emacs.d/github/flycheck-glsl")

(provide 'glsl.el)
;;; glsl.el ends here
