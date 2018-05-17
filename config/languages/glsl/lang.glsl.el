;;; lang.glsl --- Setup GLSL shader language
;;; Commentary:

;;; Code:
(use-package glsl-mode
  :ensure t
  :commands python-mode
  :mode ("\\.glsl\\'" . python-mode)
  :config
  (add-hook 'glsl-mode-hook
            (lambda()
              (defvar c-basic-offset 2)
              (setq tab-width 2))))

(use-package company-glsl
  :commands glsl-mode
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(use-package flycheck-glsl
  :commands glsl-mode
  :load-path "~/.emacs.d/github/flycheck-glsl")

(provide 'lang.glsl)
;;; lang.glsl ends here
