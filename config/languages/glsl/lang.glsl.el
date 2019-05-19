;;; lang.glsl --- Setup GLSL shader language
;;; Commentary:

;;; Code:
(use-package glsl-mode
  :ensure t
  :commands python-mode
  :mode ("\\.glsl\\'" . glsl-mode)
  :config
  (add-hook 'glsl-mode-hook
            (lambda()
              (defvar c-basic-offset 2)
              (setq tab-width 2))))

(use-package company-glsl
  :commands glsl-mode
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends '(company-glsl :with company-yasnippet))))

(use-package flycheck-glsl
  :disabled)

(use-package shaderlab-mode
  :disabled
  :straight (shaderlab-mode
             :type hg
             :host bitbucket
             :repo "bbbscarter/emacs-shaderlab-mode")
  :ensure t)

(provide 'lang.glsl)
;;; lang.glsl ends here
