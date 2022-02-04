;;; lang.shader.el --- Shader languages configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; HLSL, GLSL + Unity shader configurations

;;; Code:
;; Shader mode
(use-package shader-mode
  :straight t
  :mode "\\.compute\\'")

;; GLSL language configuration
(use-package glsl-mode
  :straight t
  :mode "\\.glsl\\'"
  :config
  (add-hook 'glsl-mode
            (lambda ()
              (defvar c-basic-offset 2)
              (setq tab-width 2))))

(use-package company-glsl
  :straight (company-glsl
             :type git
             :host github
             :repo "guidoschmidt/company-glsl")
  :after company
  :config
  (when (executable-find "glslangValidator")
    (progn
      (defun glsl-company-mode-hook ()
       (setq-local company-backends
                   '((company-glsl :width company-dabbrev company-yasnippet))))
      (add-hook 'glsl-mode-hook #'glsl-company-mode-hook))))

(provide 'lang.shader)
;;; lang.shader.el ends here
