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
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(provide 'lang.shader)
;;; lang.shader.el ends here
