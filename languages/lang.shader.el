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

(use-package wgsl-mode
  :straight (wgsl-mode
             :type git
             :host github
             :repo "acowley/wgsl-mode")
  :config
  ;; https://github.com/wgsl-analyzer/wgsl-analyzer
  (add-to-list 'lsp-language-id-configuration '(wgsl-mode . "wgsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "wgsl_analyzer")
                    :activation-fn (lsp-activate-on "wgsl")
                    :server-id 'wgsl_analyzer)))

(provide 'lang.shader)
;;; lang.shader.el ends here
