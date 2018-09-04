;;; layer.lsp.el ‒ Language server protocol client

;;; Commentary:

;;; Code:
(use-package lsp-mode
  :ensure t
  :hook (lsp-after-open . lsp-enable-imenu)
  :config
  ;; Get lsp-python-enable defined
  ;; use either projectile-project-root or ffip-get-project-root-directory
  ;; or any other function that can be used to find the root directory of
  ;; a project
  (setenv "PYTHONPATH" "c:/Users/gs/Desktop/Touchdesigner/Autocompletion/export/")
  (lsp-define-stdio-client lsp-python
                           "python"
                           #'projectile-project-root
                           '("c:/Development/python/anaconda/envs/td/Scripts/pyls"))
  ;; Activate generated lsp macro with `python-mode'.
  ;; `lsp-python-enable' is created by `lsp-define-stdio-client' above.
  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-python-enable)))
  ;; NB: only required if you prefer flake8 instead of the default
  (defun lsp-set-cfg ()
    (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))

      (lsp--set-configuration lsp-cfg)))
  (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

(use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
    :config
    (push 'company-lsp company-backends))

(provide 'layer.lsp)
;;; layer.lsp ends here
