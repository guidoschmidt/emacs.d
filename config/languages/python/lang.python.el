;;; Commentary:

;;; Code:
(use-package python
  :ensure t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  
  (setq python-indent-guess-indent-offset nil)
;;; macOS
  (when (memq window-system '(ns))
    (setq python-python-command "/usr/local/bin/python3")
    (setq exec-path
          (append exec-path
                  '("/usr/local/bin/"))))
;;; Windows
  (when (memq window-system '(w32))
    (setq python-python-command "c:/Development/python/3.6/python.exe")
    (setq exec-path
          (append exec-path '("c:/Development/python/3.5/")))))

(use-package elpy
  :ensure t
  :commands elpy-mode
  :config
  (setq elpy-rpc-python-command "python3")
  (defvar elpy-rpc-backend)
  (setq elpy-rpc-backend "jedi")
  (defun custom-python-mode-hook ()
    (setq python-indent-offset 2)
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (elpy-enable))
  (add-hook 'python-mode-hook 'custom-python-mode-hook))

(use-package company-jedi
  :ensure t
  :config
  (setq jedi:complete-on-dot t)
  (defun company/python-mode-hook ()
    (push 'company-jedi company-backends))
  (add-hook 'python-mode-hook 'company/python-mode-hook))

(use-package flycheck-pycheckers
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package importmagic
  :ensure t
  :diminish
  :hook (importmagic-mode . python-mode))

;; (defvar lsp-python)
;; (lsp-define-stdio-client lsp-python
;;                          "python"
;;                          #'projectile-project-root
;;                          '("~/.pyenv/versions/3.6.6/bin/pyls"))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (lsp-python-enable)))

;; (defun lsp-set-cfg ()
;;   "Setup language server configuration."
;;   (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))

;;     (lsp--set-configuration lsp-cfg)))
;; (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode . ("localhost:4500"))))

(provide 'lang.python)
;;; lang.python ends here
