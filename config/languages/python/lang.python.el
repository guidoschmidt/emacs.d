;;; Commentary:

;;; Code:
(use-package python
  :ensure t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-indent-guess-indent-offset nil)

  ;;; macOS: Macbook Pro Vreni
  (when (and (equalp (system-name) "Vreni")
             (memq window-system '(ns)))
    (setq python-shell-interpreter "/Users/gs/.pyenv/versions/3.6.6/bin/python3")
    (setq exec-path
          (append exec-path
                  '("/usr/local/bin")))
    (setenv "PYTHONPATH"
            "/Applications/Rhinoceros.app/Contents/Resources/ManagedPlugIns/RhinoDLR_Python.rhp/RssLib"))
            "/usr/local/Cellar/opencv@3/3.4.5/lib/python3.7/site-packages"

  ;;; Windows: Cube
  (when (and (equalp (system-name) "Cube")
             (memq window-system '(w32)))
    (setq python-shell-interpreter "c:/Development/python/3.6/python.exe")
    (setq exec-path
          (append exec-path '("c:/Development/python/3.6")))
    (setenv "PYTHONPATH"
            ""))

  ;;; Windows: NSYNK
  (when (and (equalp (system-name) "WOLFGANG")
             (memq window-system '(w32)))
    (setq python-shell-interpreter "d:/python/python.exe")
    (setq exec-path
          (append exec-path '("d:/python/python")))
    (setenv "PYTHONPATH"
            "")))

(use-package elpy
  :ensure t
  :commands elpy-mode
  :config
  (setq elpy-rpc-python-command "python3")
  (defvar elpy-rpc-backend)
  (setq elpy-rpc-backend "jedi")
  (defun custom-python-mode-hook ()
    (setq python-indent-offset 4)
    (setq tab-width 4)
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

(lsp-register-client
 (make-lsp--client :new-connection (lsp-stdio-connection "pyls")
                   :major-modes '(python-mode)
                   :server-id 'pyls))
(add-hook 'python-mode-hook #'lsp)

(defun lsp-set-cfg ()
  "Setup language server configuration."
  (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))

    (lsp--set-configuration lsp-cfg)))
(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

(provide 'lang.python)
;;; lang.python ends here
