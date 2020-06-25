;;; lang.python --- Python scripting language support
;;; Commentary:

;;; Code:
(use-package python
  :ensure t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-indent-guess-indent-offset nil)
  ;;
  ;; macOS: Macbook Pro Vreni
  ;;
  (when (and (equalp (system-name) "Vreni")
             (memq window-system '(ns)))
    (setq python-shell-interpreter "~/.pyenv/versions/3.6.5/bin/python3")
    (setenv "PYTHONPATH"
            "/Applications/Rhinoceros.app/Contents/Resources/ManagedPlugIns/RhinoDLR_Python.rhp/RssLib"))
  ;;
  ;; Windows: Cube
  ;;
  (when (and (equalp (system-name) "Cube")
             (memq window-system '(w32)))
    (setq python-shell-interpreter "c:/Development/python/3.6/python.exe")
    (setq exec-path
          (append exec-path '("c:/Development/python/3.6")))
    (setenv "PYTHONPATH"
            ""))
  ;;
  ;; Windows: FMP Zenbook
  ;;
  (when (and (equalp (system-name) "Zenbook-GS")
             (memq window-system '(w32)))
    (setq python-shell-interpreter "c:/Development/anaconda/python.exe")
    (setq exec-path
          (append exec-path '("c:/Development/anaconda")))
    (setenv "PYTHONPATH"
            "")))

(use-package elpy
  :ensure t
  :commands elpy-mode
  :disabled
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
  :disabled
  :config
  (setq jedi:complete-on-dot t)
  (defun company/python-mode-hook ()
    (push 'company-jedi company-backends))
  (add-hook 'python-mode-hook 'company/python-mode-hook))

(use-package flycheck-pycheckers
  :ensure t
  :disabled
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))

(provide 'lang.python)
;;; lang.python ends here
