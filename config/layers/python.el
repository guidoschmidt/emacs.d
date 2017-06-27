;;; python.el --- Python Language/IDE settings
;;; Commentary:


;;; Code:
;;; --- Python
(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "ipython")
  (defun custom-python-mode-hook ()
    (setq python-indent-offset 4)
    (setq tab-width 4)
    (setq indent-tabs-mode nil))
  (add-hook 'python-mode-hook 'custom-python-mode-hook)
  (elpy-enable))

(use-package company-jedi
  :ensure t
  :config
  (defun company/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'company/python-mode-hook))

(provide 'python.el)
;;; python.el ends here
