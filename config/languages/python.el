;;; python.el --- Python Language/IDE settings
;;; Commentary:

;;; Code:
;;; --- Python
(use-package elpy
  :ensure
  :commands python-mode
  :config
  (when (memq window-system '(w32))
    (setq exec-path (append exec-path '("C:/Development/python3/"))))
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (defun custom-python-mode-hook ()
    (setq python-indent-offset 4)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)
    (elpy-enable))
  (add-hook 'python-mode-hook 'custom-python-mode-hook))

(use-package company-jedi
  :ensure
  :commands python-mode
  :config
  (when (memq window-system '(w32))
    (setq exec-path (append exec-path '("c:/Development/python3"))))
 (defun company/python-mode-hook ()
    (push 'company-jedi company-backends))
  (add-hook 'python-mode-hook 'company/python-mode-hook))

(provide 'python.el)
;;; python.el ends here
