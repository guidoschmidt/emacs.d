;;; lang.python --- Python Language/IDE settings

;;; Commentary:

;;; Code:
(use-package python
  :ensure t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-guess-indent-offset nil)
;;; macOS
  (when (memq window-system '(ns))
    (setq exec-path
          (append exec-path
                  '("/usr/local/bin/"))))
;;; Windows
  (when (memq window-system '(w32))
    (setq exec-path
          (append exec-path '("c:/Development/python/anaconda/envs/td/python.exe")))))

(use-package elpy
  :ensure t
  :commands elpy-mode
  :config
  (setq elpy-rpc-python-command "python3")
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

(provide 'lang.python)
;;; lang.python ends here
