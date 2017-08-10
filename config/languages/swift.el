;;; swift.el --- Setup Swift programming language

;;; Commentary:

;;; Code:
(use-package swift-mode
  :ensure t)

(use-package flycheck-swift
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package company-sourcekit
  :ensure t
  :config
  (defun company/swift-mode-hook ()
    (push 'company-sourcekit company-backends)
    (setq company-sourcekit-use-yasnippet t)
    (setq company-sourcekit-verbose t))
  (add-hook 'swift-mode-hook 'company/swift-mode-hook))

(provide 'swift.el)
;;; swift.el ends here
