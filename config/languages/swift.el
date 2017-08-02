;;; swift.el --- Setup Swift programming language

;;; Commentary:

;;; Code:
(use-package swift-mode
  :ensure t
  :defer t)

(use-package flycheck-swift
  :ensure t
  :defer t
  :config
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package company-sourcekit
  :ensure t
  :config
  (defun company/swift-mode-hook ()
    (add-to-list 'company-backends 'company-sourcekit))
  (add-hook 'swift-mode-hook 'company/swift-mode-hook))

(provide 'swift.el)
;;; swift.el ends here
