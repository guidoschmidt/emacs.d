;;; lang.swift --- Setup Swift programming language

;;; Commentary:

;;; Code:
(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package flycheck-swift3
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup)
  (setq flycheck-swift-sdk-path
        "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS11.3.sdk")
  ;; ↑ Select the appropriate SDK version you use
  (setq flycheck-swift-target "arm64-apple-ios10"))

(use-package company-sourcekit
  :ensure t
  :config
  (defun company/swift-mode-hook ()
    (add-to-list 'company-backends '(company-dabbrev :with company-sourcekit))
    ;; (push 'company-sourcekit company-backends)
    (setq company-sourcekit-use-yasnippet t)
    (setq company-sourcekit-verbose t))
  (add-hook 'swift-mode-hook 'company/swift-mode-hook))

(provide 'lang.swift)
;;; lang.swift ends here
