;;; swift.el --- Setup Swift programming language

;;; Commentary:

;;; Code:
(use-package swift-mode
  :commands swift-mode)

(use-package flycheck-swift
  :commands swift-mode
  :config
  (eval-after-load 'flycheck '(flycheck-swift-setup))
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS10.0.sdk")
  ;; ↑ Select the appropriate SDK version you use
  (setq flycheck-swift-target "arm64-apple-ios10"))

(use-package company-sourcekit
  :commands swift-mode
  :config
  (defun company/swift-mode-hook ()
    (add-to-list 'company-backends '(company-dabbrev :with company-sourcekit))
    ;; (push 'company-sourcekit company-backends)
    (setq company-sourcekit-use-yasnippet t)
    (setq company-sourcekit-verbose t))
  (add-hook 'swift-mode-hook 'company/swift-mode-hook))

(provide 'swift.el)
;;; swift.el ends here
