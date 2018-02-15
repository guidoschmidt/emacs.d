;;; swift.el --- Setup Swift programming language

;;; Commentary:

;;; Code:
(use-package swift-mode
  :ensure)

(use-package flycheck-swift3
  :commands swift-mode
  :ensure
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))
  (setq flycheck-swift-sdk-path
        "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS10.0.sdk")
  ;; ↑ Select the appropriate SDK version you use
  (setq flycheck-swift-target "arm64-apple-ios10"))

(use-package company-sourcekit
  :commands swift-mode
  :ensure
  :config
  (defun company/swift-mode-hook ()
    (add-to-list 'company-backends '(company-dabbrev :with company-sourcekit))
    ;; (push 'company-sourcekit company-backends)
    (setq company-sourcekit-use-yasnippet t)
    (setq company-sourcekit-verbose t))
  (add-hook 'swift-mode-hook 'company/swift-mode-hook))

(use-package xcode-mode :ensure t)

(provide 'swift.el)
;;; swift.el ends here
