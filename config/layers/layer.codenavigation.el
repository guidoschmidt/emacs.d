;;; layer.codenavigation --- Setup tags and code navigation

;;; Commentary:

;;; Code:

(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode
                                    'c++-mode
                                    'java-mode
                                    'swift-mode)
                (ggtags-mode 1)))))

(provide 'layer.codenavigation)
;;; layer.codenavigation ends here
