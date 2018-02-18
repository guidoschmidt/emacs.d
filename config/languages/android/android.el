;;; android.el --- Setup android specific modes and packages
;;; Commentary:

;;; Code:
(use-package gradle-mode
  :ensure t
  :mode "\\.gradle\\"
  :commands gradle-mode)

(provide 'android.el)
;;; android.el ends here


