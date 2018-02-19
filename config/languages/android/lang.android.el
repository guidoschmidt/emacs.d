;;; lang.android --- Setup android specific modes and packages
;;; Commentary:

;;; Code:
(use-package gradle-mode
  :ensure t
  :mode "\\.gradle\\'"
  :commands gradle-mode)

(provide 'lang.android)
;;; lang.android ends here


