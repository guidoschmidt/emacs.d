;;; layer.tramp --- Tramp layer for Windows
;;; Commentary:

;;; Code:
(require 'tramp)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

(provide 'layer.tramp)
;;; layer.tramp ends here
