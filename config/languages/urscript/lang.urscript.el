;;; lang.urscript.el --- Support for URScript
;;; Commentary:

;;; Code:
(use-package urscript-mode
  :straight (urscript-mode
             :type git
             :host github
             :repo "guidoschmidt/urscript-mode")
  :mode ("\\.urscript\\'" . urscript-mode))
  
(provide 'lang.urscript)
;;; lang.urscript.el ends here
