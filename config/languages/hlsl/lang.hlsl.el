;;; lang.hlsl --- HLSL shader language support

;;; Commentary:

;;; Code:
(use-package hlsl-mode
  :mode ("\\.hlsl\\'"
         "\\.compute\\'")
  :straight (hlsl-mode
             :type git
             :host github
             :repo "darfink/hlsl-mode"))

(provide 'lang.hlsl)
;;; lang.hlsl ends here
