;;; lang.arduino --- Setup arduino features
;;; Commentary:

;;; Code:
(use-package platformio-mode
  :ensure t
  :commands (arduino-mode platformio-mode)
  :config
  (eval-when-compile
    (defun platformio-custom-hook ()
     (irony-mode)
     (irony-eldoc)
     (platformio-conditionally-enable))))

(provide 'lang.arduino)
;;; lang.arduino ends here
