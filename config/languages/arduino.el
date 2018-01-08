;;; arduino --- Setup arduino features
;;; Commentary:

;;; Code:
(use-package platformio-mode
  :commands arduino-mode
  :config
  (defun platformio-custom-hook ()
    (irony-mode)
    (irony-eldoc)
    (platformio-conditionally-enable)))

(provide 'arduino.el)
;;; arduino.el ends here
