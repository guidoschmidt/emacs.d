;;; feat.networking.el --- Network tools -*- lexical-binding: t; -*-

;;; Commentary:
;;; Tools to improve networking

;;; Code:
(use-package restclient
  :straight t
  :mode "\\.rest\\'"
  :commands (restclient-mode))

(use-package company-restclient
  :straight t
  :init
   (defun company/restclient-mode-hook ()
     (push 'company-restclient company-backends))
   :hook (restclient-mode . company/restclient-mode-hook))

(provide 'feat.networking)
;;; feat.networking.el ends here
