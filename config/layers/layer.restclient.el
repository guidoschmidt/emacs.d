;;; layer.restclient --- Setup rest client mode

;;; Commentary:
;;; restclient-mode provides Emacs
;;; interface for curl

;;; Code:
(use-package restclient
  :ensure t
  :commands (restclient-mode))

(use-package company-restclient
  :ensure t
  :init
   (defun company/restclient-mode-hook ()
     (push 'company-restclient company-backends))
   :hook (restclient-mode . company/restclient-mode-hook))

(provide 'layer.restclient)
;;; layer.restclient ends here
