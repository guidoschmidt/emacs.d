;;; rest.el --- Setup rest client mode

;;; Commentary:
;;; restclient-mode provides Emacs
;;; interface for curl

;;; Code:
(use-package restclient
  :ensure t
  :config
  (use-package company-restclient :ensure t)
  (defun company/restclient-mode-hook ()
    (push 'company-restclient company-backends))
  (add-hook 'restclient-mode-hook
            'company/restclient-mode-hook))

(provide 'rest.el)
;;; rest.el ends here
