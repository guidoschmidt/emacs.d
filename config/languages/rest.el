;;; rest.el --- Setup rest client mode

;;; Commentary:
;;; restclient-mode provides Emacs
;;; interface for curl

;;; Code:
(use-package restclient
  :ensure t
  :defer t
  :config
  (use-package company-restclient :ensure t :defer t)
  (defun company/restclient-mode-hook ()
    (add-to-list 'company-backends 'company-restclient))
  (add-hook 'restclient-mode-hook
            'company/restclient-mode-hook))

(provide 'rest.el)
;;; rest.el ends here
