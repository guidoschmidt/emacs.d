;;; php.el --- Setup PHP language
;;; Commentary:

;;; Code:
(use-package php-mode
  :ensure t
  :defer t
  :config
  (defun custom-php-mode-hook ()
    (setq indent-tabs-mode nil
          tab-width 2
          c-basic-offset 2))
  (add-hook 'php-mode-hook 'custom-php-mode-hook))

(provide 'php.el)
;;; php.el ends here
