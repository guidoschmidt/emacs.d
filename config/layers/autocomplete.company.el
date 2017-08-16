;;; autocomplete.company.el --- Setup auto completion

;;; Commentary:
;;; Setup company mode

;;; Code:
(use-package company
  :ensure t
  :config
  (setq-default company-dabbrev-other-buffers t)
  (setq-default company-idle-delay 0.2)
  (setq-default company-minimum-prefix-length 1)
  (setq-default company-require-match nil)
  (setq-default company-dabbrev-ignore-case nil)
  (setq-default company-dabbrev-downcase nil)
  (setq-default company-tooltip-limit 20)
  (setq-default company-show-numbers t)
  ;;; Fix company popups when using fci
  ;;; (fill column indicator) mode
  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))
  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))
  (add-hook 'company-completion-started-hook
            'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook
            'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook
            'company-maybe-turn-on-fci)
  (global-company-mode)
  :bind
  ("<C-tab>" . company-complete-common))

(use-package company-quickhelp
  :ensure t
  :if window-system
  :config
  (eval-when-compile
    (eval-after-load 'company
      '(define-key company-active-map
         (kbd "C-c h")
         #'company-quickhelp-manual-begin)))
      (company-quickhelp-mode))


(provide 'autocomplete.company.el)
;;; autocomplete.company.el ends here
