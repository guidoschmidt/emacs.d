;;; packages.el --- Autocompletion via company-mode
;;; Commentary:

;;; Code:
(use-package company
  :ensure t
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 1
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-tooltip-limit 20
          company-show-numbers t
          company-dabbrev-other-buffers t)
    ;;; Fix company popups when using fci (fill column indicator) mode
    (defvar-local company-fci-mode-on-p nil)
    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))
    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))
    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :config
  (progn
    ;;; Setup 3rd party backends
    (use-package company-irony           :ensure t)
    (use-package company-irony-c-headers :ensure t)
    (use-package company-tern            :ensure t)
    (use-package company-jedi            :ensure t)
    (use-package company-restclient      :ensure t)
    (use-package company-ghci            :ensure t)
    (eval-after-load 'company
      '(add-to-list 'company-backends '(company-gtags
                                        company-irony
                                        company-irony-c-headers
                                        company-tern
                                        company-jedi
                                        company-restclient
                                        company-ghci)))
    ;;; Enable company mode everywhere
    (global-company-mode))
  :bind
  (("<C-tab>" . company-complete-common)))


(use-package company-quickhelp
  :ensure t
  :config
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))


(provide 'auto-completion.el)
;;; auto-completion.el ends here
