;;; packages.el --- Autocompletion via company-mode
;;; Commentary:

;;; Code:
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-jedi            :ensure t :defer t)
  (use-package company-irony           :ensure t :defer t)
  (use-package company-irony-c-headers :ensure t :defer t)
  (use-package company-gtags           :ensure t :defer t)
  (use-package company-tern            :ensure t :defer t)
  (use-package company-restcliennt     :ensure t :defer t)
  (setq company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20
        company-dabbrev-downcase nil
        company-dabbrev-other-buffers t))
  ;;; Add backends
  (eval-after-load 'company
    (add-to-list 'company-backends (company-irony
                                    company-gtags
                                    company-tern
                                    company-jedi
                                    company-restclient)))
  :bind
  (("<C-tab>" . company-complete-common)))

(provide 'auto-completion.el)
;;; auto-completion.el ends here
