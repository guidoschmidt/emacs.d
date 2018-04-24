;;; layer.codecompletion.company --- Setup code completion using company

;;; Commentary:
;;; Setup company mode

;;; Code:
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(use-package company
  :ensure t
  :diminish company-mode
  :preface
  (defun company-mode/backend-with-yas (backend)
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :config
  (setq-default company-dabbrev-other-buffers t
                company-dabbrev-code-time-limit 0.5
                company-idle-delay 0.15
                company-minimum-prefix-length 2
                company-require-match nil
                company-dabbrev-ignore-case nil
                company-dabbrev-downcase nil
                company-tooltip-limit 20
                company-show-numbers t
                company-transformers '(company-sort-by-occurrence))
  ;; Fix company popups when using fci
  ;; (fill column indicator) mode
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
  (setq company-backends (mapcar #'company-mode/backend-with-yas
                                 company-backends))
  (global-company-mode t)
  :bind
  (("<C-tab>" . company-complete-common)
   ("C-c <C-tab>" . company-yasnippet)))

(use-package company-quickhelp
  :ensure t
  :commands company-mode
  :if window-system
  :config
  (setq pos-tip-background-color "black")
  (setq pos-tip-foreground-color "white")
  (company-quickhelp-mode)
  :bind
  (("C-c h" . company-quickhelp-manual-begin)))

(use-package company-statistics
  :ensure
  :commands company-mode
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

(provide 'layer.codecompletion.company)
;;; layer.codecompletion.company ends here
