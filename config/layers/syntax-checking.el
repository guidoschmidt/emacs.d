;;; packages.el --- Syntax checking via flycheck
;;; Commentary:

;;; Code:
;; --- Flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (setq-default flycheck-temp-prefix ".flycheck")
    (setq-default flycheck-disabled-checkers '(javascript-jshint))
    (setq-default flycheck-disabled-checkers '(json-jsonlint))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'vue-mode)))

(use-package flycheck-package
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-package-setup))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  :config
  (setq pos-tip-foreground-color '((:foreground "blue")))
  (setq pos-tip-background-color '((:background "red")))
  (pos-tip-show "ALERT"))

;; Custom fringe indicator
(define-fringe-bitmap 'my-flycheck-fringe-indicator
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00011100
          #b00111110
          #b00111110
          #b00111110
          #b00011100
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000))

(let ((bitmap 'my-flycheck-fringe-indicator))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap bitmap
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap bitmap
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap bitmap
    :fringe-face 'flycheck-fringe-info))


;; Enable flycheck globally
(global-flycheck-mode 1)

;;; --- Language styles &
;; Custom c++ sytle
;; (c-add-style "c++-style
;; 	     '("stroustrup"
;; 	       (indent-tabs-mode . nil)
;; 	       (c-basic-offset . 2)
;; 	       (c-offsets-alist . ((inline-open . 0)
;; 				   (brace-list-open . 0)
;; 				   (statement-case-open . +)))))

;; Activate custom c++ style
;; (defun my-c++-mode-hook ()
;;   (c-set-style "c++-style")
;;   (c-toggle-auto-hungry-state 0))
;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)


;; Add Google C++ style
(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c-mode-hook 'google-set-c-style))

;; Add Google C++ style checker - In default, syntax checked by clang
(use-package flycheck-google-cpplint
  :load-path "config/layers/"
  :config
  (eval-after-load 'flycheck
    '(progn (flycheck-add-next-checker 'c/c++-clang
                                       '(warning . c/c++-googlelint)))))

(provide 'syntax-checking.el)
;;; syntax-checking.el ends here
