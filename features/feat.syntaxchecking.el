;;; feat.syntaxchecking.el --- Syntax checking via flycheck -*- lexical-binding: t; -*-

;;; Commentary:
;;; Flycheck configuration for syntax checking

;;; Code:
(use-package flycheck
  :straight (flycheck
             :type git
             :host github
             :repo "Fuco1/flycheck"
             :branch "fix/stylecheck-syntax-arg")
  :diminish flycheck-mode
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq flycheck-emacs-lisp-load-path 'inherit)
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
  :hook
  ((rjsx-mode       . flycheck-mode)
   (c++-mode        . flycheck-mode)
   (emacs-lisp-mode . flycheck-mode)))

(use-package flycheck-pos-tip
  :straight t)

(provide 'feat.syntaxchecking)
;;; feat.syntaxchecking.el ends here

