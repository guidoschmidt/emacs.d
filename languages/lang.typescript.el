;;; lang.typescript.el --- Typescript configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration of Typescript and tooling

;;; Code:
(defun emmet/ts-mode-hook ()
  "Enable Emmet in typescript files."
  (setq-default emmet-expand-jsx-className? t)
  (emmet-mode))

(use-package tsi
  :straight (tsi
            :type git
            :host github
            :repo "orzechowskid/tsi.el"))

(use-package tsx-mode
  :after tsi
  :straight (tsx-mode
             :type git
             :host github
             :repo "orzechowskid/tsx-mode.el"))

(use-package typescript-mode
  :straight t
  :config
  (setq-default typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'emmet/ts-mode-hook)
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package tide
  :straight t
  :after
  (typescript-mode company flycheck)
  :config
  (require 'web-mode)
  (setq company-tooltip-align-annotations t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (defun typescript-company-mode-hook ()
    (setq-local company-backends
                '((company-tide :with company-dabbrev company-yasnippet))))
  (add-hook 'typescript-mode #'typescript-company-mode-hook)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (typescript-mode))))
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . flycheck-mode)
   (typescript-mode . eldoc-mode)
   (typescript-mode . company-mode)
   (typescript-mode . tide-hl-identifier-mode)))

(use-package web-mode
  :straight t)

(provide 'lang.typescript)
;;; lang.typescript.el ends here
