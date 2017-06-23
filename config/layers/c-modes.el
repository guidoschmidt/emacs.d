;;; packages.el --- Setup IDE like features for cc modes (C/C++)
;;; Commentary:

;;; Code:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; --- CMake
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

;;; --- ctags & ggtags
(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

;;; --- rtags
;; (use-package rtags
;;   :ensure t
;;   :config
;;   (setq rtags-completions-enabled t))

;; (use-package company-rtags
;;   :ensure t
;;   :config
;;   (progn (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;;    (setq rtags-autostart-diagnostics t)
;;    (rtags-enable-standard-keybindings)))

;; (use-package flycheck-rtags
;;   :ensure t
;;   :config
;;   (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))
;;   (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup))

;; (use-package flycheck-irony
;;   :ensure t
;;   :config
;;   (eval-after-load 'flycheck
;;     '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;; --- Irony - C++ dev mode
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (custom-set-variables
   '(irony-additional-clang-options
     '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
  (defun custom-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol]
      'counsel-irony))
  (add-hook 'irony-mode-hook 'custom-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; --- AStyle
(defun astyle-this-buffer (pmin pmax)
  "Astyle a buffer region from PMIN to PMAX."
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "astyle --style=google -s2"
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))

(provide 'c-modes.el)
;;; c-modes.el ends here
