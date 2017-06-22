;;; packages.el --- Setup IDE like features for cc modes (C/C++)
;;; Commentary:

;;; Code:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; --- CMake
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

;;; --- Rtags
(use-package rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t))

(use-package company-rtags
  :ensure t
  :config
  (progn (eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
   (setq rtags-autostart-diagnostics t)
   (rtags-enable-standard-keybindings)))

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
  :config
  (custom-set-variables
   '(irony-additional-clang-options
     '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun custom-irony-mode-hook ()
    (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))
  (add-hook 'irony-mode-hook 'custom-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package company-irony-c-headers
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-irony-c-headers company-irony))))

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
