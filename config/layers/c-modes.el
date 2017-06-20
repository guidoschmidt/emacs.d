;;; packages.el --- Setup IDE like features for cc modes (C/C++)
;;; Commentary:

;;; Code:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; --- Define custom c++ sytle
;; (c-add-style "c++-style
;; 	     '("stroustrup"
;; 	       (indent-tabs-mode . nil)
;; 	       (c-basic-offset . 2)
;; 	       (c-offsets-alist . ((inline-open . 0)
;; 				   (brace-list-open . 0)
;; 				   (statement-case-open . +)))))

;; --- Activate custom c++ style
;; (defun my-c++-mode-hook ()
;;   (c-set-style "c++-style")
;;   (c-toggle-auto-hungry-state 0))
;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; --- Flycheck

;; Add Google C++ Style checker.
;; In default, syntax checked by Clang and Cppcheck.
;; (use-package google-c-style
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook 'google-set-c-style)
;;   (add-hook 'c++-mode-hook 'google-set-c-style)
;;   (add-hook 'c-mode-hook 'google-set-c-style))

;; (use-package flycheck-google-cpplint
;;   :ensure t
;;   :config
;;   (eval-after-load 'flycheck
;;     '(progn (flycheck-add-next-checker 'c/c++-cppcheck
;;                                        '(warning . c/c++-googlelint)))))

;; (eval-after-load 'flycheck
;;   '(progn (add-hook 'c++-mode-hook 'flycheck-mode)
;;           (add-hook 'c-mode-hook 'flycheck-mode)))

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

;; (use-package flycheck-irony
;;   :ensure t
;;   :config
;;   (eval-after-load 'flycheck
;;     '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


(provide 'cc-mode-layer.el)
;;; cc-mode-layer.el ends here
