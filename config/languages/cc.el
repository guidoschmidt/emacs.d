;;; cc.el --- Setup cc mode languages (C/C++)

;;; Commentary:
;;; Setup C/C++ features with
;;; - CMake
;;; - CTAGS & GTAGS
;;; - Irony

;;; Code:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;;; --- CMake
(use-package cmake-ide
  :ensure
  :defer
  :config
  (cmake-ide-setup))

;;; --- ctags & ggtags
(use-package ggtags
  :ensure
  :commands ggtags-mode
  :diminish ggtags-mode
  :config
  (defun ggtags/c-mode-hook ()
    "Hook to setup GGTAGS in cc-modes."
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
      (ggtags-mode 1)))
  (add-hook 'c-mode-common-hook 'ggtags/c-mode-hook))

(use-package flycheck-irony
  :ensure
  :defer
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;; --- Irony - C++ IDE
(use-package irony
  :ensure
  :defer
  :config
  (use-package company-irony :ensure t)
  (use-package company-irony-c-headers :ensure t)
  (use-package irony-eldoc :ensure t)
  (custom-set-variables
   '(irony-additional-clang-options
     '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
  (defun custom/irony-mode-hook ()
    "Hook to customize irony mode."
    (add-to-list 'company-backends 'company-irony)
    (add-to-list 'company-backends 'company-irony-c-headers)
    (add-to-list 'company-backends 'company-dabbrev)
    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony))
  (add-hook 'irony-mode-hook 'custom/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (defun cc/irony-mode-hook ()
    (unless (glsl-mode)
      (irony-mode)
      (add-hook 'irony-mode-hook #'irony-eldoc)))
  (add-hook  'c-mode-hook '    cc/irony-mode-hook)
  (add-hook  'c++-mode-hook '  cc/irony-mode-hook)
  (add-hook  'objc-mode-hook ' cc/irony-mode-hook))

;; --- astyle
(defun astyle-this-buffer (pmin pmax)
  "Astyle a buffer region from PMIN to PMAX."
  (interactive "r")
  (shell-command-on-region
   pmin pmax
   "astyle --style=google -s2"
   (current-buffer) t
   (get-buffer-create "*Astyle Errors*") t))

;; Add Google C++ style
(use-package google-c-style
  :ensure t
  :defer
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c-mode-hook 'google-set-c-style))

;; Add Google C++ style checker - In default, syntax checked by clang
(use-package flycheck-google-cpplint
  :load-path "config/layers/"
  :defer
  :config
  (eval-after-load 'flycheck
    '(progn (flycheck-add-next-checker 'c/c++-clang
                                       '(warning . c/c++-googlelint)))))

(provide 'cc.el)
;;; cc.el ends here
