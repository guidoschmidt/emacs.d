;;; cc.el --- Setup cc mode languages (C/C++)

;;; Commentary:
;;; Setup C/C++ features with
;;; - CTAGS & GTAGS
;;; - YouCompleteMe

;;; Code:

;;; --- Enable c++-mode for .h and .hpp files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;;; --- CMake
;; (use-package cmake-ide
;;   :ensure
;;   :defer
;;   :config
;;   (cmake-ide-setup))

;;; --- Modern C++11 font lock/syntax highlighting
(use-package modern-cpp-font-lock
  :commands c++-mode
  :config
  (modern-c++-font-lock-global-mode t))

;; --- clang-format
(use-package clang-format
  ;; Create clang-format file using google style
  ;; clang-format -style=google -dump-config > .clang-format
  :commands c++-mode
  :bind
  (("C-c C-f" . clang-format-region)))

;; -- Add Google C++ style
(use-package google-c-style
  :commands c++-mode
  :defer
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c-mode-hook 'google-set-c-style))

;; -- Add Google C++ style checker - In default, syntax checked by clang
(use-package flycheck-google-cpplint
  :load-path "config/layers/"
  :commands c++-mode
  :config
  (eval-after-load 'flycheck
    '(progn (flycheck-add-next-checker 'c/c++-clang
                                       '(warning . c/c++-googlelint)))))

;; -- Autocomplete using YouCompleteMe
;; (use-package ycmd
;;   :commands c++-mode
;;   :init
;;   (add-hook 'c++-mode-hook 'ycmd-mode)
;;   :config
;;   (set-variable 'ycmd-global-config
;;                 "/Users/gs/.emacs.d/github/ycm_extra_conf.py")
;;   (set-variable 'ycmd-server-command
;;                 '("python" "/Users/gs/.emacs.d/github/ycmd/ycmd"))
;;   (setq ycmd-force-semantic-completion t))

;; (use-package company-ycmd
;;   :commands c++-mode
;;   :config (company-ycmd-setup))

;; (use-package flycheck-ycmd
;;   :commands c++-mode
;;   :config (flycheck-ycmd-setup))

;; -- Autocomplete using Irony
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
(add-hook 'objc-mode-hook ' cc/irony-mode-hook))

(use-package flycheck-irony
  :ensure
  :defer
  :config
  (eval-after-load 'flycheck
'(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;; TODO: move to it's code-navigation package
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

;;; -- Counsel with etags
(use-package counsel-etags
  :ensure
  :config
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :bind
  (("C-1" . counsel-etags-find-tag-at-point)
   ("C-0" . xref-pop-marker-stack)
   ("C-2" . counsel-etags-grep-symbol-at-point)
   ("C-3" . counsel-etags-find-tag)))

(provide 'cc.el)
;;; cc.el ends here
