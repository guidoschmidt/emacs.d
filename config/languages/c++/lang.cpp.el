;;; lang.cpp -- Setup cc mode languages (C/C++)

;;; Commentary:

;;; Setup C/C++ features with
;;; - CTAGS & GTAGS
;;; - YouCompleteMe/Irony for Autocompletion

;;; Code:
(require 'cl)
(require 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

;;; modern-cpp-font-lock - Modern C++11 font lock/syntax highlighting
(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package clang-format
  ;; Create clang-format file using google style
  ;; clang-format -style=google -dump-config > .clang-format
  :ensure t
  :bind
  (("C-c C-f" . clang-format-region)))

(use-package google-c-style
  :ensure t
  :hook
  (c-mode-common-hook . google-set-c-style)
  (c++-mode-hook      . google-set-c-style)
  (c-mode-hook        . google-set-c-style))

(use-package flycheck-google-cpplint
  :load-path "~/.emacs.d/config/languages/c++/"
  :config
  (eval-after-load 'flycheck
    '(progn (flycheck-add-next-checker 'c/c++-clang
                                       '(warning . c/c++-googlelint)))))

;; -- Autocomplete using YouCompleteMe
;; (use-package ycmd
;;   :disabled
;;   :ensure t
;;   :commands c++-mode
;;   :init (add-hook 'c++-mode-hook #'ycmd-mode)
;;   :config
;;   (set-variable 'ycmd-server-command '("python2" "~/.emacs.d/github/ycmd/ycmd"))
;;   (set-variable 'ycmd-global-config (expand-file-name "~/.emacs.d/config/external/ycm_extra_conf.py"))
;;   (set-variable 'ycmd-extra-conf-whitelist '("~/Repos/*"))
;;   (use-package company-ycmd
;;     :ensure t
;;     :mode ("\\.cpp\\'" "\\.h\\'" "\\.hpp\\'")
;;     :commands c++-mode
;;     :init (company-ycmd-setup)
;;     :config (add-to-list 'company-backends
;;                          (company-mode/backend-with-yas 'company-ycmd))))

;; (use-package flycheck-ycmd
;;   :disabled
;;   :commands (flycheck-ycmd-setup)
;;   :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

;; -- Autocomplete using Irony
(use-package company-irony :ensure t)
(use-package company-irony-c-headers :ensure t)
(use-package irony-eldoc :ensure t)
(use-package irony
  :ensure t
  :config
  (defun company/irony-mode-hook ()
    "Hook to customize irony mode."
    (add-to-list 'company-backends '(company-irony :with company-irony-c-headers)))
  (add-hook 'irony-mode-hook 'company/irony-mode-hook)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; (setq company-backends
  ;;       (remove-if (lambda (e)
  ;;                    (equal 'company-clang (car e)))
  ;;                  company-backends))
  :bind
  ((:map c-mode-map
         ("<C-tab>" . company-irony))))

;; -- Flycheck
(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; -- GGTags
(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :diminish ggtags-mode
  :config
  (defun ggtags/c-mode-hook ()
    "Hook to setup GGTAGS in cc-modes."
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
      (ggtags-mode 1)))
  (add-hook 'c-mode-common-hook 'ggtags/c-mode-hook))
;; TODO: add Hydra for ggtags
;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;; -- RTags
(use-package rtags
  :ensure t
  :config
  (add-to-list 'company-backends '(company-rtags :with company-yasnippet)))

(print company-backends)

(use-package company-rtags
  :ensure t
  :config
  (defhydra hydra-rtag (:color blue :hint nil)
    "
RTags

_j_: goto symbol under point
"
    ("j" rtags-find-symbol-at-point))
  (evil-leader/set-key
    "r" 'hydra-rtag/body))

;; -- Counsel etags
(use-package counsel-etags
  :ensure t
  :config
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; Setup auto update now
  :hook (prog-mode . (lambda ()
                       (add-hook 'after-save-hook
                                 'counsel-etags-virtual-update-tags
                                 'append
                                 'local)))
  :bind
  (("C-1" . counsel-etags-find-tag-at-point)
   ("C-0" . xref-pop-marker-stack)
   ("C-2" . counsel-etags-grep-symbol-at-point)
   ("C-3" . counsel-etags-find-tag)))

(use-package bazel-mode
  :ensure t
  :init
  (defun bazel-format-on-save ()
    (add-hook 'before-save-hook #'bazel-format nil t))
  :hook (bazel-mode . 'bazel-format-on-save))

(defun cpp-endline ()
  "Insert semicolon and move point/cursor to the next line."
  (interactive)
  (insert ";")
  (next-line)
  (move-beginning-of-line 1))

(define-key c++-mode-map (kbd "<C-return>") 'cpp-endline)

(provide 'lang.cpp)
;;; lang.cpp ends here
