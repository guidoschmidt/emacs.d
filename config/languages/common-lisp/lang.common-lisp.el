;;; lang.common-lisp --- setup common lisp
;;; Commentary:

;;; Code:
(use-package slime-company
  :commands slime-mode
  :ensure)

(use-package slime
  :commands slime-mode
  :init
  (progn
    (load (expand-file-name "~/.roswell/helper.el"))
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch)
          inferior-lisp-program "ros -Q run")
    (push 'slime-company slime-contribs)
    (defun slime/disable-smartparens ()
      (smartparens-strict-mode -1)
      (turn-off-smartparens-mode))
    (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens)
    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    )
  :config
  (slime-setup))

(provide 'lang.common-lisp)
;;; lang.common-lisp ends here
