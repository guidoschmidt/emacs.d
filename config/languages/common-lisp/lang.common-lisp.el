;;; lang.common-lisp --- setup common lisp
;;; Commentary:

;;; Code:
(defvar slime-complete-symbol*-fancy)
(defvar slime-complete-symbol-function)

(use-package slime-company
  :commands slime-mode
  :ensure t)

(use-package slime
  :commands slime-mode
  :init
  (setq slime-contribs '(slime-fancy
                         slime-company
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :config
  (slime-setup))

(provide 'lang.common-lisp)
;;; lang.common-lisp ends here
