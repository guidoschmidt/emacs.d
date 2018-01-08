;;; clisp.el --- setup common lisp
;;; Commentary:

;;; Code:
(use-package slime
  :commands common-lisp-mode
  :config
  (setq inferior-lisp-program "/usr/local/bin/clisp")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :commands common-lisp-mode
  :config
  (add-to-list 'company-backends 'slime-company))

(provide 'clisp.el)
;;; clisp.el ends here
