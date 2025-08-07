;;; feat.terminal.el --- Terminal tools for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Terminal tools for Emacs

;; commentary

;;; Code:

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package vterm
  :straight t)


(provide 'feat.terminal)
;;; feat.terminal.el ends here
