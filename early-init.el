;;; early-init.el --- Main entry for Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(when (not (string-equal system-type "windows-nt"))
  (add-to-list 'default-frame-alist '(undecorated . t)))

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init.el)
;;; early-init.el ends here
