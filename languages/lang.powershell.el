;;; lang.powershell.el --- Powershell language config-*- lexical-binding: t; -*-

;;; Commentary:

;;; 

;;; Code:
(use-package powershell
  :straight (powershell
             :type git
             :host github
             :repo "jschaf/powershell.el"))

(provide 'lang.powershell)
;;; lang.powershell.el ends here
