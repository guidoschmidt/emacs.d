;;; feat.auth.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Source:
;;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources

;;; Code:
(require 'auth-source)

;; (setenv "GPG_AGENT_INFO" nil)
(setq auth-source-debug t)
(setq-default epg-gpg-program "/opt/homebrew/bin/gpg")
(setq-default auth-sources '((:source "~/.emacs.d/secrets/authinfo.gpg")))

(provide 'feat.auth)
;;; feat.auth.el ends here
