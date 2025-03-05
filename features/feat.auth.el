;;; feat.auth.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Source:
;;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources

;;; Code:
(require 'auth-source)

(setq auth-source-debug t)
(setq epg-gpg-program "/opt/homebrew/bin/gpg")
(setq auth-sources '((:source "~/.emacs.d/secrets/.auth.gpg")))

(provide 'feat.auth)
;;; feat.auth.el ends here
