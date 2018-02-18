;;; init.el --- Main entry for Emacs configuration

;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(load "~/.emacs.d/config/core.el")
(load "~/.emacs.d/config/keymap.el")
(load "~/.emacs.d/config/pacman.el")
(load "~/.emacs.d/config/packages.el")
(load "~/.emacs.d/config/appearance.el")
(load "~/.emacs.d/config/layers.el")
(load "~/.emacs.d/config/languages.el")

(setq custom-file "~/.emacs.d/local/custom-set.el")

(provide 'init.el)
;;; init.el ends here
