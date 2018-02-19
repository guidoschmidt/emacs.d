;;; init.el --- Main entry for Emacs configuration

;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; Setup the load path
(let ((default-directory  "~/.emacs.d/config/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'editor.core)
(require 'editor.pacman)
(require 'editor.packages)
(require 'editor.keymap)
(require 'editor.appearance)
(require 'editor.layers)
(require 'editor.languages)

(setq custom-file "~/.emacs.d/local/custom-set.el")

(provide 'init.el)
;;; init.el ends here
