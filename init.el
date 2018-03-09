;;; init.el --- Main entry for Emacs configuration

;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(setq custom-file "~/.emacs.d/local/custom-set.el")

; Setup the load path
(let ((default-directory  "~/.emacs.d/config/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Private 
(require 'private.keys nil t)

;; Core
(require 'editor.core)
(require 'editor.pacman)
(require 'editor.packages)
(require 'editor.keymap)

;; Appearance
(require 'editor.appearance)
(require 'ligatures)
(require 'modeline)
(require 'themes)

;; Layers
(require 'layer.codecompletion.auto-complete)
(require 'layer.codecompletion.company)
(require 'layer.codenavigation)
(require 'layer.evil)
(require 'layer.git)
(require 'layer.notifications)
(require 'layer.org)
(require 'layer.restclient)
(require 'layer.shell)
(require 'layer.spellcheck)
(require 'layer.syntaxcheck)

;; Languages
(require 'lang.android)
(require 'lang.arduino)
(require 'lang.cpp)
(require 'lang.clojure)
(require 'lang.common-lisp)
(require 'lang.css)
(require 'lang.emacs-lisp)
(require 'lang.glsl)
(require 'lang.haskell)
(require 'lang.javascript)
(require 'lang.kotlin)
(require 'lang.markup)
(require 'lang.php)
(require 'lang.python)
(require 'lang.swift)

(provide 'init.el)
;;; init.el ends here
