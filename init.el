;;; init.el --- Main entry for Emacs configuration

;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;;; Code:
;; Avoid garbage collection during startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Avoid the pitfall of loading old bytecode indstead of newer
(setq load-prefer-newer t)

(when (memq system-type '(windows-nt ms-dos))
  (package-initialize))

(setq custom-file "~/.emacs.d/local/custom-set.el")

;; Setup the load path
(let ((default-directory  "~/.emacs.d/config/"))
  (normal-top-level-add-to-load-path
   '("."
     "/Volumes/Keybase (gs)/private/guidoschmidt/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Private
;; TODO: print warning when private.keys wasn't found
(require 'private.keys nil t)

;; Core
(require 'editor.core)
(require 'editor.pacman)
(require 'editor.keymap)

;; Layers
(require 'layer.codecompletion.company)
(require 'layer.codenavigation)
(require 'layer.git)
(require 'layer.evil)
(require 'layer.lsp)
(require 'layer.org)
(require 'layer.restclient)
(require 'layer.spellcheck)
(require 'layer.syntaxcheck)

;; Additional packages
(require 'editor.packages)

;; Appearance
(require 'editor.appearance)
(require 'modeline)
(require 'themes)
;;(require 'ligatures)

;; Languages
;; (require 'lang.android)
;; (require 'lang.arduino)
;; (require 'lang.clojure)
;; (require 'lang.common-lisp)
(require 'lang.cpp)
(require 'lang.csharp)
;; (require 'lang.css)
(require 'lang.emacs-lisp)
;; (require 'lang.fsharp)
;; (require 'lang.glsl)
;; (require 'lang.haskell)
;; (require 'lang.javascript)
;; (require 'lang.kotlin)
;; (require 'lang.markup)
;; (require 'lang.php)
;; (require 'lang.python)
;; (require 'lang.swift)
;; (require 'lang.urscript)

;; Then reset GC as late as possible
(defun reenable-gc ()
  "Re-set garbage collection variables."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'reenable-gc)


(provide 'init.el)
;;; init.el ends here
