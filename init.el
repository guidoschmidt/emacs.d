;;; init.el --- Main entry for Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Load configuration files from .emacs.d/config/

;; A big contributor to startup times is garbage collection.  We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'.  Not resetting it will cause stuttering/freezes.

;;; Code:
(setq gc-cons-threshold most-positive-fixnum)

;; Load custom functions
(add-to-list 'load-path "~/.emacs.d/core")
(require 'core.functions)
(require 'core.utils)
(require 'core.configuration)
(require 'core.straight)

(add-to-list 'load-path "~/.emacs.d/features")
(require 'feat.evil)
(require 'feat.functions)
(require 'feat.editor)
(require 'feat.themeing)
(require 'feat.modeline)
(require 'feat.syntaxchecking)
(require 'feat.completion)
(require 'feat.snippets)
(require 'feat.git)
(require 'feat.hydra)
(require 'feat.itautomation)
(require 'feat.livepreview)

(add-to-list 'load-path "~/.emacs.d/languages")
(require 'lang.cc)
(require 'lang.csharp)
(require 'lang.clojure)
(require 'lang.graphql)
(require 'lang.javascript)
(require 'lang.lua)
(require 'lang.markup)
(require 'lang.python)
(require 'lang.rust)
(require 'lang.shader)
(require 'lang.stylesheets)
(require 'lang.graphviz)

;; Reset garbage collection. Not doing so will cause garbage
;; collection freezes during long-term interactive use. Conversely, a
;; gc-cons-threshold that is too small will cause stuttering. We use 16mb as our
;; default.
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 33554432 ; 32mb
          gc-cons-percentage 0.1)))

(provide 'init.el)
;;; init.el ends here
