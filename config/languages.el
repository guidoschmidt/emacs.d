;;; languages --- Load language specific configurations

;;; Commentary:

;;; Code:
(setq load-path (cons "~/.emacs.d/config/languages/arduino" load-path))
(require 'arduino)

;;; TODO: refactor
(load "~/.emacs.d/config/languages/c-c++/cc.el")
(load "~/.emacs.d/config/languages/clojure/clojure.el")
(load "~/.emacs.d/config/languages/common-lisp/common-lisp.el")
(load "~/.emacs.d/config/languages/css/css.el")
(load "~/.emacs.d/config/languages/emacs-lisp/elisp.el")
(load "~/.emacs.d/config/languages/glsl/glsl.el")
(load "~/.emacs.d/config/languages/haskell/haskell.el")
(load "~/.emacs.d/config/languages/javascript/javascript.el")
(load "~/.emacs.d/config/languages/kotlin/kotlin.el")
(load "~/.emacs.d/config/languages/markup/markup.el")
(load "~/.emacs.d/config/languages/php/php.el")
(load "~/.emacs.d/config/languages/python/python.el")
(load "~/.emacs.d/config/languages/swift/swift.el")

(provide 'languages)
;;; languages ends here
