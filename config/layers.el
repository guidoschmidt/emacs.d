;;; layers.el --- Setup all kind of extra Emacs layers

;;; Commentary:

;;; Code:

(setq load-path (cons "~/.emacs.d/config/layers/" load-path))
(require 'autocomplete.company)
(require 'git.el)
(require 'orgmode.el)
(require 'shells.el)
(require 'spell-checking.el)
(require 'syntax-checking.el)
(require 'notifications.el)
(require 'evil.config)

(provide 'layers)
;;; layers.el ends here
