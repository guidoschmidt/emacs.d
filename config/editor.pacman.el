;;; editor.pacman.el --- Initialize package managing with MELPA

;;; Commentary:
;;; - Add package archives
;;; - Initialize packages
;;; - Bootstrap use-package

;;; Code:
(require 'package)

;;; See: https://github.com/melpa/melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;; Bootstrap "use-package"
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Install system dependencies automatically
(use-package use-package-ensure-system-package
  :ensure t)

(use-package auto-compile
  :ensure
  :config
  (setq load-prefer-newer t)
  (package-initialize)
  (require 'auto-compile)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'editor.pacman)
;;; editor.pacman ends here