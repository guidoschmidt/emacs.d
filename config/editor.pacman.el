;;; editor.pacman.el --- Initialize package managing with MELPA -*- lexical-binding: t -*-

;;; Commentary:
;;; - Add package archives
;;; - Initialize packages
;;; - Bootstrap use-package

;;; Code:
(require 'package)
(package-initialize)

;; use-package -----------------------------------------------------------------
;;; https://github.com/melpa/melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

;;; Bootstrap "use-package"
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; straight.el -----------------------------------------------------------------
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; integrate with use-package
(straight-use-package 'use-package)

;; Additional helpers ----------------------------------------------------------
(use-package auto-compile
  :ensure
  :config
  (setq load-prefer-newer t)
  (require 'auto-compile)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'editor.pacman)
;;; editor.pacman ends here
