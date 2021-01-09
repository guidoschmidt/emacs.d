;;; core.utils.el --- Custom functions for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Utilities and convenience functions used to customize Emacs.

;;; Code:
(defun hostname? (host)
  "Check the HOST of the current system."
  (string-equal (system-name) host))

(defun windows? ()
  "Check the current system beeing a Windows based one."
  (string-equal system-type "windows-nt"))

(defun macOS? ()
  "Check the current system beeing a macOS based one."
  (string-equal system-type "darwin"))

(defun linux? ()
  "Check the current system beeing a macOS based one."
  (string-equal system-type "gnu/linux"))

(provide 'core.utils)
;;; core.utils.el ends here
