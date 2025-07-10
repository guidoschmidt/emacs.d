;;; lang.rust.el --- Rust language setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration and packages for the Rust language

;;; Code:
(use-package rustic
  :straight t
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-format-on-save t))

(provide 'lang.rust)
;;; lang.rust.el ends here
