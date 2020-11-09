;;; lang.rust.el --- Rust language mode setup

;;; Commentary:

;;; Code:
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :ensure t
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode))
  :config
  (setq racer-rust-src-path "/Users/gs/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

(use-package rustic
  :ensure t
  :disabled
  :hook
  ((rust-mode . rustic-mode)))

(provide 'lang.rust)
;;; lang.rust.el ends here
