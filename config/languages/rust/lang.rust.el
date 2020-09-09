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
         (racer-mode . eldoc-mode)))

(use-package rustic
  :ensure t
  :disabled
  :hook
  ((rust-mode . rustic-mode)))

(provide 'lang.rust)
;;; lang.rust.el ends here
