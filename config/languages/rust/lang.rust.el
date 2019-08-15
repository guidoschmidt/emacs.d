;;; lang.rust.el --- Rust language mode setup

;;; Commentary:

;;; Code:
(use-package rust-mode
  :ensure t)

(use-package rustic
  :ensure t
  :hook
  ((rust-mode . rustic-mode)))

(provide 'lang.rust)
;;; lang.rust.el ends here
