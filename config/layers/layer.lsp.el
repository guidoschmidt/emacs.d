;;; layer.lsp --- Language server protocol client
;;; Commentary:

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure t
  :hook (prog-mode . lsp)
  :config
  (lsp--persist-session (lsp-session)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (defhydra hydra-lsp-peek (:color black)
    "
LSP UI
--------------------------------------------------------------------------------

_d_: find definitions
_x_: go to
_r_: find references

_h_: ← go backward
_l_: → go forward

"
  ("d" lsp-ui-peek-find-definitions "find-definitions")
  ("x" lsp-ui-peek--goto-xref        "go to")
  ("h" lsp-ui-peek-jump-backward     "<")
  ("l" lsp-ui-peek-jump-forward      ">")
  ("r" lsp-ui-peek-find-references   "references"))
  (evil-leader/set-key
    "u" 'hydra-lsp-peek/body))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

;; @TODO
(use-package dap-mode
  :ensure t)

(provide 'layer.lsp)
;;; layer.lsp ends here
