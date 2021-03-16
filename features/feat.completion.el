;;; feat.completion.el --- Code completion using company + lsp -*- lexical-binding: t; -*-

;;; Commentary:
;;; Company with lsp for auto-completion

;;; Code:
;;; -------------------------------------------------------------------------------------------
;;; COMPANY
;;; -------------------------------------------------------------------------------------------
(use-package company
	:straight t
  :diminish company-mode
  :preface
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend (member 'company-yasnippet backend)))
            backend
            (append (if (consp backend) backend (list backend))
                    '(:with company-yasnippet)))))
	:config
  (setq-default company-dabbrev-other-buffers t
                company-dabbrev-code-time-limit 0.1
                company-idle-delay 0
                company-minimum-prefix-length 1
                company-require-match nil
                company-dabbrev-downcase nil
                company-dabbrev-ignore-case nil
                company-tooltip-align-annotations t
                company-tooltip-limit 15
                company-show-numbers t
                company-transformers '(company-sort-by-occurrence))
	(global-company-mode t)
  :bind
  (("<alt-tab>" . company-complete-common)
   ("<C-tab>"   . company-yasnippet)))

(use-package company-box
  :straight t
  :after company
  :hook
  (company-mode . company-box-mode)
  :config
  (setq company-box--height 400))

(use-package company-quickhelp
  :straight t
  :if window-system
  :config
  (setq pos-tip-background-color "#121212")
  (setq pos-tip-foreground-color "#f3f3f3")
  (company-quickhelp-mode)
  :bind
  (("C-h" . company-quickhelp-manual-begin)))

;;; -------------------------------------------------------------------------------------------
;;; LSP: LANGUAGE SERVER PROTOCOL
;;; -------------------------------------------------------------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :straight t
  :config 
  (setq lsp-session-file "~/.emacs.d/lsp/session"
        lsp-server-install-dir "~/.emacs.d/lsp/server/")
  (setq lsp-keep-workspace-alive nil)
  ;; Disable slow features
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected code modifications
  (setq lsp-enable-on-type-formatting nil)
  :hook
  (rjsx-mode . (lambda () (lsp))))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover nil))

(use-package lsp-ivy
  :straight t)

(use-package lsp-treemacs
  :straight t
  :config
  (lsp-treemacs-sync-mode t))

(provide 'feat.completion)
;;; feat.completion.el ends here
