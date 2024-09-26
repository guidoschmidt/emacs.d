;;; feat.completion.el --- Code completion using company + lsp -*- lexical-binding: t; -*-

;;; Commentary:
;;; Company with lsp for auto-completion

;;; Code:
;;; -------------------------------------------------------------------------------------------
;;; COMPANY
;;; -------------------------------------------------------------------------------------------
(use-package company
	:straight t
  :defer t
	:config
  (setq-default company-dabbrev-other-buffers t
                company-dabbrev-code-time-limit 0.1
                company-idle-delay 0.1
                company-minimum-prefix-length 1
                company-require-match nil
                company-dabbrev-downcase nil
                company-dabbrev-ignore-case nil
                company-tooltip-align-annotations t
                company-tooltip-limit 60
                company-show-numbers t
                company-transformers '(company-sort-by-occurrence))
	(global-company-mode))

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
  (setq lsp-lens-debounce-interval 0.005)
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
  (lsp-treemacs-sync-mode t)
  (setq treemacs-no-png-images t))


;;; -------------------------------------------------------------------------------------------
;;; DASH
;;; -------------------------------------------------------------------------------------------
(if (macOS?)
    (use-package dash-at-point
      :straight (dash-at-point
                 :type git
		             :host github
		             :repo "stanaka/dash-at-point")))


;;; -------------------------------------------------------------------------------------------
;;; CODEIUM
;;; -------------------------------------------------------------------------------------------
(use-package codeium
  :after company
  :straight (codeium
             :type git
             :host github
             :repo "Exafunction/codeium.el")
  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq use-dialog-box nil)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions
                      Heartbeat
                      CancelRequest
                      GetAuthToken
                      RegisterUser
                      auth-redirect
                      AcceptCompletion))))
  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(provide 'feat.completion)
;;; feat.completion.el ends here
