;;; codecompletion.auto-complete --- Setup code completion using auto-complete

;;; Commentary:

;;; Code:

(use-package fuzzy
  :ensure t
  :disabled)

(use-package auto-complete
  :ensure t
  :disabled
  :init
  (setq ac-auto-start 0
        ac-delay 0.2
        ac-quick-help-delay 0.5
        ac-use-fuzzy t
        ac-fuzzy-enable t
        tab-always-indent 'complete
        ac-dwim t)
  :config
  (progn
    (ac-config-default)
    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))))

(provide 'layer.codecompletion.auto-complete)
;;; layer.codecompletion.auto-complete ends here
