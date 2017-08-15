;;; autocomplete.auto-complete.el --- Setup auto completion

;;; Commentary:
;;; Setup company mode

(use-package fuzzy
  :ensure t)

(use-package auto-complete
  :ensure t
  :init
  (setq ac-auto-start 0
        ac-delay 0.2
        ac-quick-help-delay 1.
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

(provide 'autocomplete.auto-complete.el)
;;; autocomplete.auto-complete.el ends here
