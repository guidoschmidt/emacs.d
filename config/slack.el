;;; slack.el --- Configure Emacs as slack client
;;; Commentary:

;;; Code:

(use-package slack
  :ensure
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t))

(provide 'slack.el)
;;; slack.el ends here
