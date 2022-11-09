;;; feat.email.el --- Email setup for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; MU4E in Emacs

;;; Code:

(use-package mu4e
  :straight t
  :config
  (setq mu4e-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/.mails")
  (setq mu4e-update-interval 300)
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "gmail"
           :match-func (lambda (msg)
                         (when msg (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-refile-folder . "/gmail/[google_mail].archive")
                   (mu4e-drafts-folder . "/gmail/[google_mail].drafts")
                   (mu4e-sent-folder . "/gmail/[google_mail].sent")
                   (mu4e-trash-folder . "/gmail/[google_mail].bin")
                   )))))

(use-package mu4e-alert
  :straight t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread maildir:/gmail/[google_mail].all_mail"))
  (mu4e-alert-enable-mode-line-display))



(print mu4e-contexts)

(provide 'feat.email)
;;; feate.email.el ends here
