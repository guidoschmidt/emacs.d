;;; notifications.el --- Send notifications via terminal-notifier
;;; Commentary:

;;; Code:
(defvar terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier.")

(defun terminal-notifier-notify (title message)
  "Show a TITLE and a MESSAGE with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

(defun notify-in(time msg)
  (interactive
   "Notification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil
               (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg))

(provide 'notifications.el)
;;; notifications.el ends here
