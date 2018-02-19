;;; layer.notifications --- Send notifications via terminal-notifier

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

(defun notify-in (time msg)
  "Notify in TIME minutes with a MSG text."
  (interactive)
  (run-at-time time nil
               (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg))

(defvar timer-minutes nil)
(defun make-timer (time)
  "Make a timer that will run for TIME minutes."
  (interactive (list
                (read-from-minibuffer
                 "Minutes: " (car timer-minutes)
                 nil nil 'timer-minutes)))
  (notify-in (concat time " minutes") (concat time " minutes are over!")))

(provide 'layer.notifications)
;;; layer.notifications ends here
