;;; timekeeper --- Account your working time
;;; Commentary:

;; Little helper functions for timekeeping with or-mode.

;;; Code:
(require 'org-timer)

(defun sec (args)
  "Get seconds of ARGS."
  (org-timer-hms-to-secs args))

(defun hms (args)
  "Get seconds of ARGS."
  (org-timer-secs-to-hms args))

(provide 'timekeeper)
;;; timekeeper.el ends here
