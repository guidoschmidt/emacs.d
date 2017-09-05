;;; package --- Summary
;;; Commentary:


;;; Code:
(defcustom daytime-theme 'hemera
  "Theme to use during the day."
  :type 'string
  :group 'circadian)

(defcustom nighttime-theme 'nyx
  "Theme to use during the night."
  :type 'string
  :group 'circadian)

(defcustom night-starts-hour 19
  "Start night at this hour."
  :type 'integer
  :group 'circadian)

(defcustom day-starts-hour 7
  "Start day at this hour."
  :type 'integer
  :group 'circadian)

;; ---
(defun circadian-nighttime? ()
  "Many."
  (let ((current-hour (nth 2 (decode-time (date-to-time (current-time-string))))))
    (or (>= current-hour night-starts-hour)
        (<= current-hour day-starts-hour))))

(defun load-theme-if-needed (theme)
  "Load the THEME when it is not already loaded."
  (when (not (find theme custom-enabled-themes))
    (load-theme theme t)))

;; before loading new theme
(defun load-theme--disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

;; After loading new theme
(defun load-theme--restore-line-numbering(theme &rest args)
  "Set linum-format again after loading any theme."
  (setq linum-format 'linum-format-func))
(advice-add 'load-theme :after #'load-theme--restore-line-numbering)

;; Custom hook for determining the day time theme
(defun daytime-theme-hook ()
  "Endin."
  (if (circadian-nighttime?)
      (load-theme-if-needed nighttime-theme)
    (load-theme-if-needed daytime-theme)))

(add-hook 'after-change-major-mode-hook 'daytime-theme-hook)

;; ---
;; (setq nighttime-theme 'nyx)
;; (print nighttime-theme)
;; (setq daytime-theme 'hemera)
;; (print daytime-theme)

;; (print custom-enabled-themes)
;; (setq custom-enabled-themes '())

(provide 'circadian)
;;; circadian.el ends here
