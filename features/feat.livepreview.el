;;; feat.livepreview.el --- Live preview support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Live preview for markdown, html etc.

;;; Code:
(use-package impatient-mode
  :straight t
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer))))

(provide 'feat.livepreview)
;;; feat.livepreview.el ends here
