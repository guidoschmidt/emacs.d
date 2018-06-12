;;; transparency --- toggle transparency of Emacs
;;; Commentary:

;;; Code:
(defcustom transparency 80
  "Current alpha of the frame appearance transparency."
  :type 'integer :group 'appearance)

(defun set-transparent (alpha)
  "Set frame ALPHA and store as transparency custom."
  (customize-save-variable 'transparency alpha)
  (set-frame-parameter nil 'alpha `(,(- 100 transparency) . 50)))

(defun toggle-transparency ()
  "Toggle transparency of a frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         `(,(- 100 transparency) . 50) '(100 . 100)))))

(defhydra hydra-transparency (:color black)
  "Transparency
"
  ("0" (set-transparent 0))
  ("1" (set-transparent 10))
  ("2" (set-transparent 20))
  ("3" (set-transparent 30))
  ("4" (set-transparent 40))
  ("5" (set-transparent 50))
  ("6" (set-transparent 60))
  ("7" (set-transparent 70))
  ("8" (set-transparent 80))
  ("9" (set-transparent 90))
  ("+" (set-transparent 100)))
(evil-leader/set-key
  "1" 'hydra-transparency/body)

(provide 'transparency)
;;; transparency ends here
