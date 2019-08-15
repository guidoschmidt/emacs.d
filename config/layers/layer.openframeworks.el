;;; layer.openframeworks --- Openframeworks convenience functions
;;; Commentary:

;;; Code:
(defun ofCompile ()
  "Compile your openframeworks project from the root (make -j)."
  (interactive)
  (let ((root (projectile-project-root)))
    (let ((cmd (concat "cd " root " && make -j")))
      (compile cmd))))

(defun ofRun ()
  "Run your openframeworks project (make RunRelease)."
  (interactive)
  (let ((root (projectile-project-root)))
    (let ((cmd (concat "cd " root " && make RunRelease")))
      (compile cmd))))

(provide 'layer.openframeworks)
;;; layer.openframeworks ends here
