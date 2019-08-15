;;; layer.codedocs --- Code documentation using zeal

;;; Commentary:
;;; Install Zeal

;;; macOS:
;;; https://github.com/zealdocs/zeal/issues/995
;;;
;; brew tap markwu/personal
;; brew install qt
;; brew install qt5-webkit
;; brew install --HEAD zeal

;;; Code:
(use-package zeal-at-point
  :ensure t
  :after evil-leader
  :config
  (evil-leader/set-key "." 'zeal-at-point))

(provide 'layer.codedocs)
;;; layer.codedocs ends here
