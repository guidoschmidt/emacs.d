;;; ligatures --- Configure font ligatures -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(let ((alist
       '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
         (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
         (36 . ".\\(?:>\\)")
         (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
         (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
         (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
         (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
         (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")

         ;; May produce: error in process filter: Attempt to shape unibyte text
         ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")

         (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
         (48 . ".\\(?:x[a-zA-Z]\\)")
         (58 . ".\\(?:::\\|[:=]\\)")
         (59 . ".\\(?:;;\\|;\\)")
         (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
         (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
         (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
         (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
         (91 . ".\\(?:]\\)")
         (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
         (94 . ".\\(?:=\\)")
         (119 . ".\\(?:ww\\)")
         (123 . ".\\(?:-\\)")
         (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
         (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Disable auto-composition-mode for ediff
(add-hook 'ediff-mode-hook
          (lambda () (setq-local auto-composition-mode nil)))

(add-hook 'c++-mode-hook
          (lambda () (setq-local auto-composition-mode nil)))

(add-hook 'org-mode-hook
          (lambda () (setq-local auto-composition-mode nil)))

(provide 'ligatures)
;;; ligatures ends here
