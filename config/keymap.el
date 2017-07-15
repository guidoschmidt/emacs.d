;;; keymap.el --- Configure custom key bindings
;;; Commentary:

;;; Code:
;; macOS key binding setup
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

;; Disable macOS minfication
(global-set-key (kbd "C-z") nil)

;; Custom key bindings
(global-set-key (kbd "C-+")         'text-scale-increase)
(global-set-key (kbd "C--")         'text-scale-decrease)
(global-set-key (kbd "C-;")         'comment-or-uncomment-region)
(global-set-key (kbd "C-M-f")       'toggle-frame-fullscreen)
(global-set-key (kbd "C-c C-k")     'compile)
(global-set-key (kbd "C-d")         'duplicate-line)
(global-set-key (kbd "C-x g")       'goto-line)
(global-set-key (kbd "M-<up>")      'move-line-up)
(global-set-key (kbd "M-p")         'move-line-up)
(global-set-key (kbd "M-<down>")    'move-line-down)
(global-set-key (kbd "M-n")         'move-line-down)
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-c s")       'flyspell-popup-correct)
(global-set-key (kbd "C-;")         'comment-or-uncomment-region)
(global-set-key (kbd "C-c x")       'insert-checkmark)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-x n")       'new-buffer)
(global-set-key (kbd "C-c t")       'neotree-toggle)

(provide 'keymap.el)
;;; keymap.el ends here
