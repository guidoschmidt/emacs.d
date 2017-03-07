;;; init.el --- Main entry for Emacs configuration


;;; Commentary:
;; - Remove startup screen
;; - Setup Emacs melpa packages
;; - Install "use-package"


;;; Code:

;; Disable tool-bar
(tool-bar-mode -1)

;;; Remove startup message
(setq inhibit-startup-message t)

;;; -- Use ibuffer instead of list-buffer
(defalias 'list-buffers 'ibuffer)

;;; -- Enable ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; Enable winner mode
(winner-mode 1)


;;; -------- 3rd Party Packages

;;; --- Setup melpa packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; --- Bootstrap "use-package"
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

;;; --- Try packages without installing them
(use-package try
  :ensure t)

;;; --- Setup which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; --- Setup org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; --- Setup ace-window
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)))

(provide 'init.el)
;;; init.el ends here

;;; --- Automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org-bullets which-key use-package try))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )