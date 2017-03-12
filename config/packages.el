;;; --- Try packages without installing them
(use-package try
  :ensure t)

;;; --- Setup which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; --- Flycheck
(use-package flycheck-pos-tip
  :ensure t)
(use-package flycheck
  :ensure t
  :init
  (progn
    (global-flycheck-mode)
    (setq-default flycheck-temp-prefix ".flycheck")
    (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
    (setq-default flycheck-disabled-checkers '(javascript-jshint))
    (setq-default flycheck-disabled-checkers '(json-jsonlint))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'vue-mode)))

;;; --- Yasnippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;;; --- Powerline & Spaceline
(use-package powerline
  :ensure t)
(use-package spaceline
  :ensure t
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'utf-8)
    (setq powerline-height 20)
    (spaceline-spacemacs-theme)))

;;; --- Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; --- Undo-tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;;; --- PHP mode
(use-package php-mode
  :ensure t)

;;; --- Elm mode
(use-package elm-mode
  :ensure t
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook
            (lambda ()
              (add-to-list 'company-backends '(company-elm))))
  (custom-set-variables '(elm-format-on-save t)))

;;; --- Tern
(use-package tern
  :ensure t
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

;;; --- Setup org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; --- Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;;; --- Setup ace-window
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)))

;;; --- Emmet
(use-package emmet-mode
  :ensure t)
(add-hook 'sass-mode-hook  'emmet-mode)

;;; --- Rainbow mode
(use-package rainbow-mode
  :ensure t)

;;; --- SASS/SCSS
(use-package sass-mode
  :ensure t)
(defun init-sass-mode-hook()
  (setq rainbow-html-colors t)
  (rainbow-mode))
(add-hook 'sass-mode-hook 'init-sass-mode-hook)

;;; --- Ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

;;; --- Swiper - better isearch
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;;; --- Avy
(use-package avy
  :ensure t
  :bind (("C-c a" . avy-goto-char)
         ("C-c o" . avy-goto-char-timer)))

;;; --- Auto-complete via company
(use-package company-jedi
  :ensure t)
(use-package company-tern
  :ensure t)
(use-package company
  :ensure t
  :init
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers t)
  (add-hook 'after-init-hook 'global-company-mode))
