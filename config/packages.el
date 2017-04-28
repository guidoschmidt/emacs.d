;;; packages.el --- Manage MELPA packages
;;; Commentary:

;;; Code:
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

;; --- Icons
(use-package all-the-icons
  :ensure t)

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
  :after powerline
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

;;; --- Projectile
(use-package counsel-projectile
  :ensure t)
(use-package projectile
  :ensure t
  :config
  (projectile-mode))


;;; --- Undo-tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

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
  :ensure t
  :config
  (progn
    (add-hook 'sass-mode-hook  'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'stylus-mode-hook 'emmet-mode)
    (add-hook 'jsx-mode-hook 'emmet-mode)))

;;; --- Rainbow mode
(use-package rainbow-mode
  :ensure t)

;;; --- Git
(use-package git-commit
  :ensure t)
(use-package magit
  :ensure)

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
(use-package company-php
  :ensure t)
(use-package company-c-headers
  :ensure t)
(use-package company-jedi
  :ensure t)
(use-package company-tern
  :ensure t)
(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq-default company-dabbrev-downcase nil)
  (setq-default company-dabbrev-other-buffers t)
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (add-hook 'after-init-hook 'global-company-mode))

;;; --- Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)))

;;; --- Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(use-package flyspell-correct
  :ensure t)
(use-package flyspell-popup
  :ensure t)

;;; --- RESTclient
(use-package restclient
  :ensure t)

;;; --- FIC Mode
(use-package fic-mode
  :ensure t
  :config
  (require 'fic-mode)
  (add-hook 'c++-mode-hook 'fic-mode)
  (add-hook 'sass-mode-hook 'fic-mode)
  (add-hook 'jsx-mode-hook 'fic-mode))

;;; --- Language specific
;;; --- CC Modes
(setq c-default-style "stroustrup")

;;; --- C++ Mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-add-style "c++-style"
	     '("stroustrup"
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 2)
	       (c-offsets-alist . ((inline-open . 0)
				   (brace-list-open . 0)
				   (statement-case-open . +)))))
(defun my-c++-mode-hook ()
  (c-set-style "c++-style")
  (c-toggle-auto-hungry-state 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; --- GLSL
(use-package glsl-mode
  :ensure t
  :config
  (add-hook 'glsl-mode-hook
            (lambda()
              (setq c-basic-offset 1)
              (setq tab-width 2))))

;;; --- SASS/SCSS
(use-package sass-mode
  :ensure t
  :config
  (progn
    (add-hook 'sass-mode-hook
	      (lambda ()
		(setq rainbow-html-colors t)
		(rainbow-mode)))))

;; --- Haskell
(use-package haskell-mode
  :ensure t)

;;; --- CSS
(setq-default css-indent-offset 2)

;;; --- Elm
(use-package elm-mode
  :ensure t
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook
            (lambda ()
              (add-to-list 'company-backends '(company-elm))))
  (custom-set-variables '(elm-format-on-save t)))

;;; --- PHP
(use-package php-mode
  :ensure t)

;;; --- Javascript & JSX
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-hook 'jsx-mode-hook
    (lambda ()
      (setq emmet-expand-jsx-className? t)))

(setq-default jsx-indent-level 2)

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround `sgml-mode' and follow airbnb component style."
  (let* ((cur-line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (if (string-match "^\\( +\\)\/?> *$" cur-line)
      (let* ((empty-spaces (match-string 1 cur-line)))
        ;; (replace-regexp empty-spaces
        ;;                 (make-string (- (length empty-spaces) sgml-basic-offset) 32)
        ;;                 nil
        ;;                 (line-beginning-position) (line-end-position))))))
        ))))

;;; --- Python
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "ipython"))

(provide 'packages.el)
;;; packages.el ends here
