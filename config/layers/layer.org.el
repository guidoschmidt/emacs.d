;;; layer.org --- Setup and configure org-mode

;;; Commentary:

;;; Code:

;; Utility functions
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
         (filext (or filext "org$\\\|org_archive"))
         (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (add-to-list 'org-file-list org-file)))))))

(defun do-org-show-all-inline-images ()
  "Show all images inside an org file."
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)

(defun org-current-date ()
  "Insert the current date."
  (interactive
   (insert (format-time-string "%d.%m. — %A"))))

;; Setup org with org-files, variable configuration and keywords
(defvar org-directory "~/Dropbox/Notes")

(defvar org-agenda-files '("~/Dropbox/Notes/TODO.org"))

(defvar org-agenda-text-search-extra-files)
(setq org-agenda-text-search-extra-files
      (append (sa-find-org-file-recursively "~/Dropbox/Notes/" "org")
              (sa-find-org-file-recursively "~/Dropbox/Notes/" "org")))

(defvar org-startup-indented)
(defvar org-bullets-bullet-list)
(defvar org-ellipsis)
(defvar org-pretty-entities)
(defvar org-hide-emphasis-markers)
(defvar org-agenda-block-separator)
(defvar org-src-fontify-natively)
(defvar org-fontify-whole-heading-line)
(defvar org-fontify-done-headline)
(defvar org-fontify-quote-and-verse-blocks)

(setq org-startup-indented t
      org-bullets-bullet-list '(" ")
      org-ellipsis "  "
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator ""
      org-src-fontify-natively t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(defvar org-image-actual-width)
(setq org-image-actual-width '(400))

(defvar org-todo-keywords)
(setq org-todo-keywords
      '((sequence "IN(i)"
                  "TODO(t)"
                  "WIP(w)"
                  "SET(s)"
                  "PROJ(p)"
                  "BLOCKED(b)"
                  "|"
                  "DONE(d)"
                  "WONTDO(n)")))

(defvar org-todo-keyword-faces)
(setq org-todo-keyword-faces
      '(("IN"      . (:foreground "#AFBDBF" :weight bold))
        ("TODO"    . (:foreground "#FE6264" :weight bold :box nil))
        ("WIP"     . (:foreground "#FFB204" :weight bold))
        ("SET"     . (:foreground "#00D0F3" :weight bold))
        ("PROJ"    . (:foreground "#0050F3" :weight bold))
        ("BLOCKED" . (:foreground "#FE042B" :weight bold :box nil))
        ("DONE"    . (:foreground "#12DA73" :weight bold))
        ("WONTDO"  . (:foreground "#12DA73" :weight bold  :strike-through t))))

;; LaTeX configuration
(defvar org-format-latex-options '())

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 3.0))

;; Third party packages
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-pretty-table
  :straight (org-pretty-table
             :type git
             :host github
             :repo "Fuco1/org-pretty-table"))

(use-package writeroom-mode
  :ensure t
  :hook (org-mode . writeroom-mode)
  :config
  (setq writeroom-extra-line-spacing (round (/ (alfontzo-scale-for-host) 1.5)))
  (setq writeroom-fullscreen-effect nil)
  (setq writeroom-bottom-divider-width 0)
  (setq writeroom-width 0.8))

;; Hydra
(eval-after-load "evil-leader"
  '(progn
     (defhydra hydra-org (:color blue :hint nil)
       "
Org Mode

_j_: org-cycle-level ->     _o_: org-sort
_k_: org-cycle <-           _i_: insert current date
_t_: org-todo
"
       ("j" org-cycle-level)
       ("k" org-cycle)
       ("t" org-todo)
       ("o" org-sort)
       ("i" org-current-date))
     (evil-leader/set-key
       "z" 'hydra-org/body)))

(provide 'layer.org)
;;; layer.org ends here
