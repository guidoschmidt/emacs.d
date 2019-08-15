;;; layer.org --- Setup and configure org-mode

;;; Commentary:

;;; Code:
(require 'parse-time)
(require 'solar)
(require 'timekeeper)

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

(defun org-date-plus-x (offset)
  "Insert the current date plus OFFSET days."
  (interactive
   (list
    (read-from-minibuffer
     (concat "Day offset: ")
     nil
     nil
     nil
     nil)))
  (let ((today-day (nth 3 (decode-time)))
        (today-year (string-to-number (format-time-string "%Y")))
        (today-month (string-to-number (format-time-string "%m"))))
    (insert (concat (number-to-string today-year) "-"
                   (number-to-string today-month) "-"
                   (number-to-string today-day)
                   "T12:00:00-0:0"))))

(defun datetime-with-offset (offset)
  "Get the current date and time with day OFFSET."
  (let ((now (decode-time (current-time))))
    (setcar (nthcdr 3 now) (+ offset (nth 3 now)))
    (let ((secs  (nth 0 now))
          (mins  (nth 1 now))
          (hrs   (nth 2 now))
          (day   (nth 3 now))
          (month (nth 4 now))
          (year  (nth 5 now)))
      `(,secs ,mins ,hrs ,day ,month ,year 2 nil 3600))))

(defun org-insert-date-with-offset (offset)
  "Insert the current date with day OFFSET."
  (let ((datestring (format-time-string
                     "%d.%m. — %A"
                     (encode-time (datetime-with-offset offset)))))
    (insert (concat "** " datestring "  [/]"))
   (newline)))

(defun org-insert-date-for ()
  "Insert the current day with a given day offset from the mini buffer."
  (interactive
   (let ((offset (string-to-number (read-from-minibuffer "Day offset: "))))
     (org-insert-date-with-offset offset))))

(defun org-insert-week ()
  "Insert a whole week from today."
  (interactive
   (mapcar #'org-insert-date-with-offset '(0 1 2 3 4 5 6))))

;; Setup org with org-files, variable configuration and keywords
;;(defvar org-directory "~/Dropbox/Notes")

(defvar org-agenda-files '("~/Dropbox/Notes/TODO.org"
                           "~/Dropbox/Notes/PROJECTS.org"
                           "~/Dropbox/Notes/DAILY.org"
                           "~/Dropbox/Notes/ARCHIVE.org"
                           "~/Dropbox/Notes/INBOX.org"))

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
      `(:scale 1.2 :foreground ,(face-attribute 'default :foreground)))

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
  ;; (setq writeroom-extra-line-spacing (round (/ (alfontzo-scale-for-host) 1.5)))
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

(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path "~/Dropbox/org")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t))

(defun screenshot (&optional arg)
  "Take a screenshot with optional ARG and insert org link.
with prefix arg, minimize Emacs first.
Only works on macOS."
  (interactive "P")
  (let ((dirname "img"))
    (when arg
      (suspend-frame))
    (unless (f-directory? dirname)
      (make-directory dirname))
    (sit-for 0.2)
    (let ((fname (concat (format-time-string "date-%d-%m-%Y-time-%H-%M-%S" (current-time)) ".png")))
      (do-applescript
       (mapconcat
        'identity
        (list (format "set screenshotFilePath to \"%s\"" (expand-file-name fname dirname))
              "do shell script \"screencapture \" & \"-s\" & \" \" & quoted form of screenshotFilePath"
              (concat "set result to \"[[./" fname "]]\"")
              "set the clipboard to result")
        "\n"))
      (insert (format "\n\n#+attr_org: :width 300\n[[./%s]]\n\n" (concat dirname "/" fname)))
      (org-redisplay-inline-images)
      (raise-frame))))

(provide 'layer.org)
;;; layer.org ends here
