;;; packages.el --- Spell checking via flyspell
;;; Commentary:

;;; Code:
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(use-package flyspell
  :ensure t
  :config
  ;; Activate Flyspell by default in text & modes.
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  ;; Spellcheck comments in programming modes.
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; Prevent Flycheck from checking code blocks in org-mode.
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))
  ;; Prevent Flyspell from printing a log for every
  ;; checked word in a buffer to avoid performance impacts.
  (setq flyspell-issue-message-flag nil)
  ;; Define aspell as the spellchecking engine.
  (setq-default ispell-program-name "aspell")
  ;; Set the default language dictionary.
  (ispell-change-dictionary "en_US" t)
  ;; Correct word on mouse click
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [C-down-mouse-1] #'flyspell-correct-word)))
  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  :bind
  (("C-c w" . ispell-word)
   ("C-c p" . flyspell-check-previous-highlighted-word)
   ("C-c n" . flyspell-check-next-highlighted-word)))

(use-package flyspell-correct
  :ensure t)

(use-package flyspell-popup
  :ensure t)

(provide 'spell-checking.el)
;;; spell-checking.el ends here
