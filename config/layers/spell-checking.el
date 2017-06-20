;;; packages.el --- Spell checking via flyspell
;;; Commentary:

;;; Code:
;;; --- Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(use-package flyspell-correct
  :ensure t)
(use-package flyspell-popup
  :ensure t)

(provide 'spell-checking.el)
;;; spell-checking.el ends here
