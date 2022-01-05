;;; lang.haskell.el --- Haskell configuration -*- lexical-binding: t -*-

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:
(use-package haskell-mode
  :straight t)

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :hook (haskell-mode . dante-mode))

(use-package lsp-haskell
  :straight t
  :config
  (add-hook 'haskell-mode #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(provide 'lang.haskell)

;;; lang.haskell.el ends here
