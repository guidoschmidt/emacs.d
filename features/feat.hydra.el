;;; feat.hydra.el --- Hydra setup and definitions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Setup hydra package and specific hydras

;;; Code:
(use-package hydra
  :straight t)

(use-package major-mode-hydra
  :straight (major-mode-hydra :type git
		                          :host github
		                          :repo "jerrypnz/major-mode-hydra.el")
  :bind ("C-SPC" . major-mode-hydra))

;;; Hydras
(pretty-hydra-define hydra/window-management
  (:foreign-keys warn
                 :title "Window Management"
                 :quit-key "q")
  ("Actions"
   (("b" ivy-switch-buffer "buffer")
    ("a" ace-select-window "select")
    ("m" ace-delete-other-windows "maximize")
    ("x" ace-delete-window "delete")
    ("w" ace-swap-window "swap"))

   "Layout"
   (("s" split-window-below "horizontally")
    ("v" split-window-right "vertically")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "fullscreen"))

   "Zoom"
   (("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" text-scale-adjust "reset"))))

(pretty-hydra-define hydra/multiple-cursors
  (:foreign-keys warn
                 :title "Cursors"
                 :quit-key "q")
  ("Movement"
   (("j" evil-mc-make-cursor-move-next-line "next")
    ("k" evil-mc-make-cursor-move-prev-line "previous"))

   "Matches"
   (("n" evil-mc-skip-and-goto-next-match "skip/next")
    ("p" evil-mc-skip-and-goto-prev-match "skip/prev"))

   "Cursor"
   (("m" evil-mc-make-cursor-here "make cursor")
    ("c" evil-mc-undo-all-cursors "clear all cursors"))

   "iedit"
   (("i" iedit-dwim "iedit")
    ("x" iedit--quit "quit"))))

(pretty-hydra-define hydra/lsp-ui
  (:foreign-keys warn
                 :title "LSP UI"
                 :quit-key "q")
  ("Peek"
   (("d" lsp-ui-peek-find-definitions "find definitions")
    ("x" lsp-ui-peek--goto-xref "go to")
    ("r" lsp-ui-peek-find-references "peek references")
    ("?" lsp-ui-doc-glance "peek documentation"))

   "Movement"
   (("h" lsp-ui-peek-jump-backward "←")
    ("l" lsp-ui-peek-jump-forward "→"))))

(provide 'feat.hydra)
;;; feat.hydra.el ends here
