;;; feat.assistant.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:
(use-package ellama
  :straight t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	        (make-llm-ollama
	         :chat-model "llama3.1"
	         :embedding-model "nomic-embed-text"
	         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))

(use-package gptel
  :straight t
  :config
  (setq
   gptel-model 'llama3.1:latest
   gptel-backend (gptel-make-ollama "ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(llama3.1:latest))))

(use-package shell-maker
  :straight (:type git
             :host github
             :repo "xenodium/shell-maker"
             :files ("shell-maker*.el")))

(use-package chatgpt-shell
  :straight (:type git
             :host github
             :repo "xenodium/chatgpt-shell"
             :files ("chatgpt-shell*.el"))
  :config
  (setq chatgpt-shell-anthropic-key
        (auth-source-pick-first-password :host "api.anthropic.com"))
  (setq claude-shell-model-version "claude-3-7-sonnet-latest"))

(provide 'feat.assistant)

;;; feat.assistant.el ends here
