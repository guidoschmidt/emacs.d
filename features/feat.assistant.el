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
	         :chat-model "mistral"
	         :embedding-model "nomic-embed-text"
	         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))


(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker"))

(use-package chatgpt-shell
  :straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
  :custom
  ((setq chatgpt-shell-anthropic-key
         (auth-source-pick-first-password :host "api.anthropic.com"))))

(provide 'feat.assistant)

;;; feat.assistant.el ends here
