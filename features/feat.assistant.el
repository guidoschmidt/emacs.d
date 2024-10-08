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

(provide 'feat.assistant)

;;; feat.assistant.el ends here
