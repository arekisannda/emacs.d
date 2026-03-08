;;; tools/gpt.el -*- lexical-binding: t; -*-

(use-package gptel
  :defer t
  :preface
  (defun gptel-mode-setup ()
    (org-mode)
    (gptel-highlight-mode 1))
  :custom
  (gptel-default-mode #'gptel-mode-setup)
  (gptel-cache '(message system))
  (gptel-directives
   '((default   . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (assistant . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (writing   . "You are a large language model and a writing assistant. Respond concisely.")
     (chat      . "You are a large language model and a conversation partner. Respond concisely.")))
  (gptel-prompt-prefix-alist
   '((markdown-mode . "# ")
     (org-mode . "* ")
     (text-mode . "# ")))
  (gptel-response-prefix-alist
   '((markdown-mode . "")
     (org-mode . "")
     (text-mode . "")))
  :config

  (defvar gptel--anthropic
    (gptel-make-anthropic "Claude"
      :stream t
      :key 'gptel-api-key
      ))
  (defcustom gptel-backend gptel--openai
    "LLM backend to use.

    This is the default \"backend\", an object of type
    `gptel-backend' containing connection, authentication and model
    information.

    A backend for ChatGPT is pre-defined by gptel.  Backends for
    other LLM providers (local or remote) may be constructed using
    one of the available backend creation functions:
    - `gptel-make-openai'
    - `gptel-make-azure'
    - `gptel-make-ollama'
    - `gptel-make-gpt4all'
    - `gptel-make-gemini'
    See their documentation for more information and the package
    README for examples."
    :safe #'always
    :type `(choice
            (const :tag "ChatGPT" ,gptel--openai)
            (const :tag "Claude" ,gptel--anthropic)
            (restricted-sexp :match-alternatives (gptel-backend-p 'nil)
                             :tag "Other backend")))
  (setq gptel-model 'claude-haiku-4-5-20251001)
  (setq gptel-backend gptel--anthropic)
  )
