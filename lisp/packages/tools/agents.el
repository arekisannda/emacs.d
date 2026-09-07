;;; tools/agents.el -*- lexical-binding: t; -*-

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
   '((default
      . "You are a large language model and a careful programmer.
Do not use markdown. Do not use triple backticks. Do not wrap code in any fencing or formatting.
Output only the raw code characters, nothing else — no preamble, no explanation, no backticks.")
     (assistant
      . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (writing
      . "You are a large language model and a writing assistant. Respond concisely.")
     (chat
      . "You are a large language model and a conversation partner. Respond concisely.")))
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

(use-package agent-shell
  :demand t
  :custom
  (agent-shell-dot-subdir-function
   (lambda (subdir)
     (let ((activity (activities-current)))
       (expand-file-name
        (file-name-concat "agent-shell"
                          (and activity (activities-name-for activity))
                          subdir)
        no-littering-var-directory))
     ))
  (agent-shell-mcp-servers nil)
  (agent-shell-header-style 'text)
  (agent-shell-show-welcome-message nil)
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication
    :oauth (lambda () (auth-source-pass-get 'secret "claude-code"))))
  (agent-shell-clipboard-image-handlers
   '(((:command . "wl-paste")
      (:save
       . (lambda (file-path)
           (with-temp-buffer
             (let* ((coding-system-for-read 'binary)
                    (exit-code (call-process "wl-paste" nil (list t nil) nil "--type" "image/png")))
               (if (zerop exit-code)
                   (write-region nil nil file-path)
                 (error "Command wl-paste failed with exit code %d" exit-code))))))))
   )
  :config
  (defun agent-shell-no-context ()
    (interactive)
    (let (agent-shell-context-sources)
      (call-interactively #'agent-shell)
      ))

  (defun agent-shell-mode-setup ()
    (face-remap-add-relative 'hl-line `(nil :background ,(doom-color 'bg-alt)))
    (face-remap-add-relative 'header-line `(nil :background ,(doom-color 'bg)))
    )
  :hook
  (agent-shell-mode . agent-shell-mode-setup))

(with-eval-after-load 'ghostel
  (defvar ghostel-project-claude--last nil)

  (define-derived-mode ghostel-claude-mode ghostel-mode "Claude")

  (defun ghostel-claude-focus-handler (window)
    (when (derived-mode-p 'ghostel-claude-mode)
      (with-selected-window window
        (setq ghostel-project-claude--last (window-buffer)))))

  (defun ghostel-project-claude--init (bufname)
    (let ((buffer (get-buffer-create bufname)))
      (with-current-buffer buffer
        (ghostel-claude-mode)
        (add-hook 'window-buffer-change-functions #'ghostel-claude-focus-handler nil t))

      (display-buffer buffer)
      (ghostel-exec buffer "claude")
      buffer))

  (defun ghostel-project-claude (&optional arg)
    (interactive "p")
    (let (target-directory)
      (pcase arg
        (4 (setq target-directory (and arg (funcall project-prompter))))
        (_ (setq target-directory (project-root (project-current t)))))

      (let* ((default-directory target-directory)
             (ghostel-buffer-name  (project-prefixed-buffer-name "claude"))
             (buffer (get-buffer ghostel-buffer-name)))
        (if (buffer-live-p buffer)
            (display-buffer buffer)
          (setq buffer (ghostel-project-claude--init ghostel-buffer-name)))
        (setq ghostel-project-claude--last buffer)
        buffer)))

  (defun ghostel-project-claude--file-context (filename &optional start end)
    (let (context)
      (if (use-region-p)
          (let ((line-start (line-number-at-pos start))
                (line-end (line-number-at-pos end)))
            (setq context
                  (cond
                   ((util/region-is-whole-line-p start end)
                    (format "@%s#L%d" filename line-start))
                   ((= line-start line-end)
                    (format "`%s`" (buffer-substring-no-properties start end)))
                   (t (format "@%s#L%d-%d" filename line-start line-end)))))
        (setq context (format "@%s" filename)))
      context))

  (defun ghostel-project-claude--non-file-context (&optional start end)
    (let (context)
      (if (use-region-p)
          (let ((line-start (line-number-at-pos start))
                (line-end (line-number-at-pos end)))
            (setq context
                  (format "`%s`" (buffer-substring-no-properties start end)))
            )
        (user-error "Unable all of non-file buffer as context. Select a region."))
      context))

  (defun ghostel-project-claude-with-context (&optional arg start end)
    (interactive
     (list (prefix-numeric-value current-prefix-arg)
           (when (use-region-p) (region-beginning))
           (when (use-region-p) (region-end))))

    (let* ((init-buffer (current-buffer))
           (claude-buffer (or (and
                               (buffer-live-p ghostel-project-claude--last)
                               ghostel-project-claude--last)
                              (ghostel-project-claude)))
           (force-string (and arg (< arg 0)))
           (arg (abs arg))
           filename
           context)

      (with-current-buffer init-buffer
        (cond
         ((and (not force-string) (setq filename buffer-file-name))
          (setq context (ghostel-project-claude--file-context filename start end)))
         (t ; non-file-backed buffers
          (setq context (ghostel-project-claude--non-file-context start end)))
         )
        (deactivate-mark))

      (when claude-buffer
        (with-current-buffer claude-buffer
          (deactivate-mark)
          (pcase arg
            (4  (ghostel--send-encoded "s" "ctrl")
                (sit-for 0.1))
            (16 (ghostel--send-encoded "escape" "")
                (ghostel--send-encoded "escape" "")
                (sit-for 0.1)))

          (ghostel-send-string (concat context "\n")))

        (display-buffer claude-buffer))))
  )
