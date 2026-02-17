;;; tools/lsp.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defvar-local +eglot-local-contact nil
  "Buffer-local `eglot' server configuration for project.")

(defun +eglot-with-local (alternative)
  "Generate server-choosing function with `+eglot-local-contact'.
If it is not set, use ALTERNATIVE instead."
  (lambda (&optional interactive _project)
    (if (and +eglot-local-contact (listp +eglot-local-contact))
        +eglot-local-contact
      alternative)))

(use-package eglot
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider
                                       :signatureHelpProvider))
  ;; (eglot-extend-to-xref nil)
  :config
  (let ((nixd          '("nixd" :name "nixd"))
        (tinymist      '("tinymist" :name "tinymist"))
        (texlab        '("texlab" :name "texlab"))
        (ltex-ls-plus  '("ltex-ls-plus" :name "ltex-ls-plus"))
        (ty            '("ty" "server" :name "ty"))
        (pyrefly       '("pyrefly" "lsp" :name "pyrefly"))
        (biome         '("biome" "lsp-proxy" :name "biome"))
        (html          (+eglot-with-local '("vscode-html-language-server" "--stdio" :name "vscode-html")))
        (scad          '("openscad-lsp" "--stdio"))
        (rust-analyzer '("rust-analyzer" :initializationOptions (:check (:command "clippy"))))
        )

    (dolist (conf `(((scad-mode                  :language-id "scad")             . ,scad)
                    ((nix-ts-mode                :language-id "nix")              . ,nixd)
                    ((typst-ts-mode              :language-id "typst")            . ,tinymist)
                    ((yaml-ts-mode               :language-id "yaml")             . ,ltex-ls-plus)
                    ((org-mode                   :language-id "org")              . ,ltex-ls-plus)
                    ((git-commit-elisp-text-mode :language-id "gitcommit")        . ,ltex-ls-plus)
                    ((bibtex-mode                :language-id "bibtex")           . ,ltex-ls-plus)
                    ((context-mode               :language-id "context")          . ,ltex-ls-plus)
                    (((latex-mode                :language-id "latex")
                      (LaTeX-mode                :language-id "latex"))           . ,texlab)
                    ((markdown-mode              :language-id "markdown")         . ,ltex-ls-plus)
                    ((rst-mode                   :language-id "restructuredtext") . ,ltex-ls-plus)
                    (((python-ts-mode            :language-id "python")
                      (python-mode               :language-id "python"))          . ,pyrefly)
                    ((html-mode                  :language-id "html")             . ,html)
                    (((rust-mode                 :language-id "rust")
                      (rust-ts-mode              :language-id "rust"))           . ,rust-analyzer)
                    ))
      (setf (alist-get (car conf) eglot-server-programs nil nil #'equal)
            (cdr conf))))

  (fset #'jsonrpc--log-event #'ignore)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun +eglot--shutdown-project (name)
    (let* ((project (project-current nil name))
           (servers (gethash project eglot--servers-by-project)))
      (dolist (server servers)
        (eglot-shutdown server))))

  (setq eglot-stay-out-of '(flymake))

  (defun +eglot-flymake-setup ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (flymake-mode 1))

  :autoload eglot-managed-p
  :hook
  (prog-mode . util/lsp-ensure-modes)
  (text-mode . util/lsp-ensure-modes)
  (eglot-managed-mode . +eglot-flymake-setup)
  (eglot-managed-mode . eldoc-mode))

(use-package consult-eglot :after eglot)

(use-package eglot-booster :after eglot
  :custom
  (eglot-booster-io-only t)
  :config
  (eglot-booster-mode))
