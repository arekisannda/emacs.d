;;; packages-lsp.el --- LSP/Language Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
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
  :ensure nil
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
  ;; (eglot-highlight-symbol-face ((t (:inherit (lazy-highlight)))))
  :config
  (let ((nixd         '("nixd" :name "nixd"))
        (tinymist     '("tinymist" :name "tinymist"))
        (ltex-ls-plus '("ltex-ls-plus" :name "ltex-ls-plus"))
        (ty           '("ty" "server" :name "ty"))
        (pyrefly      '("pyrefly" "lsp" :name "pyrefly"))
        (biome        '("biome" "lsp-proxy" :name "biome"))
        (html         (+eglot-with-local '("vscode-html-language-server" "--stdio" :name "vscode-html")))
        )

    (dolist (conf `(((scad-mode                  :language-id "scad")             . ("openscad-lsp" "--stdio"))
                    ((nix-ts-mode                :language-id "nix")              . ,nixd)
                    ((typst-ts-mode              :language-id "typst")            . ,tinymist)
                    ((yaml-ts-mode               :language-id "yaml")             . ,ltex-ls-plus)
                    ((org-mode                   :language-id "org")              . ,ltex-ls-plus)
                    ((git-commit-elisp-text-mode :language-id "gitcommit")        . ,ltex-ls-plus)
                    ((bibtex-mode                :language-id "bibtex")           . ,ltex-ls-plus)
                    ((context-mode               :language-id "context")          . ,ltex-ls-plus)
                    ((latex-mode LaTeX-mode      :language-id "latex")            . ,ltex-ls-plus)
                    ((markdown-mode              :language-id "markdown")         . ,ltex-ls-plus)
                    ((rst-mode                   :language-id "restructuredtext") . ,ltex-ls-plus)
                    (((python-ts-mode            :language-id "python")
                      (python-mode               :language-id "python"))          . ,pyrefly)
                    ((html-mode                  :language-id "html")             . ,html))
                  )
      (setf (alist-get (car conf) eglot-server-programs nil nil #'equal)
            (cdr conf))))

  (fset #'jsonrpc--log-event #'ignore)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (setq eglot-stay-out-of '(flymake))

  (defun +eglot--shutdown-project (name)
    (let* ((project (project-current nil name))
           (servers (gethash project eglot--servers-by-project)))
      (dolist (server servers)
        (eglot-shutdown server))))
  :hook
  (prog-mode . util/lsp-ensure-modes)
  (text-mode . util/lsp-ensure-modes)
  (eglot-managed-mode . (lambda ()
                          (setq-local context-menu nil)
                          (eldoc-mode 1))))

(use-package consult-eglot :after eglot)

(use-package eglot-booster :after eglot
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :custom
  (eglot-booster-io-only t)
  :hook
  (elpaca-after-init . eglot-booster-mode))

(use-package eldoc-box :after (eldoc eglot)
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))

(defun +eldoc-close-buffer ()
  "Helper function to kill Eldoc doc buffer."
  (interactive)
  (let (window)
    (when (and (buffer-live-p eldoc--doc-buffer)
               (setq window (get-buffer-window eldoc--doc-buffer)))
      (quit-window t window))))

;; (defun +eldoc)

;; (defun +eldoc-display-in-side (&optional interactive)
;;   (let (eldoc-display-functions '()))
;;   )

(provide 'packages-lsp)

;;; packages-lsp.el ends here
