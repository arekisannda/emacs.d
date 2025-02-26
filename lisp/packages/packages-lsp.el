;;; packages-lsp.el --- LSP/Language Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package eglot
  :ensure nil
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  ;; (eldoc-echo-area-use-multiline-p nil)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider))

  ;; (eglot-extend-to-xref nil)
  ;; (eglot-highlight-symbol-face ((t (:inherit (lazy-highlight)))))
  :config
  ;; (setf (alist-get '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
  ;;                  eglot-server-programs nil nil #'equal)
  ;;       '("clangd" "--clang-tidy")
  ;; (fset #'jsonrpc--log-event #'ignore)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . (lambda ()
                          (setq-local context-menu nil)
                          (eldoc-mode -1))))

(use-package consult-eglot :after eglot)

(provide 'packages-lsp)

;;; packages-lsp.el ends here
