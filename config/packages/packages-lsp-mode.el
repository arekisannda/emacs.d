;;; packages-lsp-mode.el --- Emacs lsp-mode Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix nil
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-doc-show-with-mouse nil
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-completion-show-detail nil
        lsp-completion-show-kind nil
        lsp-ui-doc-enable nil
        lsp-enable-on-type-formatting nil)
  ;; performance tuning
  (setq gc-cons-threshold (* 1024 1024 100))
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.1)
  :commands (lsp lsp-mode lsp-deferred)
  :config
  (setq lsp-command-map (make-sparse-keymap)))

(provide 'packages-lsp-mode)

;;; packages-lsp-mode.el ends here
