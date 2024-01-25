;;; programming.el --- Emacs programming editor packages and configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

;;; modes {{{
(use-package i3wm-config-mode :ensure t)
;;; }}}

;;; lsp-mode {{{
(defun configs--lsp-go-config ()
  "Config method for golang lsp configurations."
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)))

  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

;; (defun configs--lsp-python-config ()
;;   "Config method for python lsp configurations."
;;   (setq lsp-clients-pylsp-library-directories))

;; (defun configs--lsp-cpp-config ()
;;   "Config method for c++ lsp configurations"
;;   ())

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
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq lsp-idle-delay 0.1)

  :hook
  (go-ts-mode . lsp-deferred)
  (c++-ts-mode . lsp-deferred)
  (c-ts-mode . lsp-deferred)
  (c-or-c++-ts-mode . lsp-deferred)
  :commands (lsp lsp-mode lsp-deferred)
  :config
  (setq lsp-command-map (make-sparse-keymap))
  (add-hook 'go-ts-mode #'configs--lsp-go-config))
;;; }}}

;;; dap-mode {{{
(use-package dap-mode
  :ensure t
  :init
  (setq dap-print-io nil)
  (setq dap-auto-configure-features '())
  :config
  (require 'dap-lldb)
  (require 'dap-dlv-go))
;;; }}}

(provide 'configs-programming)
;;; programming.el ends here
