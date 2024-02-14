;;; packages-dap-mode.el --- Emacs dap-mode Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package dap-mode
  :ensure t
  :after '(lsp-mode lsp-treemacs)
  :init
  (setq dap-print-io nil)
  (setq dap-auto-configure-features '())
  :config
  (require 'dap-lldb)
  (require 'dap-dlv-go))

(provide 'packages-dap-mode)

;;; packages-dap-mode.el ends here
