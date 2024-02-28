;;; packages-dap-mode.el --- DAP-mode Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package dap-mode :after (lsp-mode lsp-treemacs)
  :init
  (setq dap-print-io nil)
  (setq dap-auto-configure-features '())
  :config
  (require 'dap-lldb)
  (require 'dap-dlv-go))

(provide 'packages-dap-mode)

;;; packages-dap-mode.el ends here
