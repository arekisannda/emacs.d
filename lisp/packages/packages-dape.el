;;; packages-dape.el --- Dape Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package dape
  :after jsonrpc
  :preface
  (setq dape-key-prefix nil))

(provide 'packages-dape)

;;; packages-dape.el ends here
