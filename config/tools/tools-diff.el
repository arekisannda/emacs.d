;;; tools-diff.el --- Emacs Diff Tool Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'ediff)

;; (defun ediff-setup-windows-custom (buffer-A buffer-B buffer-C control-buffer))
(setq ediff-setup-windows-function 'ediff-setup-windows-merge)

(provide 'tools-diff)

;;; tools-diff.el ends here
