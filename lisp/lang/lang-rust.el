;;; lang-rust.el --- Rust Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package rust-mode
  :preface
  (defun +lang-rust-setup ()
    "Configurations for rust."
    (lsp-deferred))
  :hook
  (rust-ts-mode . +lang-rust-setup)
  :config
  ("\\.rs\\'" . rust-ts-mode))

(provide 'lang-rust)

;;; lang-rust.el ends here
