;;; lang-rust.el --- Rust Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package rust-mode
  :preface
  (defun +lang-rust-setup ()
    "Configurations for rust."
    (util/lsp-ensure))
  :hook
  (rust-ts-mode . +lang-rust-setup)
  :mode
  ("\\.rs\\'" . rust-ts-mode))

(provide 'lang-rust)

;;; lang-rust.el ends here
