;;; lang-rust.el --- Rust Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'lang-utils)

(defun lang/rust--setup ()
  "Configurations for rust."
  (lsp-deferred))

(defun lang/rust-setup ()
  "Configurations for rust."
  (add-hook 'rust-ts-mode-hook #'lang/rust--setup)

  (lang/utils--set-auto-mode
   '(("\\.rs\\'" . rust-ts-mode))))

(lang/rust-setup)

(provide 'lang-rust)

;;; lang-rust.el ends here
