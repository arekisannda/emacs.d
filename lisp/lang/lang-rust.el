;;; lang-rust.el --- Rust Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun lang/rust--setup ()
  "Configurations for rust."
  (lsp-deferred))

(defun lang/rust-setup ()
  "Configurations for rust."
  (add-hook 'rust-ts-mode-hook #'lang/rust--setup)

  (util/lang--set-auto-mode
   '(("\\.rs\\'" . rust-ts-mode))))

(lang/rust-setup)

(provide 'lang-rust)

;;; lang-rust.el ends here
