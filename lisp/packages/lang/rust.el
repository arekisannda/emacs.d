;;; lang/rust.el -*- lexical-binding: t; -*-

(defun +lang-rust-mode-setup ()
  "Setup to run for `rust` modes."
  nil)

(use-package flymake-clippy :defer t)

(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . flymake-clippy-setup-backend))

(use-package ob-rust :defer t)
