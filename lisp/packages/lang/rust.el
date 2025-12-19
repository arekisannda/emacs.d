;;; lang/rust.el -*- lexical-binding: t; -*-

(defun +lang-rust-mode-setup ()
  "Setup to run for `rust` modes."
  (setq-local rust-cargo-bin (executable-find "cargo")))

(use-package rust-mode
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((rust-mode . rust-ts-mode)))
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . +lang-rust-mode-setup))

(use-package flymake-clippy
  :hook
  (rust-ts-mode . flymake-clippy-setup-backend))

(use-package ob-rust :defer t)
