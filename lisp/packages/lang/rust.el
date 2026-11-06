;;; lang/rust.el -*- lexical-binding: t; -*-

(defun +lang-rust-flymake-setup ()
  "Setup to run for `flymake-clippy`."
  (when-let* ((exec (executable-find "cargo")))
    (setq-local flymake-clippy-cargo-path exec)
    (flymake-clippy-setup-backend)))

(defun +lang-rust-mode-setup ()
  "Setup to run for `rust` modes."
  (when (+envrc-root)
    (add-hook '+envrc-update-hook #'+lang-rust-flymake-setup nil t))
  (+lang-rust-flymake-setup))

(use-package flymake-clippy)

(use-package rust-mode
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((rust-mode . rust-ts-mode)))
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . +lang-rust-mode-setup))

(use-package ob-rust :defer t)
