;;; lang/python.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defun +lang-python-flymake-setup ()
  "Setup to run for `flymake-ruff`."
  (when-let ((exec (executable-find "ruff")))
    (setq-local flymake-ruff-program exec)
    (flymake-ruff-load)))

(defun +lang-python-mode-setup ()
  (remove-hook 'flymake-diagnostic-functions #'python-flymake t)
  (when (+envrc-root)
    (add-hook '+envrc-update-hook #'+lang-python-flymake-setup nil t))
  (+lang-python-flymake-setup))

(use-package python
  :custom
  (python-indent-offset 4)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((python-mode . python-ts-mode)))
  :hook
  (python-ts-mode . +lang-python-mode-setup))

(use-package flymake-ruff)
