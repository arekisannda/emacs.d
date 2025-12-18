;;; lang/python.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package flymake-ruff :defer t)

(defun +lang-python-mode-setup ()
  (setq-local python-flymake-command '("flake8" "--max-line-length=120" "-"))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions #'python-flymake t t))
            nil t))

(use-package python-ts-mode
  :custom
  (python-indent-offset 4)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((python-mode . python-ts-mode)))
  :hook
  (python-ts-mode . +lang-python-mode-setup)
  (python-ts-mode . flymake-ruff-load))
