;;; lang-python.el --- Python Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-python-setup ()
    "Configurations for python."
    (lsp-deferred)
    (setq lsp-clients-pylsp-library-directories))
  :hook
  (python-ts-mode-hook . +lang-python-setup)
  :config
  (util/lang--remap-major-mode
   '((python-mode . python-ts-mode))))

(provide 'lang-python)

;;; lang-python.el ends here
