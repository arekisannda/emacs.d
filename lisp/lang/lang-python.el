;;; lang-python.el --- Python Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-python-setup ()
    "Configurations for python."
    (util/lsp-ensure))
  :hook
  (python-ts-mode . +lang-python-setup)
  :custom
  (major-mode-remap-alist
   (append '((python-mode . python-ts-mode))
           major-mode-remap-alist)))

(provide 'lang-python)

;;; lang-python.el ends here
