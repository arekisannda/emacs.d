;;; lang-python.el --- Python Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-python-setup ()
    "Configurations for python."
    (eglot-ensure))
  :hook
  (python-ts-mode-hook . +lang-python-setup)
  :config
  (util/lang--remap-major-mode
   '((python-mode . python-ts-mode))))

(provide 'lang-python)

;;; lang-python.el ends here
