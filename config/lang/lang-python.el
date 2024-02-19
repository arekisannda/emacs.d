;;; lang-python.el --- Python Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'lang-utils)

(defun lang/python--setup ()
  "Configurations for python."
  (lsp-deferred)
  (setq lsp-clients-pylsp-library-directories))

(defun lang/python-setup ()
  "Configurations for python."
  (add-hook 'python-ts-mode-hook #'lang/python-setup)

  (lang/utils--remap-major-mode
   '((python-mode . python-ts-mode))))

(lang/python-setup)

(provide 'lang-python)

;;; lang-python.el ends here
