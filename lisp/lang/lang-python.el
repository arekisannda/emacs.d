;;; lang-python.el --- Python Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun lang/python--setup ()
  "Configurations for python."
  (lsp-deferred)
  (setq lsp-clients-pylsp-library-directories))

(defun lang/python-setup ()
  "Configurations for python."
  (add-hook 'python-ts-mode-hook #'lang/python-setup)

  (util/lang--remap-major-mode
   '((python-mode . python-ts-mode))))

(lang/python-setup)

(provide 'lang-python)

;;; lang-python.el ends here
