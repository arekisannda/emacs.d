;;; lang-go.el --- Golang Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-go-setup ()
    (eglot-ensure))
  :mode
  ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . +lang-go-setup))

(provide 'lang-go)

;;; lang-go.el ends here
