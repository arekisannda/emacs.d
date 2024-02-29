;;; lang-kotlin.el --- Kotlin Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package kotlin-ts-mode
  :mode
  ("\\.kt\\'" . kotlin-ts-mode))

(provide 'lang-kotlin)

;;; lang-kotlin.el ends here
