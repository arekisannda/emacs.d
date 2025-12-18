;;; lang/kotlin.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package kotlin-mode)

(use-package kotlin-ts-mode
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((kotlin-mode . kotlin-ts-mode)))
  :mode
  ("\\.kt\\'" . kotlin-ts-mode))

(use-package ob-kotlin :defer t)
