;;; editor/format.el -*- lexical-binding: t; -*-

(use-package editorconfig
  :demand t
  :config
  (setq editorconfig-lisp-use-default-indent t)
  (editorconfig-mode 1)
  :diminish editorconfig-mode)

(use-package prettier-js)
