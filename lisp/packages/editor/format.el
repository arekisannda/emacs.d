;;; editor/format.el -*- lexical-binding: t; -*-

(use-package editorconfig
  :config
  (setq editorconfig-lisp-use-default-indent t)
  (editorconfig-mode t)
  :diminish editorconfig-mode)

(use-package prettier-js)
