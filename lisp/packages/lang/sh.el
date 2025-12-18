;;; lang/sh.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package shell
  :custom
  (sh-basic-offset 2)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((sh-mode . bash-ts-mode)))
  )
