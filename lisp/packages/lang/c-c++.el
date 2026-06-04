;;; lang/c-c++.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package google-c-style)

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-style 'k&r)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((c++-mode      . c++-ts-mode)
     (c-mode        . c-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode))))
