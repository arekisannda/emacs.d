;;; lang/web.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((css-mode . css-ts-mode))))

(use-package xml-mode
  :mode
  ("\\.opf\\'" . xml-mode)
  ("\\.ncx\\'" . xml-mode))
