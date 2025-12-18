;;; lang/typescript.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package typescript-mode
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((typescript-mode . typescript-ts-mode)))
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode))

(use-package ob-typescript :defer t)
