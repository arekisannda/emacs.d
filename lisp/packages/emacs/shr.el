;;; emacs/shr.el -*- lexical-binding: t; -*-

(use-package shr
  :custom
  (shr-image-animate nil)
  (shr-use-fonts nil)
  (shr-bullet "• ")
  (shr-hr-line "—")
  (shr-indentation 2)
  (shr-max-width 90))

(use-package eww
  :hook
  (eww-mode . visual-line-mode)
  (eww-mode . word-wrap-whitespace-mode))
