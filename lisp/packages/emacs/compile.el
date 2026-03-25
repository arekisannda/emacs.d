;;; emacs/compile.el -*- lexical-binding: t; -*-

(use-package compile
  :defer t
  :custom
  (compilation-ask-about-save nil)
  :config
  (defun +colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :hook
  (compilation-filter . +colorize-compilation-buffer))
