;;; lang/sway.el -*- lexical-binding: t; -*-

(use-package i3wm-config-mode
  :mode
  ("\\.sway\\'" . i3wm-config-mode)
  ("\\.i3\\'" . i3wm-config-mode)
  :hook
  (i3wm-config-mode . display-line-numbers-mode)
  (i3wm-config-mode . rainbow-delimiters-mode))
