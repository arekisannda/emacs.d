;;; emacs/winner.el -*- lexical-binding: t; -*-

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :hook
  (window-setup . winner-mode))
