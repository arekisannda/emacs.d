;;; lang/nix.el -*- lexical-binding: t; -*-

(use-package nix-ts-mode
  :mode
  ("\\.nix\\'" . nix-ts-mode))

(use-package nix-mode)
