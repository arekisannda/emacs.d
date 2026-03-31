;;; lang/nix.el -*- lexical-binding: t; -*-

(use-package nix-ts-mode
  :mode
  ("\\.nix\\'" . nix-ts-mode))

(use-package nix-mode)

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-default-configurations
   (cons
    "nix"
    '( :nixd
       ( :nixpkgs (:expr "import (builtins.getFlake (builtins.toString ./)).inputs.nixpkgs { }")
         :formatting (:command ["nixfmt"])
         :options ()
         ))
    ))
  )
