;;; lang/docker.el -*- lexical-binding: t; -*-

(use-package dockerfile-ts-mode
  :mode
  ("Dockerfile\\'"    . dockerfile-ts-mode)
  ("\\.dockerfile\\'" . dockerfile-ts-mode))
