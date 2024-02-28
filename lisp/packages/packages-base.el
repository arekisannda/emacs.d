;;; packages-base.el --- Emacs Packages Base Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package shut-up :demand t)

(use-package no-littering :demand t)

(use-package diminish :demand t)

(use-package disable-mouse :demand t
  :diminish disable-mouse-mode
  :config
  (disable-mouse-global-mode 1))

(use-package editorconfig :demand t
  :init
  (setq-default editorconfig-lisp-use-default-indent t)
  :config
  (editorconfig-mode 1))

(use-package undo-fu :after diminish)

(use-package llama)

(use-package epkg :after llama)

(use-package general)

(use-package hydra
  :init (setq-default hydra-key-doc-function nil))

(provide 'packages-base)

;;; packages-base.el ends here
