;;; packages-base.el --- Base Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package shut-up :demand t)

(use-package no-littering :demand t)

(use-package diminish :demand t)

(use-package disable-mouse :demand t
  :diminish disable-mouse-mode
  :hook
  (elpaca-after-init . disable-mouse-global-mode))

(use-package editorconfig :demand t
  :hook
  (elpaca-after-init . editorconfig-mode)
  :config
  (setq-default editorconfig-lisp-use-default-indent t))

(use-package undo-fu :after diminish)

(use-package llama)

(use-package epkg :after llama)

(use-package general)

(use-package hydra
  :init
  (setq-default hydra-key-doc-function nil))

(use-package seq
  :ensure (seq))

(use-package jsonrpc
  :ensure (jsonrpc))

(provide 'packages-base)

;;; packages-base.el ends here
