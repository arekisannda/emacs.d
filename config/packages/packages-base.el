;;; packages-base.el --- Emacs Packages Base Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package shut-up :ensure t :demand t)

(use-package no-littering :ensure t :demand t)

(use-package diminish :ensure t :demand t)

(use-package disable-mouse :ensure t :demand t)

(use-package editorconfig :ensure t :demand t)

(use-package undo-fu :ensure t :after diminish)

(use-package llama :ensure t)

(use-package epkg :ensure t :after llama)

(use-package general :ensure t)

(use-package hydra :ensure t
  :init (setq-default hydra-key-doc-function nil))

(elpaca-wait)

(provide 'packages-base)

;;; packages-base.el ends here
