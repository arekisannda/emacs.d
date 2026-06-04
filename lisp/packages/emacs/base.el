;;; emacs/base.el -*- lexical-binding: t; -*-

(use-package no-littering :demand t
  :config
  (no-littering-theme-backups))

(use-package aio :defer t)

(use-package diminish :defer t)

(use-package general :defer t)

(use-package hydra :defer t)

(use-package compat :defer t)

(use-package persist :defer t)

(use-package impatient-mode :defer t)

(advice-add #'bookmark-jump :after (lambda (&rest _) (pulse-momentary-highlight-one-line (point))))

(setq comp-async-buffer-name " *Async-native-compile-log*")

(defun emacs-copy-buffer-file-name ()
  (interactive)
  (if-let ((buffer-file-name buffer-file-name))
      (kill-new buffer-file-name)
    (user-error "Buffer is not a file.")))
