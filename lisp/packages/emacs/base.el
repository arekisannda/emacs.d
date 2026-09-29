;;; emacs/base.el -*- lexical-binding: t; -*-

(use-package no-littering :demand t
  :config
  (no-littering-theme-backups))

(use-package aio :defer t)

(use-package diminish :defer t)

(use-package general :demand t)

(use-package hydra :defer t)

(use-package compat :defer t)

(use-package persist :defer t)

(use-package impatient-mode :defer t)

(use-package fringe-helper :demand t)

(defun +emacs-pulse-line (&rest _ )
  (interactive)
  (pulse-momentary-highlight-one-line (point) 'highlight))

(defun +emacs-pulse-window (&rest _)
  (interactive)
  (if (seq-some (lambda (f) (eq (frame-focus-state f) t)) (frame-list))
      (pulse-momentary-highlight-region (point-min) (point-max) 'highlight)))

(advice-add #'bookmark-jump :after #'+emacs-pulse-line)
(add-function :after after-focus-change-function #'+emacs-pulse-window)

(defun emacs-copy-buffer-file-name ()
  (interactive)
  (if-let* ((buffer-file-name buffer-file-name))
      (kill-new buffer-file-name)
    (user-error "Buffer is not a file.")))
