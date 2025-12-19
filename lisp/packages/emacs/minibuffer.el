;;; emacs/minibuffer.el -*- lexical-binding: t; -*-

(defun +emacs-minibuffer-setup ()
  (setq gc-cons-threshold most-positive-fixnum
        left-margin-width 0
        right-margin-width 0)
  (set-window-fringes (selected-window) 8 8 nil t)
  (marginalia-mode +1))

(defun +emacs-minibuffer-exit ()
  (marginalia-mode -1)
  (setq gc-cons-threshold packages/emacs-gc-cons-threshold))

(defun +emacs-message-buffer-setup ()
  (visual-line-mode t)
  (follow-mode t))

(use-package minibuffer
  :custom
  (minibuffer-message-clear-timeout 0)
  :hook
  (messages-buffer-mode . +emacs-message-buffer-setup)
  (minibuffer-setup     . +emacs-minibuffer-setup)
  (minibuffer-exit      . +emacs-minibuffer-exit))
