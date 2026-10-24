;;; lang/just.el -*- lexical-binding: t; -*-

(use-package just-ts-mode
  :mode
  ("\\.justfile\\'" . just-ts-mode)
  ("\\.just\\'" . just-ts-mode))

(use-package justcmd
  :custom
  (justcmd-default-shell-command #'detached-shell-command)
  :config
  (defun util/commands-add-just-commands (buffer)
    (require 'justcmd)
    (with-current-buffer (window-normalize-buffer buffer)
      (justcmd-commands-list)))

  (add-hook 'util/commands-add-base-commands-functions #'util/commands-add-just-commands)
  )
