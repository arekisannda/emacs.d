;;; tools/envrc.el -*- lexical-binding: t; -*-

(defvar-local +envrc-update-hook '()
  "Buffer-local hook to run after `envrc--update'.")

(defun +envrc--update-after-setup ()
  "Setup to run after `envrc--update'."
  (run-hooks '+envrc-update-hook))

(use-package envrc
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :hook
  (after-init . envrc-global-mode)
  :init
  (advice-add #'envrc--update :after #'+envrc--update-after-setup)
  :config
  (defun +envrc-root ()
    (let* ((envrc-dir (envrc--find-env-dir))
           (project (project-current))
           (project-dir (and project (project-root project))))
      (cond
       ((and envrc-dir (envrc--env-dir-p envrc-dir)) envrc-dir)
       ((and project-dir (envrc--env-dir-p project-dir)) (expand-file-name project-dir))
       ))
    ))
