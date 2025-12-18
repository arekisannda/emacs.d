;;; tools/rmsbolt.el -*- lexical-binding: t; -*-

(use-package rmsbolt
  :config
  (defun +rmsbolt-quit ()
    (interactive)
    (rmsbolt-mode -1)
    (if-let* ((buffer (get-buffer rmsbolt-output-buffer))
              (window (get-buffer-window buffer)))
        (quit-window t (get-buffer-window (get-buffer rmsbolt-output-buffer)))
      )
    ;; HACK: relies on rmsbolt closing assembly windows and focusing source window
    (window-state-put +rmsbolt-layout-state (frame-root-window) 'safe))

  (defun +rmsbolt-start ()
    (interactive)
    (unless (rmsbolt--get-lang)
      (user-error "rmsbolt unsupported language"))
    (setq-local +rmsbolt-layout-state (window-state-get (frame-root-window) t))
    (delete-other-windows)
    (rmsbolt))

  (defun +rmsbolt-toggle (&optional arg)
    (interactive "p")
    (pcase arg
      (4 (+rmsbolt-quit))
      (_ (if rmsbolt-mode
             (+rmsbolt-quit)
           (+rmsbolt-start)))
      )))
