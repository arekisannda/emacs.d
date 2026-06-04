;;; tools/debugger.el -*- lexical-binding: t; -*-

(use-package dape
  :preface
  (setq dape-key-prefix nil)
  :custom-face
  (dape-breakpoint-face
   ((nil :stipple nil)))
  :custom
  (dape-active-mode nil)
  (dape-inlay-hints nil)
  (dape-buffer-window-arrangement 'right)

  ;; (dape-breakpoint-global-mode t)
  (dape-breakpoint-margin-string
   (propertize "●" :face 'dape-breakpoint-face))
  (dape-repl-commands
   '(("debug"      . dape)
     ("next"       . dape-next)
     ("continue"   . dape-continue)
     ("pause"      . dape-pause)
     ("step"       . dape-step-in)
     ("out"        . dape-step-out)
     ("restart"    . dape-restart)
     ("kill"       . dape-kill)
     ("disconnect" . dape-disconnect-quit)
     ("quit"       . dape-quit)))

  (dape-start-hook
   '(dape-repl
     dape-info
     (lambda () (interactive)
       (if-let ((compilation-buffer (get-buffer "*compilation*")))
           (quit-window nil (get-buffer-window compilation-buffer)))
       (select-window (windex-get-mru-in-main)))))
  :config

  (defvar +dape-layout-state nil)

  (defun +dape-start (&rest _)
    (setq +dape-layout-state (window-state-get (frame-root-window) t)))

  (advice-add #'dape :before #'+dape-start)

  (defun +dape-stop (&rest _)
    (window-state-put +dape-layout-state (frame-root-window) 'safe))

  (advice-add #'dape-quit :after #'+dape-stop)
  )

(defun +edebug-defun-region ()
  (interactive)
  (call-interactively #'narrow-to-region)
  (edebug-defun)
  (call-interactively #'widen)
  (deactivate-mark))
