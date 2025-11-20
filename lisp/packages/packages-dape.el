;;; packages-dape.el --- Dape Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

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
     (lambda () (interactive) (select-window (windex-get-mru-in-main))))))

(provide 'packages-dape)

;;; packages-dape.el ends here
