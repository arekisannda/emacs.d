;;; packages-dape.el --- Dape Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package dape
  :preface
  (setq dape-key-prefix nil)
  :config
  ;; Fix indent-bars stipple
  (set-face-attribute 'dape-breakpoint-face nil :stipple nil)
  :custom
  ;; (dape-breakpoint-global-mode t)
  (dape-breakpoint-margin-string
   (propertize "●" :face 'dape-breakpoint-face))
  (dape-repl-commands
   '((" debug"      . dape)
     (" next"       . dape-next)
     (" continue"   . dape-continue)
     (" pause"      . dape-pause)
     (" step"       . dape-step-in)
     (" out"        . dape-step-out)
     (" restart"    . dape-restart)
     ("󰯇 kill"       . dape-kill)
     (" disconnect" . dape-disconnect-quit)
     ("󰩈 quit"       . dape-quit))))

(provide 'packages-dape)

;;; packages-dape.el ends here
