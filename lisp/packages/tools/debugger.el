;;; tools/debugger.el -*- lexical-binding: t; -*-

(use-package dape
  :preface
  (setq dape-key-prefix nil)
  :custom
  (dape-active-mode nil)
  (dape-inlay-hints nil)
  (dape-buffer-window-arrangement 'nil)

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
   '(dape-info dape-repl))

  :config
  (utils/custom-set-faces
   (dape-breakpoint-face
    ((nil :stipple nil)))
   (dape-header-line-active-face
    ((nil :stipple nil
          :foreground ,(doom-color 'fg)
          :background ,(doom-color 'bg)
          :overline nil
          )))
   (dape-header-line-inactive-face
    ((nil :stipple nil
          :foreground ,(doom-color 'fg-alt)
          :background ,(doom-color 'bg-alt)
          :overline nil
          )))
   )
  :hook
  (dape-info-parent-mode . emacs-set-alt-face))

(defun +edebug-defun-region ()
  (interactive)
  (call-interactively #'narrow-to-region)
  (edebug-defun)
  (call-interactively #'widen)
  (deactivate-mark))
