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
    ((nil :stipple nil
          :foreground ,(doom-color 'red))))

   (dape-header-line-active-face
    ((nil :stipple nil
          :foreground ,(doom-color 'yellow)
          :background unspecified
          :overline nil
          )))
   (dape-header-line-inactive-face
    ((nil :stipple nil
          :foreground ,(doom-color 'fg-alt)
          :background unspecified
          :overline nil
          )))
   )

  (defun +dape-indicator-margin (string _bitmap face)
    "Always draw dape indicators in the left margin."
    ;; apply the new width to windows already showing this buffer
    (dolist (w (get-buffer-window-list nil nil t))
      (set-window-margins w left-margin-width right-margin-width))
    (propertize " " 'display
                `((margin left-margin) ,(propertize string 'face face))))

  (advice-add 'dape--indicator :override #'+dape-indicator-margin)
  :hook
  (dape-info-parent-mode . emacs-set-alt-face))

(defun +edebug-defun-region ()
  (interactive)
  (call-interactively #'narrow-to-region)
  (edebug-defun)
  (call-interactively #'widen)
  (deactivate-mark))
