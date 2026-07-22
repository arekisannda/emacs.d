;;; tools/debugger.el -*- lexical-binding: t; -*-

(use-package dape
  :preface
  (setq dape-key-prefix nil)
  :custom-face
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
   '(
     ;; dape-repl
     dape-info
     (lambda () (interactive)
       (if-let ((compilation-buffer (get-buffer "*compilation*")))
           (quit-window nil (get-buffer-window compilation-buffer)))
       (select-window (windex-get-mru-in-main)))))
  :config

  (defun dape-frame (fn &rest r)
    (interactive)
    (let ((parent-frame (selected-frame))
          frame)
      (setq frame
            (make-frame
             (append
              `((no-other-frame . t)
                (left           . 0.5)
                (top            . 0.5)
                (minibuffer     . t))
              )))
      (select-frame-set-input-focus frame t)
      (with-selected-frame frame
        (apply fn r))
      ))


  ;; (advice-add #'dape :around #'dape-frame)
  )

(defun +edebug-defun-region ()
  (interactive)
  (call-interactively #'narrow-to-region)
  (edebug-defun)
  (call-interactively #'widen)
  (deactivate-mark))
