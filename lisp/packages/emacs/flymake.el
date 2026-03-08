;;; emacs/flymake.el -*- lexical-binding: t; -*-

(use-package flymake
  :defer t
  :custom
  (flymake-start-on-flymake-mode t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-fringe-indicator-position nil)
  (flymake-indicator-type nil)
  :custom-face
  (flymake-warning
   ((nil :underline (:style wave :color ,(doom-color 'orange)))))
  :config
  (defun flymake--highlight-line-wrapper (fn &rest args)
    (let ((ov (apply fn args)))
      (unless flymake-indicator-type
        (overlay-put ov 'before-string nil))))

  (advice-add #'flymake--highlight-line :around #'flymake--highlight-line-wrapper))
