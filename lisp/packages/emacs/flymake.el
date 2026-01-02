;;; emacs/flymake.el -*- lexical-binding: t; -*-

(use-package flymake
  :defer t
  :custom
  (flymake-start-on-flymake-mode t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-indicator-type nil)
  (flymake-fringe-indicator-position nil)
  :custom-face
  (flymake-warning
   ((nil :underline (:style wave :color ,(doom-color 'orange))))))
