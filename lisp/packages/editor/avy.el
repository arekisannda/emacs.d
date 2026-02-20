;;; editor/avy.el -*- lexical-binding: t; -*-

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-timeout-seconds 0.5)
  (avy-background nil)
  :custom-face
  (avy-lead-face
   ((nil :weight bold
         :underline (:color foreground-color :style line :position nil)
         :foreground ,(doom-color 'red)
         :background ,(doom-color 'bg-alt)
         )))
  (avy-lead-face-0
   ((nil :inherit avy-lead-face :foreground unspecified :background unspecified)))
  (avy-lead-face-1
   ((nil :inherit avy-lead-face :foreground unspecified :background unspecified)))
  (avy-lead-face-2
   ((nil :inherit avy-lead-face :foreground unspecified :background unspecified)))
  )
