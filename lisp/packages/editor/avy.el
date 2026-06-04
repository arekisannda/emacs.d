;;; editor/avy.el -*- lexical-binding: t; -*-

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-timeout-seconds 0.5)
  (avy-background t)
  :custom-face
  (avy-background-face
   ((nil :stipple nil)))
  (avy-lead-face
   ((nil :weight bold
         :underline (:color foreground-color :style line :position nil)
         :foreground ,(doom-color 'red)
         :background ,(doom-color 'bg)
         )))
  (avy-lead-face-0
   ((nil :inherit unspecified :foreground ,(doom-color 'vertical-bar) :background unspecified)))
  (avy-lead-face-1
   ((nil :inherit unspecified :foreground ,(doom-color 'vertical-bar) :background unspecified)))
  (avy-lead-face-2
   ((nil :inherit unspecified :foreground ,(doom-color 'vertical-bar) :background unspecified)))
  )
