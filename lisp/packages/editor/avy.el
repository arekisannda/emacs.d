;;; avy.el -*- lexical-binding: t; -*-

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-timeout-seconds 0.5)
  (avy-background nil)
  :custom-face
  (avy-lead-face
   ((nil :inherit unspecified
         :weight bold
         :box nil
         :foreground ,(doom-color 'bg)
         :background ,(doom-color 'yellow))))
  (avy-lead-face-0
   ((nil :inherit unspecified
         :weight bold
         :box nil
         :foreground ,(doom-color 'bg)
         :background ,(doom-color 'yellow))))
  (avy-lead-face-1
   ((nil :inherit unspecified
         :weight bold
         :box nil
         :foreground ,(doom-color 'bg)
         :background ,(doom-color 'yellow))))
  (avy-lead-face-2
   ((nil :inherit unspecified
         :weight bold
         :box nil
         :foreground ,(doom-color 'bg)
         :background ,(doom-color 'yellow))))
  )
