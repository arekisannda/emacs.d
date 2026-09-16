;;; org/org-modern.el -*- lexical-binding: t; -*-

(use-package org-modern :after org
  :custom
  (org-modern-table nil)
  (org-modern-timestamp nil)
  (org-modern-todo nil)
  (org-modern-todo-faces nil)
  (org-modern-star 'replace)
  (org-modern-replace-stars "○○○◦◦◦∙")
  (org-modern-internal-target '(" ↪ " t " "))
  (org-modern-radio-target '("  " t " "))
  (org-modern-progress nil)
  (org-modern-checkbox '((?X . "󰄳") (?- . "󰝥") (?\s . "󰝦")))
  (org-modern-priority t)
  (org-modern-priority-faces
   '((?A . (:foreground "#ffffff" :weight semibold))
     (?B . (:foreground "#ff0000" :weight semibold))
     (?C . (:foreground "#ffff00" :weight semibold))
     (?D . (:foreground "#00d200" :weight semibold))
     (?E . (:foreground "#008eff" :weight semibold))))
  :config
  (utils/custom-set-faces
   (org-level-1
    ((nil :weight regular
          :foreground ,(doom-color 'blue)
          :height 1.20)))
   (org-level-2
    ((nil :weight regular
          :foreground ,(doom-color 'dark-blue)
          :height 1.20)))
   (org-level-3
    ((nil :weight regular
          :foreground ,(doom-color 'violet)
          :height 1.20)))
   (org-level-4
    ((nil :weight regular
          :foreground ,(doom-color 'magenta)
          :height 1.10)))
   (org-level-5
    ((nil :weight regular
          :foreground ,(doom-color 'red)
          :height 1.10)))
   (org-level-6
    ((nil :weight regular
          :foreground ,(doom-color 'orange)
          :height 1.10)))
   (org-level-7
    ((nil :weight regular
          :foreground ,(doom-color 'yellow)
          :height 1.00)))
   (org-level-8
    ((nil :inherit default
          :foreground ,(doom-color 'fg)
          :height 1.00)))
   (org-checkbox
    ((nil :box nil
          :height ,+fonts-fixed-pitch-size)))
   (org-modern-label
    ((nil :box (:line-width 5 :style flat-button)
          :height ,+fonts-fixed-pitch-size)))
   (org-block
    ((nil :background ,(doom-color 'bg))))
   ))
