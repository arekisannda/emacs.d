;;; ui/ace-window.el -*- lexical-binding: t; -*-

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-dispatch-when-more-than 0)
  (aw-swap-invert nil)
  (aw-dispatch-always nil)
  :custom-face
  (aw-leading-char-face
   ((nil :weight bold
         :height 2.00
         :box (:line-width 5 :style flat-button)
         :foreground ,(doom-color 'red)
         :background unspecified)))
  :config
  (defun +ace-swap-window ()
    "Ace swap window."
    (interactive)
    (aw-select " Ace - Swap Window"
               (apply-partially #'window-swap-states (selected-window))))
  (advice-add #'ace-swap-window :override #'+ace-swap-window)
  :hook
  (after-init . ace-window-posframe-mode))
