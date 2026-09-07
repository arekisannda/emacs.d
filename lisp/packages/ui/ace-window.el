;;; ui/ace-window.el -*- lexical-binding: t; -*-

(use-package ace-window
  :custom
  (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?8 ?9 ?0))
  (aw-scope 'frame)
  (aw-dispatch-when-more-than 0)
  (aw-swap-invert nil)
  (aw-dispatch-always nil)
  (aw-display-mode-overlay nil)
  (aw-background t)
  :config
  (utils/custom-set-faces
   (aw-leading-char-face
    ((nil :weight bold
          :height 1.0
          :background ,(doom-color 'red)
          :foreground ,(doom-color 'bg))))
   )

  (defun +ace-swap-window ()
    "Ace swap window."
    (interactive)
    (aw-select " Ace - Swap Window"
               (apply-partially #'window-swap-states (selected-window))))
  (advice-add #'ace-swap-window :override #'+ace-swap-window)
  :hook
  (after-init . ace-window-display-mode))
