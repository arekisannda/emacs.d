;;; ui/faces.el -*- lexical-binding: t; -*-

(defface popup-border
  '((((type tty)) :inherit mode-line-inactive))
  "Face used for popup borders."
  :group 'basic-faces)

(defface fold-replacement-face
  '((((type tty)) :inherit 'font-lock-comment-face))
  "Face used for fold replacement face."
  :group 'basic-faces)

(use-package nil ; _faces_
  :custom-face
  (match
   ((nil :inherit unspecified
         :box nil
         :foreground unspecified
         :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg) 0.3)
         )))
  (isearch
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'bg)
         :background ,(doom-color 'yellow))))
  (isearch-fail
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'magenta)
         :background unspecified)))
  (isearch-group-1
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'fg)
         :background ,(doom-blend (doom-color 'orange) (doom-color 'bg) 0.5)
         )))
  (isearch-group-2
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'fg)
         :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.5)
         )))
  (popup-isearch-match
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'fg)
         :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg) 0.3)
         )))

  (line-number
   ((nil :inherit default
         :weight normal
         :foreground ,(doom-color 'comments))))
  (line-number-current-line
   ((nil :inherit default
         :weight bold
         :foreground ,(doom-color 'orange))))

  (show-paren-match
   ((nil :inherit region
         :weight bold
         :foreground unspecified
         :background unspecified)))
  (show-paren-mismatch
   ((nil :weight bold
         :foreground ,(doom-color 'red))))

  (default
   ((nil :weight normal
         :font ,+fonts-fixed-pitch-face
         :height ,+fonts-fixed-pitch-size)))
  (fixed-pitch
   ((nil :weight normal
         :font ,+fonts-fixed-pitch-face
         :height ,+fonts-fixed-pitch-size)))
  (variable-pitch
   ((nil :weight normal
         :font ,+fonts-variable-pitch-face
         :height ,+fonts-variable-pitch-size)))
  (variable-pitch-text
   ((nil :weight normal
         :font ,+fonts-variable-pitch-face
         :height ,+fonts-variable-pitch-size)))
  (italic
   ((nil :slant italic
         :underline nil
         :font ,+fonts-fixed-pitch-italic-face)))
  (bold-italic
   ((nil :weight bold
         :slant italic
         :underline nil
         :font ,+fonts-fixed-pitch-italic-face)))
  (highlight
   ((nil :extend t
         :background ,(doom-color 'selection)
         :foreground unspecified)))
  (fringe
   ((nil :foreground ,(doom-color 'fg-alt))))

  (font-lock-comment-face
   ((nil :inherit italic)))

  (popup-border
   ((nil :inherit unspecified
         :foreground ,(doom-darken (doom-blend (doom-color 'red) (doom-color 'orange) 0.3) 0.3)
         :background ,(doom-darken (doom-blend (doom-color 'red) (doom-color 'orange) 0.3) 0.3)
         )))

  (fold-replacement-face
   ((nil :foreground ,(doom-color 'dark-blue)
         :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg) 0.2)
         :weight bold)))

  (diff-refine-removed
   ((nil  :inverse-video nil
          :foreground ,(doom-color 'red)
          :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.2)
          )))

  (diff-refine-added
   ((nil :inverse-video nil
         :foreground ,(doom-color 'green)
         :background ,(doom-blend (doom-color 'green) (doom-color 'bg) 0.2)
         )))

  (diff-removed
   ((nil :background unspecified
         :foreground ,(doom-color 'red))))

  (diff-indicator-removed
   ((nil :inherit hl-line
         :background unspecified
         :foreground ,(doom-color 'red))))

  (diff-added
   ((nil :background unspecified
         :foreground ,(doom-color 'green))))

  (diff-indicator-added
   ((nil :inherit hl-line
         :background unspecified
         :foreground ,(doom-color 'green))))

  (bookmark-face
   ((nil :inherit unspecified
         :background unspecified
         :foreground ,(doom-color 'red))))
  )

(use-package rainbow-delimiters)

(use-package rainbow-mode
  :defer t
  :custom
  (rainbow-r-colors-alist '())
  (rainbow-html-colors-alist '()))
