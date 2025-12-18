;;; ui/indent-bars.el -*- lexical-binding: t; -*-

(use-package indent-bars
  :custom
  (indent-bars-no-stipple-char 9615)
  (indent-bars-depth-update-delay 0.1)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-starting-column 0)
  (indent-bars-color-by-depth nil)
  (indent-bars-color `(,(doom-blend (doom-color 'vertical-bar) (doom-color 'bg) 0.5)))
  ;; '(highlight :face-bg t :blend 0.2))
  (indent-bars-highlight-current-depth nil)
  ;; '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.0)
  (indent-bars-width-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-zigzag nil)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-display-on-blank-lines 'least))
