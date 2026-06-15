;;; ui/indent-bars.el -*- lexical-binding: t; -*-

(use-package indent-bars
  :custom
  (indent-bars-no-stipple-char 9615)
  (indent-bars-depth-update-delay 0.1)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-starting-column 0)
  (indent-bars-color-by-depth nil)
  (indent-bars-color `(,(doom-color 'vertical-bar) :face-bg t :blend 0.5))
  (indent-bars-unspecified-bg-color "black")
  (indent-bars-unspecified-fg-color "black")
  (indent-bars-highlight-current-depth nil)
  ;; '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.0)
  (indent-bars-width-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-zigzag nil)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-display-on-blank-lines 'least)
  :custom
  (indent-bars-invisible-face
   ((nil :stipple nil :foreground ,(doom-color 'bg):background ,(doom-color 'bg))))
  :config
  (defun indent-bars--create-faces (style num)
    "Create bar faces up to depth NUM for STYLE."
    (mapc (lambda (face)
            (unless (string-match-p "indent-bar" (symbol-name face))
              (set-face-attribute face nil :stipple nil)))
          (face-list))


    (face-spec-set (intern "indent-bars-face")
                   `((t . ( :stipple nil :inherit nil
                            :foreground unspecified ))))
    (vconcat
     (cl-loop
      for i from 0 to num
      for face = (indent-bars--tag "indent-bars%s-%d" style i) do
      (face-spec-set face (indent-bars--calculate-face-spec style i))
      collect face))
    )
  :hook
  (evil-visual-state-entry
   . (lambda () (setq indent-bars-display-on-blank-lines nil)
       (indent-bars-reset)))
  (evil-visual-state-exit
   . (lambda () (setq indent-bars-display-on-blank-lines 'least)
       (indent-bars-reset)))
  )
