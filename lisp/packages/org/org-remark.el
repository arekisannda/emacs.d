;;; org/org-remark.el -*- lexical-binding: t; -*-

(use-package org-remark :after org
  :custom
  (org-remark-create-default-pen-set nil)
  (org-remark-notes-file-name ".remarks.org")
  (org-remark-notes-auto-delete :auto-delete)
  (org-remark-notes-buffer-name "*remark-notes*")
  (org-remark-notes-display-buffer-action '())
  :custom-face
  (org-remark-highlighter
   ((nil :inherit default
         :weight bold)))
  (org-remark-highlighter-warning
   ((nil :inherit default
         :weight bold
         :foreground ,(doom-darken (doom-color 'yellow) 0.2))))
  :config
  (org-remark-global-tracking-mode +1)
  (org-remark-line-mode +1)

  (defmacro +org-remark-height-face (height)
    `(list t :height ,height))

  (defmacro +org-remark-event-face (color height)
    `(list t :foreground unspecified :height ,height))

  (defmacro +org-remark-highlight-face (color)
    `(list t :foreground unspecified :inverse-video t))

  (defmacro +org-remark-color-face (color)
    `(list t :foreground unspecified))

  (org-remark-create "size-025" (+org-remark-height-face 0.25))
  (org-remark-create "size-050" (+org-remark-height-face 0.50))
  (org-remark-create "size-075" (+org-remark-height-face 0.75))
  (org-remark-create "size-125" (+org-remark-height-face 1.25))
  (org-remark-create "size-150" (+org-remark-height-face 1.50))
  (org-remark-create "size-175" (+org-remark-height-face 1.75))
  (org-remark-create "size-200" (+org-remark-height-face 2.00))

  (org-remark-create "warn"
                     (+org-remark-event-face
                      (doom-darken (doom-color 'yellow) 0.1)
                      1.25))
  (org-remark-create "error"
                     (+org-remark-event-face
                      (doom-darken (doom-color 'red) 0.1)
                      1.25))

  (org-remark-create "hl-yellow"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'yellow) 0.1)))
  (org-remark-create "hl-orange"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'orange) 0.1)))
  (org-remark-create "hl-red"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'red) 0.1)))
  (org-remark-create "hl-magenta"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'magenta) 0.1)))
  (org-remark-create "hl-blue"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'dark-blue) 0.1)))
  (org-remark-create "hl-green"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'green) 0.1)))
  (org-remark-create "hl-cyan"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'blue) 0.1)))
  (org-remark-create "hl-violet"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'violet) 0.3)))
  (org-remark-create "hl-purple"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'violet) 0.1)))
  (org-remark-create "hl-gray"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'fg-alt) 0.1)))

  (org-remark-create "yellow"
                     (+org-remark-color-face
                      (doom-color 'yellow)))
  (org-remark-create "orange"
                     (+org-remark-color-face
                      (doom-color 'orange)))
  (org-remark-create "red"
                     (+org-remark-color-face
                      (doom-color 'red)))
  (org-remark-create "magenta"
                     (+org-remark-color-face
                      (doom-color 'magenta)))
  (org-remark-create "blue"
                     (+org-remark-color-face
                      (doom-lighten (doom-color 'dark-blue) 0.1)))
  (org-remark-create "green"
                     (+org-remark-color-face
                      (doom-color 'green)))
  (org-remark-create "cyan"
                     (+org-remark-color-face
                      (doom-color 'cyan)))
  (org-remark-create "violet"
                     (+org-remark-color-face
                      (doom-lighten (doom-color 'violet) 0.3)))
  (org-remark-create "purple"
                     (+org-remark-color-face
                      (doom-color 'violet)))
  (org-remark-create "gray"
                     (+org-remark-color-face
                      (doom-color 'fg-alt)))
  )
