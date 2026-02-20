;;; tools/w3m.el -*- lexical-binding: t; -*-

(use-package w3m
  :custom
  (w3m-search-default-engine "duckduckgo")
  (w3m-default-display-inline-images t)
  (w3m-display-mode 'plain)
  (w3m-home-page "about:blank")
  (w3m-use-header-line t)
  (w3m-use-header-line-title t)
  :custom-face
  (w3m-anchor
   ((nil :foreground ,(doom-lighten (doom-color 'dark-blue) 0.0))))
  (w3m-arrived-anchor
   ((nil :foreground ,(doom-darken (doom-color 'dark-blue) 0.3))))
  (w3m-insert
   ((nil :foreground ,(doom-color 'violet))))
  (w3m-error
   ((nil :foreground ,(doom-color 'error))))
  (w3m-form
   ((nil :foreground ,(doom-color 'magenta))))
  (w3m-form-button
   ((nil :foreground ,(doom-color 'yellow))))
  (w3m-form-button-mouse
   ((nil :foreground ,(doom-color 'yellow))))
  (w3m-form-button-pressed
   ((nil :foreground ,(doom-color 'orange))))
  (w3m-header-line-background
   ((nil :background ,(doom-color 'bg-alt))))
  (w3m-header-line-content
   ((nil :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg-alt))))
  (w3m-header-line-title
   ((nil :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg-alt))))
  (w3m-image
   ((nil :foreground ,(doom-color 'green))))
  (w3m-image-anchor
   ((nil :foreground ,(doom-color 'green)
         :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.1))))
  (w3m-unsafe-url-warning
   ((nil :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'red))))
  :hook
  (w3m-mode . visual-line-mode)
  (w3m-mode . word-wrap-whitespace-mode))
