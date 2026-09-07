;;; tools/docs.el -*- lexical-binding: t; -*-

(use-package rfc-mode
  :defer t
  :config
  (utils/custom-set-faces
   (rfc-mode-browser-status-face
    ((nil :inherit font-lock-string-face)))
   (rfc-mode-browser-ref-face
    ((nil :inherit font-lock-operator-face)))
   (rfc-mode-browser-title-face
    ((nil :inherit font-lock-operator-face))))
  )

(use-package devdocs
  :defer t
  :config
  (utils/custom-set-faces
   (nxml-text
    ((nil :background unspecified)))
   (devdocs-code-block
    ((nil :extend t
          :background ,(doom-color 'bg-alt))))
   ))

(use-package man
  :defer t
  :custom
  (Man-width-max 100)
  (Man-notify-method 'thrifty))
