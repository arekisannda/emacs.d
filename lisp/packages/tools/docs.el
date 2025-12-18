;;; tools/docs.el -*- lexical-binding: t; -*-

(use-package rfc-mode
  :defer t
  :custom-face
  (rfc-mode-browser-status-face
   ((nil :inherit font-lock-string-face)))
  (rfc-mode-browser-ref-face
   ((nil :inherit font-lock-operator-face)))
  (rfc-mode-browser-title-face
   ((nil :inherit font-lock-operator-face))))

(use-package devdocs
  :defer t)

(use-package man
  :defer t
  :custom
  (Man-width-max nil))
