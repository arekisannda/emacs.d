;;; lang/markdown.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package markdown-mode
  :custom-face
  (markdown-code-face
   ((nil :background unspecified :inherit tooltip)))
  :config
  (util/update-alist
   'markdown-code-lang-modes
   '(("go"         . go-mode)
     ("rust"       . rust-mode)
     ("diff"       . diff-mode)
     ("python"     . python-mode)
     ("javascript" . javascript-mode)
     ("typescript" . typescript-mode)
     ("kotlin"     . kotlin-mode))))
