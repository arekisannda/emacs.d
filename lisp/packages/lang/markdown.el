;;; lang/markdown.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package markdown-mode
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  :config
  (utils/custom-set-faces
   (markdown-code-face
    ((nil :background unspecified :inherit tooltip)))
   )

  (util/update-alist
   'markdown-code-lang-modes
   '(("go"         . go-mode)
     ("rust"       . rust-mode)
     ("diff"       . diff-mode)
     ("python"     . python-mode)
     ("javascript" . javascript-mode)
     ("typescript" . typescript-mode)
     ("kotlin"     . kotlin-mode))))
