;;; packages-latex.el --- LaTeX Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package latex :demand t
  :ensure
  (auctex :version (lambda (_) (require 'tex-site) AUCTeX-version)
          :files ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
          :pre-build (("./autogen.sh")
                      ("./configure"
                       "--with-texmf-dir=$EMACS_USER_DIRECTORY/var")
                      ("make")))
  :custom
  (TeX-engine 'xetex)
  (TeX-electric-math (cons "$" "$"))
  (TeX-master nil)
  (TeX-save-query nil)
  (TeX-auto-save nil)
  (TeX-parse-self t))

(use-package latex-preview-pane :after latex)

(use-package latex-math-preview :after latex)

(provide 'packages-latex)

;;; packages-latex.el ends here
