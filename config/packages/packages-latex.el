;;; packages-latex.el --- LaTeX Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

(use-package latex
  :demand t
  :elpaca
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

(use-package latex-preview-pane :ensure t :after latex)

(use-package latex-math-preview :ensure t :after latex)

(provide 'packages-latex)

;;; packages-latex.el ends here
