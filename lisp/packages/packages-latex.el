;;; packages-latex.el --- LaTeX Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defun +latex-setup ()
  "Setup to run for latex major modes."
  (shut-up
    (display-line-numbers-mode 1))

  (util/lang--add-to-capf-list (list #'cape-dabbrev
                                     #'cape-file
                                     #'cape-tex
                                     #'cape-keyword))
  (flyspell-mode))

(use-package latex
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
  (TeX-parse-self t)
  :hook
  (latex-mode . +latex-setup)
  (TeX-mode . +latex-setup)
  (LaTeX-mode . +latex-setup))

(use-package latex-preview-pane :after latex)

(use-package latex-math-preview :after latex)

(provide 'packages-latex)

;;; packages-latex.el ends here
