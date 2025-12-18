;;; lang/latex.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defun +latex-setup ()
  "Setup to run for latex major modes."
  (display-line-numbers-mode 1)

  (util/add-capf-hooks
   #'cape-dabbrev
   #'cape-file
   #'cape-tex
   #'cape-keyword)

  (flyspell-mode))

(use-package latex
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

(use-package cdlatex
  :custom
  (cdlatex-use-dollar-to-ensure-math nil)
  (cdlatex-math-modify-alist
   '(( ?l "\\mathbb" "\\textbf" t nil nil )))
  (cdlatex-env-alist
   '(("tikzpicture"
      "\\begin{tikzpicture}
?
\\end{tikzpicture}"
      nil)
     ("axis"
      "\\begin{axis}
?
\\end{axis}"
      nil)
     )))

(use-package lazytab)
