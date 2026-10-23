;;; org/org-latex.el -*- lexical-binding: t; -*-

(use-package org ;; org-latex
  :ensure nil
  :custom
  (org-latex-compiler "lualatex")
  (org-latex-packages-alist
   '(("" "bbm" t)
     ("" "amsmath" t)
     ("" "amssymb" t)
     ("" "graphicx" t)
     ("" "hyperref" t)
     ("" "multirow" t)
     ("" "makecell" t)
     ("" "tikz" t)
     ("" "pgfplots" t)))

  (org-latex-logfiles-extensions
   '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc"
     "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf"
     "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf"
     "acn" "acr" "alg" "glg" "gls" "ist" "ltjruby"))

  (org-latex-hyperref-template nil)
  (org-highlight-latex-and-related '(native script entities))
  (org-startup-with-latex-preview nil)
  (org-export-with-latex 'dvisvgm)
  (org-html-with-latex 'dvisvgm)
  (org-latex-preview-process-default 'dvisvgm)
  (org-latex-preview-mode-display-live '(inline block edit-special))
  (org-latex-preview-appearance-options
   '( :foreground "White"
      :background auto
      :scale nil
      :zoom 1.5
      :page-width nil
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-latex-pdf-process
   '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  :config
  (let ((alist (copy-tree org-latex-preview-process-alist)))
    (setf (plist-get (alist-get 'dvisvgm alist) :image-size-adjust)
          '(1.7 . 1.5))
    (setq org-latex-preview-process-alist alist))
  :hook
  (org-mode . turn-on-org-cdlatex))
