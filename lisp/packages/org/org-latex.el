;;; org/org-latex.el -*- lexical-binding: t; -*-

(use-package emacs :after org ;; org-latex
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

  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc"
                                         "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf"
                                         "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf"
                                         "acn" "acr" "alg" "glg" "gls" "ist" "ltjruby")))
  (org-latex-hyperref-template nil)
  (org-highlight-latex-and-related '(native script entities))
  (org-startup-with-latex-preview t)
  (org-export-with-latex 'luadvisvgm)
  (org-html-with-latex 'luadvisvgm)
  (org-latex-preview-live '(inline block edit-special))
  (org-preview-latex-default-process 'luadvisvgm)
  (org-latex-preview-appearance-options
   `( :foreground auto
      :background auto
      :scale nil
      :zoom 1.0
      :page-width nil
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-latex-pdf-process
   '("lualatex -shell-escape -interaction nonstopmode %f"))
  :init
  (add-to-list
   'org-preview-latex-process-alist
   '(luadvisvgm :programs ("dvilualatex" "dvisvgm")
                :description "dvi > svg"
                :message "you need to install the programs: dvilualatex and dvisvgm."
                :image-input-type "dvi"
                :image-output-type "svg"
                :image-size-adjust (1.0 . 1.0)
                :latex-compiler
                ("cd %o && dvilualatex -interaction=nonstopmode -shell-escape -output-directory=%o %f")
                :image-converter
                ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
   )
  :hook
  (org-mode . turn-on-org-cdlatex))
