;;; org/org-latex.el -*- lexical-binding: t; -*-

(use-package nil :after org ;; org-latex
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
  (org-latex-preview-process-default 'luadvisvgm)
  (org-latex-preview-appearance-options
   `( :foreground auto
      :background auto
      :scale nil
      :zoom 1.3
      :page-width nil
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-latex-pdf-process
   '("lualatex -shell-escape -interaction nonstopmode %f"))
  :init
  (add-to-list
   'org-latex-preview-process-alist
   '(luadvisvgm :programs ("dvilualatex" "dvisvgm")
                :description "dvi > svg"
                :message "you need to install the programs: lualatex and dvisvgm."
                :image-size-adjust (1.7 . 1.5)
                :latex-precompiler
                ("dvilualatex --output-directory=/tmp --ini --jobname=%b \"&%L\" mylatexformat.ltx %f")
                :latex-compiler
                ("dvilualatex --output-directory=/tmp --output-format=dvi --shell-escape --interaction=nonstopmode %f")
                :image-input-type "dvi"
                :image-output-type "svg"
                :image-converter
                ("dvisvgm --page=1- --clipjoin --relative --no-fonts -v3 --bbox=preview --output=%B-%%9p.svg %f")))
  :hook
  (org-mode . turn-on-org-cdlatex))
