;;; packages-org-mode.el --- Org-mode Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package org
  :ensure nil
  :custom
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-startup-indented t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ... ") ;; folding symbol

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  (org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f"
                           "bibtex %b"
                           "pdflatex -interaction nonstopmode -output-directory %o %f"
                           "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc"
                                         "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf"
                                         "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf"
                                         "acn" "acr" "alg" "glg" "gls" "ist")))
  (org-latex-hyperref-template nil)

  :config

  (dolist (face `((org-level-1 . 1.30)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.20)
                  (org-level-4 . 1.16)
                  (org-level-5 . 1.12)
                  (org-level-6 . 1.08)
                  (org-level-7 . 1.04)
                  (org-level-8 . 1.00)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))

(use-package ob-go)

(use-package ob-rust)

(use-package ob-typescript)

(use-package ob-kotlin)

(use-package gnuplot)

(use-package emacs :after (ob-go ob-rust ob-kotlin ob-typescript gnuplot)
  :ensure nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages '((C . t)
                              (awk . t)
                              (calc . t)
                              (emacs-lisp . t)
                              (gnuplot . t)
                              (go . t)
                              (js . t)
                              (kotlin . t)
                              (plantuml . t)
                              (python . t)
                              (rust . t)
                              (shell . t)
                              (sql . t)
                              (sqlite . t)
                              (typescript . t))))

(use-package org-modern :after org
  :custom
  (org-modern-internal-target '(" ↪ " t " "))
  (org-modern-radio-target '("  " t " "))
  (org-modern-progress '("󰝦" "󰪞" "󰪟" "󰪠" "󰪡" "󰪢" "󰪣" "󰪤" "󰪥"))
  (org-modern-checkbox '((?X . "󰄳") (?- . "󰝥") (?\s . "󰝦"))))

(use-package emacs :after (sonokai-theme org org-modern easy-color-faces)
  :ensure nil
  :preface
  (defun +themes-configure-org-fonts ()
    (set-face-attribute 'org-checkbox nil
                        :height +fonts-fixed-pitch-size
                        :box nil)
    (set-face-attribute 'org-modern-label nil
                        :height +fonts-fixed-pitch-size
                        :box '(:line-width 5 :style flat-button))
    )

  :init
  (util/if-daemon-run-after-make-frame-else-add-hook
   (+themes-configure-org-fonts)
   'window-setup-hook))

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(provide 'packages-org-mode)

;;; packages-org-mode.el ends here
