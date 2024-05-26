;;; packages-org-mode.el --- Org-mode Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-org-mode)

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 100))

(use-package valign
  :custom
  (valign-fancy-bar t))

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
  (org-cycle-level-faces nil)

  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ... ") ;; folding symbol
  (org-use-sub-superscripts '{})

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

  (org-agenda-files '("~/agenda/date"
                      "~/agenda/project"
                      "~/agenda/work"))

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
  (org-modern-table nil)
  (org-modern-star "○")
  (org-modern-replace-stars "○")
  (org-modern-internal-target '(" ↪ " t " "))
  (org-modern-radio-target '("  " t " "))
  (org-modern-progress '("󰝦" "󰪞" "󰪟" "󰪠" "󰪡" "󰪢" "󰪣" "󰪤" "󰪥"))
  (org-modern-checkbox '((?X . "󰄳") (?- . "󰝥") (?\s . "󰝦")))
  :config
  (global-org-modern-mode 1))

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

(use-package org-contrib :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package org-super-agenda :after org
  :custom
  (org-agenda-custom-commands
   '(("n" "Next View"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :todo "TODAY"
                             :scheduled today
                             :order 0)
                      (:habit t)
                      (:name "Due Today"
                             :deadline today
                             :order 2)
                      (:name "Due Soon"
                             :deadline future
                             :order 8)
                      (:name "Overdue"
                             :deadline past
                             :order 7)
                      ))))
       (todo "" ((org-agenda-overriding-header "")
                 (org-super-agenda-groups
                  '((:name "Inbox"
                           :file-path "inbox"
                           :order 0
                           )
                    (:discard (:todo "TODO"))
                    (:auto-category t
                                    :order 9)
                    ))))))
     ("t" "Todo View"
      (
       (todo "" ((org-agenda-overriding-header "")
                 (org-super-agenda-groups
                  '((:name "Inbox"
                           :file-path "inbox"
                           :order 0
                           )
                    (:auto-category t
                                    :order 9)
                    ))))))
     ))

  :hook
  (elpaca-after-init . org-super-agenda-mode))

(use-package org-remark :after org
  :preface
  (defun +org-remar-configure-fonts ()
    (set-face-attribute 'org-remark-highlighter nil
                        :inherit 'default
                        :weight 'bold)
    (set-face-attribute 'org-remark-highlighter-warning nil
                        :inherit 'easy-color-faces-yellow-d
                        :weight 'bold))
  :custom
  (org-remark-create-default-pen-set nil)
  (org-remark-notes-file-name ".remarks.org")
  (org-remark-notes-auto-delete :auto-delete)
  (org-remark-notes-buffer-name "*remark-notes*")
  (org-remark-notes-display-buffer-action '())
  :init
  (util/if-daemon-run-after-make-frame-else-add-hook
   (+org-remar-configure-fonts)
   'window-setup-hook)
  :config
  (org-remark-global-tracking-mode +1)
  (org-remark-line-mode +1)

  (org-remark-create "size-025" (util/org-remark-height-face 0.25))
  (org-remark-create "size-050" (util/org-remark-height-face 0.50))
  (org-remark-create "size-075" (util/org-remark-height-face 0.75))
  (org-remark-create "size-125" (util/org-remark-height-face 1.25))
  (org-remark-create "size-150" (util/org-remark-height-face 1.50))
  (org-remark-create "size-175" (util/org-remark-height-face 1.75))
  (org-remark-create "size-200" (util/org-remark-height-face 2.00))

  (org-remark-create "warn"
                     (util/org-remark-event-face
                      'easy-color-faces-yellow-l
                      1.25))
  (org-remark-create "error"
                     (util/org-remark-event-face
                      'easy-color-faces-red-d
                      1.25))

  (org-remark-create "hl-yellow"
                     (util/org-remark-highlight-face
                      'easy-color-faces-yellow-l))
  (org-remark-create "hl-orange"
                     (util/org-remark-highlight-face
                      'easy-color-faces-orange-l))
  (org-remark-create "hl-red"
                     (util/org-remark-highlight-face
                      'easy-color-faces-red-l))
  (org-remark-create "hl-magenta"
                     (util/org-remark-highlight-face
                      'easy-color-faces-magenta-l))
  (org-remark-create "hl-blue"
                     (util/org-remark-highlight-face
                      'easy-color-faces-blue-l))
  (org-remark-create "hl-green"
                     (util/org-remark-highlight-face
                      'easy-color-faces-green-l))
  (org-remark-create "hl-cyan"
                     (util/org-remark-highlight-face
                      'easy-color-faces-cyan-l))
  (org-remark-create "hl-violet"
                     (util/org-remark-highlight-face
                      'easy-color-faces-violet-l))
  (org-remark-create "hl-purple"
                     (util/org-remark-highlight-face
                      'easy-color-faces-purple-l))
  (org-remark-create "hl-gray"
                     (util/org-remark-highlight-face
                      'easy-color-faces-gray-l))

  (org-remark-create "yellow"
                     (util/org-remark-color-face
                      'easy-color-faces-yellow))
  (org-remark-create "orange"
                     (util/org-remark-color-face
                      'easy-color-faces-orange))
  (org-remark-create "red"
                     (util/org-remark-color-face
                      'easy-color-faces-red))
  (org-remark-create "magenta"
                     (util/org-remark-color-face
                      'easy-color-faces-magenta))
  (org-remark-create "blue"
                     (util/org-remark-color-face
                      'easy-color-faces-blue-l))
  (org-remark-create "green"
                     (util/org-remark-color-face
                      'easy-color-faces-green))
  (org-remark-create "cyan"
                     (util/org-remark-color-face
                      'easy-color-faces-cyan))
  (org-remark-create "violet"
                     (util/org-remark-color-face
                      'easy-color-faces-violet))
  (org-remark-create "purple"
                     (util/org-remark-color-face
                      'easy-color-faces-purple))
  (org-remark-create "gray"
                     (util/org-remark-color-face
                      'easy-color-faces-gray)))

(provide 'packages-org-mode)

;;; packages-org-mode.el ends here
