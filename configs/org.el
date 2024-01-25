;;; org.el --- Emacs org-mode configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

;;; org-mode {{{
(use-package org
  :defer t
  :elpaca nil
  :hook
  (org-mode . visual-line-mode)
  :init
  (setq org-startup-with-inline-images t
        org-startup-indented t
        org-hide-emphasis-markers t
        org-ellipsis "  " ;; folding symbol
        org-pretty-entities t
        ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  :config
  (dolist (face '((org-level-1 . 1.30)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.20)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.10)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.00)
                  (org-level-8 . 0.90)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▪")))))))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
;;; }}}

;;; latex {{{
(use-package latex
  :demand t
  :elpaca (auctex
           :version (lambda (_) (require 'tex-site) AUCTeX-version)
           :files ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
           :pre-build (("./autogen.sh")
                       ("./configure"
                        "--with-texmf-dir=$EMACS_USER_DIRECTORY/var")
                       ("make")))
  :hook
  (latex-mode . visual-line-mode)
  :custom
  (TeX-engine 'xetex)
  (TeX-electric-math (cons "$" "$"))
  (TeX-master nil)
  (TeX-save-query nil)
  (TeX-auto-save nil)
  (TeX-parse-self t))

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))

(use-package latex-math-preview)
;;; }}}

(provide 'configs-org)
;;; org.el ends here
