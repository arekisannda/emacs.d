;;; org.el --- Emacs org-mode configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package org
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

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  :init
  (setq mixed-pitch-set-height 80)
  )

(provide 'packages-org)

;;; org.el ends here
