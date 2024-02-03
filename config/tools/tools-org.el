;;; tools-org.el --- Emacs Org-mode/LaTeX Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'packages-init)
(require 'ui-fonts)

(defun tools/org--org-mode-setup ()
  "Setup to run for `org-mode` major modes."
  (mixed-pitch-mode)
  (org-bullets-mode))

(defun tools/org--latex-mode-setup ()
  "Setup to run for latex major modes."
  (setq truncate-lines nil)
  (visual-line-mode 1)
  (latex-preview-pane-mode))

(defun tools/org--setup ()
  "Set up `org-mode` configurations."
  (setq-default  org-startup-with-inline-images t
                 org-startup-indented t
                 org-hide-emphasis-markers t
                 org-ellipsis "  " ;; folding symbol
                 org-pretty-entities t
                 ;; show actually italicized text instead of /italicized text/
                 org-agenda-block-separator ""
                 org-fontify-whole-heading-line t
                 org-fontify-done-headline t
                 org-fontify-quote-and-verse-blocks t)

  (setq-default org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

  (dolist (face `((org-level-1 . ,(+ ui/fonts-variable-pitch-size (* 2 7)))
                  (org-level-2 . ,(+ ui/fonts-variable-pitch-size (* 2 6)))
                  (org-level-3 . ,(+ ui/fonts-variable-pitch-size (* 2 5)))
                  (org-level-4 . ,(+ ui/fonts-variable-pitch-size (* 2 4)))
                  (org-level-5 . ,(+ ui/fonts-variable-pitch-size (* 2 3)))
                  (org-level-6 . ,(+ ui/fonts-variable-pitch-size (* 2 2)))
                  (org-level-7 . ,(+ ui/fonts-variable-pitch-size (* 2 1)))
                  (org-level-8 . ,(+ ui/fonts-variable-pitch-size (* 2 0)))))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▪"))))))

  (add-hook 'org-mode-hook #'tools/org--org-mode-setup))

(defun tools/org--latex-setup ()
  "Set up latex configurations."
  (add-hook 'latex-mode-hook #'tools/org--latex-mode-setup))

(defun tools/org-setup ()
  "Set up org configurations."
  (tools/org--setup)
  (tools/org--latex-setup))

(tools/org-setup)

(provide 'tools-org)

;;; tools-org.el ends here
