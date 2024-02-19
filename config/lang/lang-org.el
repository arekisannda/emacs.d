;;; lang-org.el --- Org-mode/LaTeX Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'packages-init)
(require 'ui-fonts)
(require 'lang-utils)

(defun lang/org--org-mode-setup ()
  "Setup to run for `org-mode` major modes."
  (mixed-pitch-mode)
  (org-bullets-mode)
  (display-line-numbers-mode 1)

  (lang/utils--add-to-capf-list (list #'cape-dabbrev
                                      #'cape-file
                                      #'cape-tex
                                      #'cape-elisp-block
                                      #'cape-keyword))
  )

(defun lang/org--latex-mode-setup ()
  "Setup to run for latex major modes."
  ;; (latex-preview-pane-mode)
  (shut-up
    (corfu-candidate-overlay-mode 1)
    (display-line-numbers-mode 1))

  (lang/utils--add-to-capf-list (list #'cape-dabbrev
                                      #'cape-file
                                      #'cape-tex
                                      #'cape-keyword)))

(defun lang/org--setup ()
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

  (add-hook 'org-mode-hook #'lang/org--org-mode-setup))

(defun lang/org--latex-setup ()
  "Set up latex configurations."
  (add-hook 'latex-mode-hook #'lang/org--latex-mode-setup)
  (add-hook 'TeX-mode-hook #'lang/org--latex-mode-setup)
  (add-hook 'LaTeX-mode-hook #'lang/org--latex-mode-setup)
  )

(defun lang/org-setup ()
  "Set up org configurations."
  (lang/org--setup)
  (lang/org--latex-setup))

(lang/org-setup)

(provide 'lang-org)

;;; lang-org.el ends here
