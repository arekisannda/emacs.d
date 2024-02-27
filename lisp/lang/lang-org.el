;;; lang-org.el --- Org-mode/LaTeX Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'util-lang)

(defun lang/org--org-mode-setup ()
  "Setup to run for `org-mode` major modes."
  (display-line-numbers-mode 1)
  (global-org-modern-mode 1)
  (util/lang--add-to-capf-list (list #'cape-dabbrev
                                     #'cape-file
                                     #'cape-tex
                                     #'cape-elisp-block
                                     #'cape-keyword)))

(defun lang/org--latex-mode-setup ()
  "Setup to run for latex major modes."
  (shut-up
    (corfu-candidate-overlay-mode 1)
    (display-line-numbers-mode 1))

  (util/lang--add-to-capf-list (list #'cape-dabbrev
                                     #'cape-file
                                     #'cape-tex
                                     #'cape-keyword)))

(defun lang/org-setup ()
  "Set up org configurations."
  (add-hook 'org-mode-hook #'lang/org--org-mode-setup)

  (add-hook 'latex-mode-hook #'lang/org--latex-mode-setup)
  (add-hook 'TeX-mode-hook #'lang/org--latex-mode-setup)
  (add-hook 'LaTeX-mode-hook #'lang/org--latex-mode-setup))

(lang/org-setup)

(provide 'lang-org)

;;; lang-org.el ends here
