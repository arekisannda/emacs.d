;;; lang-org.el --- Org-mode/LaTeX Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-org-mode-setup ()
    "Setup to run for `org-mode` major modes."
    ;; (display-line-numbers-mode 1)
    (setq-local left-fringe-width 25)
    (global-org-modern-mode 1)
    (util/lang--add-to-capf-list (list #'cape-dabbrev
                                       #'cape-file
                                       #'cape-tex
                                       #'cape-elisp-block
                                       #'cape-keyword)))
  :hook
  (org-mode . +lang-org-mode-setup))

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-latex-setup ()
    "Setup to run for latex major modes."
    (shut-up
      (corfu-candidate-overlay-mode 1)
      (display-line-numbers-mode 1))

    (util/lang--add-to-capf-list (list #'cape-dabbrev
                                       #'cape-file
                                       #'cape-tex
                                       #'cape-keyword)))
  :hook
  (latex-mode . +lang-latex-setup)
  (TeX-mode . +lang-latex-setup)
  (LaTeX-mode . +lang-latex-setup))

(provide 'lang-org)

;;; lang-org.el ends here
