;;; lang-org.el --- Org-mode/LaTeX Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-org-mode-font ()
    (let ((font-family (org-entry-get (point-min) "font-family" t))
          (font-height (org-entry-get (point-min) "font-height" t)))
      (when font-family
        (setq-local buffer-face-mode-face `(:family ,font-family)))
      (when font-height
        (setq-local buffer-face-mode-face `(:height ,(string-to-number font-height))))
      (buffer-face-mode)))

  (defun +lang-org-mode-setup ()
    "Setup to run for `org-mode` major modes."
    ;; (display-line-numbers-mode 1)
    (setq-local left-fringe-width 25)
    (setq-local right-fringe-width 25)
    (global-org-modern-mode 1)
    (util/lang--add-to-capf-list (list #'cape-dabbrev
                                       #'cape-file
                                       #'cape-tex
                                       #'cape-elisp-block
                                       #'cape-keyword))
    (flyspell-mode)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (valign-mode t)
    (+lang-org-mode-font))
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
                                       #'cape-keyword))
    (flyspell-mode))
  :hook
  (latex-mode . +lang-latex-setup)
  (TeX-mode . +lang-latex-setup)
  (LaTeX-mode . +lang-latex-setup))

(provide 'lang-org)

;;; lang-org.el ends here
