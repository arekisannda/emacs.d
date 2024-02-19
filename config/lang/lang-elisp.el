;;; lang-elisp.el --- Emacs Lisp Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun lang/elisp--setup ()
  "Setup to run for `emacs-lisp-mode` major-modes."
  (shut-up
    (corfu-candidate-overlay-mode 1))

  (lang/utils--add-to-capf-list (list #'cape-dabbrev
                                      #'cape-file
                                      #'cape-elisp-symbol
                                      #'cape-keyword)))

(defun lang/elisp-setup ()
  "Set up `emacs-lisp-mode` configurations."
  (add-hook 'emacs-lisp-mode-hook #'lang/elisp--setup))

(lang/elisp-setup)

(provide 'lang-elisp)

;;; lang-elisp.el ends here
