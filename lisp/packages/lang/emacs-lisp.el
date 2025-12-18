;;; lang/emacs-lisp.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package helpful :defer t)

(defun +lang-elisp-exec-on-save ()
  "Operations to be executed on buffer save."
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

(defun +lang-elisp-mode-setup ()
  "Setup to run for `emacs-lisp-mode` modes."
  (add-hook 'before-save-hook #'+lang-elisp-exec-on-save nil 'local)
  (util/add-capf-hooks
   #'yasnippet-capf
   #'cape-dabbrev
   #'cape-file
   #'cape-elisp-symbol
   #'cape-keyword)

  (flymake-mode -1))

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (emacs-lisp-mode . +lang-elisp-mode-setup))
