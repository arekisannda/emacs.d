;;; lang/base.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defun +lang-prog-mode-setup ()
  "Prog-mode setup."
  (setq-local truncate-lines t)

  (visual-line-mode -1)
  (completion-preview-mode 1)
  (diff-hl-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (flyspell-prog-mode)
  (indent-bars-mode 1)
  (yas-minor-mode 1)

  (cond
   ((treesit-fold-ready-p) (treesit-fold-mode 1))
   ((derived-mode-p 'emacs-lisp-mode) (hs-minor-mode 1)))

  (util/add-capf-hooks
   #'cape-file
   #'cape-keyword))

(use-package prog-mode
  :hook
  (prog-mode . +lang-prog-mode-setup))

(defun +lang-conf-mode-setup ()
  "Conf-mode setup."
  (setq-local truncate-lines t)
  (visual-line-mode -1)
  (yas-minor-mode 1))

(use-package conf-mode
  :hook
  (conf-mode . diff-hl-mode)
  (conf-mode . display-line-numbers-mode)
  (conf-mode . rainbow-delimiters-mode)
  (conf-mode . +lang-conf-mode-setup))

(defun +lang-special-mode-setup ()
  "Setup to run for `special-mode` major modes."
  (cond
   ((string-match-p "\\*eldoc.*\\*" (buffer-name))
    (visual-line-mode 1))))

(use-package special-mode
  :hook
  (special-mode . +lang-special-mode-setup))
