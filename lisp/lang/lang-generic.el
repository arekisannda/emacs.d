;;; lang-generic.el --- Generic Programming Language Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hideshow)
(require 'util-lang)

(use-package i3wm-config-mode
  :preface
  (defun +lang-i3wm-setup ()
    "Setup to run for conf major modes."
    (setq truncate-lines t)
    (visual-line-mode -1)
    (display-line-numbers-mode 1)
    (rainbow-delimiters-mode 1))
  :mode
  ("\\.sway\\'" . i3wm-config-mode)
  ("\\.i3\\'" . i3wm-config-mode)
  :hook
  (i3wm-config-mode . +lang-i3wm-setup))

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-prog-mode-setup ()
    "Setup to run for `prog-mode` major modes."
    (setq truncate-lines t)
    (visual-line-mode -1)
    (display-line-numbers-mode 1)
    (rainbow-delimiters-mode 1))

  (defun +lang-sh-setup ()
    "Setup to run for sh major modes."
    (setq sh-basic-offset 2))
  :hook
  (prog-mode . +lang-prog-mode-setup)
  (bash-ts-mode . +lang-sh-setup)
  (sh-mode . +lang-sh-setup)
  :config
  (util/lang--remap-major-mode
   '((sh-mode . bash-ts-mode))))

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-conf-setup ()
    "Setup to run for conf major modes."
    (setq truncate-lines t)
    (visual-line-mode -1)
    (display-line-numbers-mode 1)
    (rainbow-delimiters-mode 1))
  :mode
  ("Dockerfile\\'" . dockerfile-ts-mode)
  ("\\.dockerfile\\'" . dockerfile-ts-mode)
  ("\\.ya?ml\\'" . yaml-ts-mode)
  ("\\.jsonc\\'" . json-ts-mode)
  :hook
  (conf-mode . +lang-conf-setup)
  :config
  (util/lang--remap-major-mode
   '((css-mode . css-ts-mode)
     (js-json-mode . json-ts-mode))))

(provide 'lang-generic)

;;; lang-generic.el ends here
