;;; lang-generic.el --- Generic Programming Language Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hideshow)
(require 'util-lang)

(use-package markdown-mode)

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
    (rainbow-delimiters-mode 1)
    (flyspell-prog-mode)
    (valign-mode 1))

  (defun +lang-sh-setup ()
    "Setup to run for sh major modes."
    (setq-local sh-basic-offset 2)
    (valign-mode 1))
  :hook
  (prog-mode . +lang-prog-mode-setup)
  (bash-ts-mode . +lang-sh-setup)
  (sh-mode . +lang-sh-setup)
  (typst-ts-mode . +lang-prog-mode-setup)
  :custom
  (major-mode-remap-alist
   (append '((sh-mode . bash-ts-mode))
           major-mode-remap-alist)))

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
  (yaml-ts-mode . +lang-conf-setup)
  :custom
  (major-mode-remap-alist
   (append '((css-mode . css-ts-mode)
             (js-json-mode . json-ts-mode))
           major-mode-remap-alist)))

(provide 'lang-generic)

;;; lang-generic.el ends here
