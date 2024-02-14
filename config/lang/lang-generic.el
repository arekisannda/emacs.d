;;; lang-generic.el --- Generic Programming Language Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hideshow)
(require 'packages-init)
(require 'lang-utils)

(defun lang/generic--prog-mode-setup ()
  "Setup to run for `prog-mode` major modes."
  (setq truncate-lines t)
  (visual-line-mode -1)
  (hs-minor-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1))

(defun lang/generic--sh-setup ()
  "Setup to run for sh major modes."
  (setq sh-basic-offset 2))

(defun lang/generic--conf-setup ()
  "Setup to run for conf major modes."
  (setq truncate-lines t)
  (visual-line-mode -1)
  (hs-minor-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1))

(defun lang/generic-prog-mode-setup ()
  "Configurations for `prog-mode`."
  (add-hook 'prog-mode-hook #'lang/generic--prog-mode-setup)
  (add-hook 'bash-ts-mode #'lang/generic--sh-setup)
  (add-hook 'sh-mode #'lang/generic--sh-setup)

  (setq mode-remap-alist
        '((sh-mode . bash-ts-mode)))

  (setq mode-auto-alist
        '(("Dockerfile\\'" . dockerfile-ts-mode)
          ("\\.dockerfile\\'" . dockerfile-ts-mode)))

  (lang/utils--remap-major-mode mode-remap-alist)
  (lang/utils--set-auto-mode mode-auto-alist))

(defun lang/generic-conf-mode-setup ()
  "Configurations for `conf-mode`."
  (add-hook 'i3wm-config-mode-hook #'lang/generic--conf-setup)
  (add-hook 'conf-mode-hook #'lang/generic--conf-setup)

  (setq mode-remap-alist
        '((css-mode . css-ts-mode)
          (js-json-mode . json-ts-mode)))

  (setq mode-auto-alist
        '(("\\.ya?ml\\'" . yaml-ts-mode)
          ("\\.sway\\'" . i3wm-config-mode)
          ("\\.jsonc\\'" . json-ts-mode)))

  (lang/utils--remap-major-mode mode-remap-alist)
  (lang/utils--set-auto-mode mode-auto-alist))

(lang/generic-conf-mode-setup)
(lang/generic-prog-mode-setup)

(provide 'lang-generic)

;;; lang-generic.el ends here
