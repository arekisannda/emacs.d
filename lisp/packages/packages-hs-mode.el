;;; packages-hs-mode.el --- Emacs hs-mode Package Configurations-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs :after easy-color-faces
  :ensure nil
  :config
  (defvar packages/hs-mode--fold-overlay-string
    (concat " "
            (propertize "!" 'face `(nil :inherit easy-color-faces-green
                                        :weight bold
                                        :box '(:style flat-button)))
            (propertize "!" 'face `(nil :inherit easy-color-faces-orange
                                        :weight bold
                                        :box '(:style flat-button)))
            (propertize "!" 'face `(nil :inherit easy-color-faces-red
                                        :weight bold
                                        :box '(:style flat-button)))
            " "))

  (defun packages/hs-mode--fold-overlay (ov)
    "Format fold overlay OV."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display packages/hs-mode--fold-overlay-string)))

  (defvar packages/hs-mode--overlay-fold-function #'packages/hs-mode--fold-overlay)
  (setq hs-set-up-overlay packages/hs-mode--overlay-fold-function))

(provide 'packages-hs-mode)

;;; packages-hs-mode.el ends here
