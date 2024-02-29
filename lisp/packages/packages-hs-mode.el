;;; packages-hs-mode.el --- Emacs hs-mode Package Configurations-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs :after easy-color-faces
  :ensure nil
  :preface
  (defvar +hs-mode-fold-overlay-string
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

  (defun +hs-mode-fold-overlay (ov)
    "Format fold overlay OV."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display +hs-mode-fold-overlay-string)))

  (defvar +hs-mode-overlay-fold-function #'+hs-mode-fold-overlay)
  :custom
  (hs-set-up-overlay +hs-mode-overlay-fold-function))

(provide 'packages-hs-mode)

;;; packages-hs-mode.el ends here
