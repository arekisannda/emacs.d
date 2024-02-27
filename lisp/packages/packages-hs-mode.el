;;; packages-hs-mode.el --- Emacs Hide/Show Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs :after easy-color-faces
  :elpaca nil
  :config
  (defvar ui/code-editor--fold-overlay-string
    (concat " "
            (propertize "!" 'face `(nil :inherit easy-color-faces-green-d
                                        :weight bold
                                        :box '(:style flat)))
            (propertize "!" 'face `(nil :inherit easy-color-faces-orange-d
                                        :weight bold
                                        :box '(:style flat)))
            (propertize "!" 'face `(nil :inherit easy-color-facres-red-d
                                        :weight bold
                                        :box '(:style flat)))
            " "))

  (defun packages/hs-mode--fold-overlay (ov)
    "Format fold overlay OV."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display packages/hs-mode--fold-overlay-string)))

  (defvar packages/hs-mode--overlay-fold-function #'packages/hs-mode--fold-overlay)
  (setq hs-set-up-overlay packages/hs-mode--overlay-fold-function))

(provide 'packages-hs-mode)

;;; packages-hs-mode.el ends here
