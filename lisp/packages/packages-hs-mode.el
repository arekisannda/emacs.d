;;; packages-hs-mode.el --- Emacs hs-mode Package Configurations-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs :after (easy-color-faces treesit-fold)
  :ensure nil
  :preface
  (setq +fold-replacement "  ")

  (defun +hs-mode-fold-overlay (ov)
    "Format fold overlay OV."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (propertize +fold-replacement
                               'face
                               `((nil :foreground ,easy-color-gray
                                      :box nil
                                      :weight bold))))))

  (defvar +hs-mode-overlay-fold-function #'+hs-mode-fold-overlay)
  :custom
  (treesit-fold-replacement +fold-replacement)
  (hs-set-up-overlay +hs-mode-overlay-fold-function)
  :custom-face
  (treesit-fold-replacement-face ((nil :foreground ,easy-color-gray
                                       :box nil
                                       :weight bold))))

(provide 'packages-hs-mode)

;;; packages-hs-mode.el ends here
