;;; modes.el --- Emacs mode packages and configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package i3wm-config-mode
  :hook
  (i3wm-config-mode . display-line-numbers-mode))

(elpaca-wait)

(provide 'packages-modes)

;;; modes.el ends here
