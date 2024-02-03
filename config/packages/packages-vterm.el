;;; packages-vterm.el --- Emacs Term Shell Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

(use-package vterm :ensure t)

(use-package multi-vterm :ensure t :after vterm)

(elpaca-wait)

(defun packages/vterm--multi-vterm ()
  "Create new vterm buffer."
  (interactive)
  (let* ((vterm-buffer (multi-vterm-get-buffer)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (multi-vterm-internal)
    (switch-to-buffer-other-window vterm-buffer)))

(provide 'packages-vterm)

;;; packages-vterm.el ends here
