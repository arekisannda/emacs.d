;;; packages-vterm.el --- Emacs Term Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package vterm)

(use-package multi-vterm :after vterm)

(use-package emacs :after multi-vterm
  :ensure nil
  :preface
  (defun +vterm-custom-multi-vterm ()
    "Create new vterm buffer."
    (interactive)
    (let* ((vterm-buffer (multi-vterm-get-buffer)))
      (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
      (set-buffer vterm-buffer)
      (multi-vterm-internal)
      (switch-to-buffer-other-window vterm-buffer)))
  :autoload +vterm-custom-multi-vterm)

(provide 'packages-vterm)

;;; packages-vterm.el ends here
