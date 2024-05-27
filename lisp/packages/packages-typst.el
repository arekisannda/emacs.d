;;; packages-typst.el --- Typst Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(use-package org-typst-preview
  :ensure (:type git :host github :repo "remimimimimi/org-typst-preview" :files (:defaults "*.el"))
  :preface
  (defun +org-typst-preview-render (&optional arg)
    "Render/clear `Typst` preview in buffer.

With prefix ARG \\[universal-argument], clear preview in buffer."
    (interactive "p")
    (pcase arg
      (4 (org-typst-preview-clear-buffer))
      (_ (org-typst-preview-render-buffer)))))

(provide 'packages-typst)

;;; packages-typst.el ends here
