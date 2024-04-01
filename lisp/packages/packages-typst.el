;;; packages-typst.el --- Typst Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(provide 'packages-typst)

;;; packages-typst.el ends here
