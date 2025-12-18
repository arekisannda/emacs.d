;;; lang/typst.el -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :custom
  (typst-ts-mode-indent-offset 2)
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t)
  :hook
  (typst-ts-mode . +lang-prog-mode-setup))
