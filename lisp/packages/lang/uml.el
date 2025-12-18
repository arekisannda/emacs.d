;;; lang/uml.el -*- lexical-binding: t; -*-

(use-package plantuml-mode)

(use-package mermaid-mode)

(use-package ob-mermaid
  :defer t
  :custom
  (ob-mermaid-cli-path (executable-find "mmdc"))
  :config
  (setq org-babel-default-header-args:mermaid
        '((:results . "file")
          (:exports . "results")
          (:theme . "dark")
          (:background-color . "transparent"))))
