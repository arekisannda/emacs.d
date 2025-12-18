;;; org/org-crypt.el -*- lexical-binding: t; -*-

(use-package org-crypt :after org
  :custom
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  :config
  (org-crypt-use-before-save-magic))
