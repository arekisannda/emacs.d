;;; tools/gpt.el -*- lexical-binding: t; -*-

(use-package gptel
  :defer t
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-api-key-from-auth-source)
  :hook
  (gptel-mode . visual-fill-column-mode--disable))
