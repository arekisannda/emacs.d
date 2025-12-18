;;; lang/json.el -*- lexical-binding: t; -*-

(require 'util-lang)

(use-package flymake-json :defer t)

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 2)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((js-json-mode . json-ts-mode)))
  :mode
  ("\\.jsonc\\'" . json-ts-mode)
  :hook
  (json-mode . flymake-json-load))
