;;; completion/affe.el -*- lexical-binding: t; -*-

(use-package affe
  :custom
  (affe-find-command "rg --color=never --no-ignore --files --hidden --glob=!**/.git/*")
  (affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize
   affe-grep
   :preview-key nil))
