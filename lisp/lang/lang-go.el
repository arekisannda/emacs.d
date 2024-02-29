;;; lang-go.el --- Golang Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-go-setup ()
    (lsp-deferred)
    (setq lsp-go-analyses '((shadow . t)
                            (simplifycompositelit . :json-false)))

    (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
    (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))
  :mode
  ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . +lang-go-setup))

(provide 'lang-go)

;;; lang-go.el ends here
