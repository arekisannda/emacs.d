;;; lang-go.el --- Golang Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun lang/go--setup ()
  "Setup to run for go major modes."
  (lsp-deferred)
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)))

  (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
  (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))

(defun lang/go-setup ()
  "Configurations for go."
  (add-hook 'go-ts-mode-hook #'lang/go--setup)

  (util/lang--set-auto-mode
   '(("\\.go\\'" . go-ts-mode))))

(lang/go-setup)

(provide 'lang-go)

;;; lang-go.el ends here
