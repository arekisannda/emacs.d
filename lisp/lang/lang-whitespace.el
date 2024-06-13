;;; lang-whitespace.el --- Language Whitespace Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (python-indent-offset 4)
  (go-ts-mode-indent-offset 4)
  (json-ts-mode-indent-offset 2)
  (go-ts-mode-indent-offset 2)
  (yaml-indent-offset 2)
  (typst-ts-mode-indent-offset 2)
  (sh-basic-offset 2))

(provide 'lang-whitespace)

;;; lang-whitespace.el ends here
