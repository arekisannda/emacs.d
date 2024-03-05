;;; lang-csharp.el --- C# Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-csharp-setup ()
    "Setup to run for csharp major modes."
    (eglot-ensure))
  :hook
  (csharp-ts-mode . +lang-csharp-setup)
  :config
  (util/lang--remap-major-mode
   '((csharp-mode . csharp-ts-mode))))

(provide 'lang-csharp)

;;; lang-csharp.el ends here
