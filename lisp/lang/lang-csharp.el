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
  :custom
  (major-mode-remap-alist
   (append '((csharp-mode . csharp-ts-mode))
           major-mode-remap-alist)))

(provide 'lang-csharp)

;;; lang-csharp.el ends here
