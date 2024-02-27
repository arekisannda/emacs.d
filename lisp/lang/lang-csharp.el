;;; lang-csharp.el --- C# Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun lang/csharp--setup ()
  "Setup to run for csharp major modes."
  (lsp-deferred))

(defun lang/csharp-setup ()
  "Configurations for csharp."
  (add-hook 'csharp-mode-hook #'lang/csharp--setup)
  (add-hook 'csharp-ts-mode-hook #'lang/csharp--setup)

  (util/lang--remap-major-mode
   '((csharp-mode . csharp-ts-mode))))

(lang/csharp-setup)

(provide 'lang-csharp)

;;; lang-csharp.el ends here
