;;; lang-kotlin.el --- Kotlin Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun lang/kotlin-setup ()
  "Configurations for go."
  (add-hook 'go-ts-mode-hook #'lang/go--setup)

  (util/lang--set-auto-mode
   '(("\\.kt\\'" . kotlin-ts-mode))))

(lang/kotlin-setup)

(provide 'lang-kotlin)

;;; lang-kotlin.el ends here
