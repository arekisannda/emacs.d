;;; lang-typescript.el --- typescript configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'lang-utils)

(defun lang/typescript-setup ()
  "Configurations for typescript."

  (lang/utils--set-auto-mode
   '(("\\.tsx\\'" . tsx-ts-mode)
     ("\\.ts\\'" . typescript-ts-mode))))

(lang/typescript-setup)

(provide 'lang-typescript)

;;; lang-typescript.el ends here
