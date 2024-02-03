;;; lang-typescript.el --- Typescript Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'lang-utils)

(defun lang/typescript-setup ()
  "Configurations for typescript."
  (setq mode-auto-alist
        '(("\\.tsx\\'" . tsx-ts-mode)
          ("\\.ts\\'" . typescript-ts-mode)))

  (lang/utils--set-auto-mode mode-auto-alist))

(lang/typescript-setup)

(provide 'lang-typescript)

;;; lang-typescript.el ends here
