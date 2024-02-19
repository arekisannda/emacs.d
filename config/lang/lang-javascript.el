;;; lang-javascript.el --- Javascript Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'lang-utils)

(defun lang/javascript-setup ()
  "Configurations for javascript."

  (lang/utils--remap-major-mode
   '((javascript-mode . js-ts-mode))))

(lang/javascript-setup)

(provide 'lang-javascript)

;;; lang-javascript.el ends here
