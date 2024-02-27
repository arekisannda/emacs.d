;;; lang-javascript.el --- Javascript Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun lang/javascript-setup ()
  "Configurations for javascript."

  (util/lang--remap-major-mode
   '((javascript-mode . js-ts-mode))))

(lang/javascript-setup)

(provide 'lang-javascript)

;;; lang-javascript.el ends here
