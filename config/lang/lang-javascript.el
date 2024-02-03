;;; lang-javascript.el --- Javascript Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'lang-utils)

(defun lang/javascript-setup ()
  "Configurations for javascript."

  (setq mode-remap-alist
        '((javascript-mode . js-ts-mode)))

  (lang/utils--remap-major-mode mode-remap-alist))

(lang/javascript-setup)

(provide 'lang-javascript)

;;; lang-javascript.el ends here
