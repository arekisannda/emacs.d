;;; lang-javascript.el --- Javascript Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :config
  (util/lang--remap-major-mode
   '((javascript-mode . js-ts-mode))))

(provide 'lang-javascript)

;;; lang-javascript.el ends here
