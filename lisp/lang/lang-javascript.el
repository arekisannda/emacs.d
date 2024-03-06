;;; lang-javascript.el --- Javascript Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :custom
  (major-mode-remap-alist
   (append '((javascript-mode . js-ts-mode))
           major-mode-remap-alist)))

(provide 'lang-javascript)

;;; lang-javascript.el ends here
