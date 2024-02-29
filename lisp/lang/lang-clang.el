;;; lang-clang.el --- C/C++ Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-clang-setup ()
    "Setup to run for C/C++ major modes."
    (lsp-deferred))
  :hook
  (c++-ts-mode . +lang-clang-setup)
  (c-ts-mode . +lang-clang-setup)
  (c-or-c++-ts-mode. +lang-clang-setup)
  :config
  (util/lang--remap-major-mode
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode))))

(provide 'lang-clang)

;;; lang-clang.el ends here
