;;; lang-clang.el --- C/C++ Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'lang-utils)

(defun lang/clang--setup ()
  "Setup to run for C/C++ major modes."
  (lsp-deferred))

(defun lang/clang-setup ()
  "Configurations for c and c++."
  (add-hook 'c++-ts-mode-hook #'lang/clang--setup)
  (add-hook 'c-ts-mode-hook #'lang/clang--setup)
  (add-hook 'c-or-c++-ts-mode-hook #'lang/clang--setup)

  (lang/utils--remap-major-mode
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode))))

(lang/clang-setup)

(provide 'lang-clang)

;;; lang-clang.el ends here
