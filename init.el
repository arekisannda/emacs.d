;; init.el --- Emacs Configuration Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

;; (profiler-start 'cpu+mem)

(defun +recursive-load-path (path)
  "Recursively load subdirectories in PATH."
  (let* ((path (expand-file-name path user-emacs-directory))
         (local-pkgs (mapcar 'file-name-directory
                             (directory-files-recursively path"\\.el$"))))
    (if (file-accessible-directory-p path)
        (mapc (apply-partially 'add-to-list 'load-path) local-pkgs))))

(dolist (path '("lisp"))
  (+recursive-load-path path))

(require 'packages-manager)
(require 'packages-base)
(require 'packages-treesit)
(require 'packages-hs-mode)
(require 'packages-themes)
(require 'packages-interface)
(require 'packages-treemacs)
(require 'packages-evil)
(require 'packages-vterm)
(require 'packages-perspective)
(require 'packages-project)
(require 'packages-windows)
(require 'packages-layout)
(require 'packages-corfu)
(require 'packages-vertico)

(elpaca-wait)

(require 'lang-generic)
(require 'lang-elisp)
(require 'lang-org)
(require 'lang-clang)
(require 'lang-csharp)
(require 'lang-go)
(require 'lang-kotlin)
(require 'lang-lua)
(require 'lang-python)
(require 'lang-rust)
(require 'lang-whitespace)

(elpaca-wait)

(require 'packages-vc)
(require 'packages-flycheck)
(require 'packages-eglot)
(require 'packages-dape)
(require 'packages-yasnippet)
(require 'packages-latex)
(require 'packages-org-mode)
(require 'packages-japanese)
(require 'packages-ispell)
(require 'packages-emacs)

(provide 'config/init)

;;; init.el ends here
