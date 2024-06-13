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

(require 'lib-window-extras)
(require 'lib-layouts)

(require 'packages-manager)
(require 'packages-base)
(require 'packages-treesit)
(require 'packages-hs-mode)
(require 'packages-themes)
(require 'packages-interface)
(require 'packages-evil)
(require 'packages-vterm)
(require 'packages-project)
(require 'packages-windows)
(require 'packages-perspective)
(require 'packages-corfu)
(require 'packages-vertico)

(require 'packages-vc)
(require 'packages-eglot)
(require 'packages-dape)
(require 'packages-yasnippet)
(require 'packages-latex)
(require 'packages-org-mode)
(require 'packages-typst)
(require 'packages-japanese)
(require 'packages-ispell)
(require 'packages-code)
(require 'packages-emacs)

(require 'lang-whitespace)
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

(elpaca-wait)

(require 'keybinds-global)
(require 'keybinds-evil)
(require 'keybinds-session)
(require 'keybinds-editor)
(require 'keybinds-search)
(require 'keybinds-completion)

(provide 'config/init)

;;; init.el ends here
